--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Strings.Fixed;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with Mulog;
with Muxml.Utils;
with Mutools.Match;
with Mutools.Templates;
with Mutools.XML_Utils;

with String_Templates;

package body Spec.Features
is

   --  Write IOMMU-related policy file to specified output directory.
   procedure Write_IOMMU
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
   begin
      if Mutools.XML_Utils.Has_Feature_Enabled
        (Data => Policy,
         F    => Mutools.XML_Utils.Feature_IOMMU)
      then
         Write_IOMMU (Output_Dir => Output_Dir,
                      Policy     => Policy);
      end if;
   end Write;

   -------------------------------------------------------------------------

   procedure Write_IOMMU
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_64;
      use type Mutools.XML_Utils.IOMMU_Paging_Level;

      --  Return the lowest virtualAddress value string of the memory regions
      --  given as node list. Returns zero if node list is empty.
      function Get_Base_Addr
        (Nodes : DOM.Core.Node_List)
         return String;

      ----------------------------------------------------------------------

      function Get_Base_Addr
        (Nodes : DOM.Core.Node_List)
         return String
      is
         Result : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
         Count  : constant Natural       := DOM.Core.Nodes.Length
           (List => Nodes);
      begin
         if Count = 0 then
            return "0";
         end if;

         for I in 0 .. Count - 1 loop
            declare
               Node : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Nodes,
                    Index => I);
               virtualAddr : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Node,
                       Name => "virtualAddress"));
            begin
               if virtualAddr < Result then
                  Result := virtualAddr;
               end if;
            end;
         end loop;

         return Mutools.Utils.To_Hex (Number => Result);
      end Get_Base_Addr;

      Filename  : constant String := Output_Dir & "/skp-iommu.ads";
      Root_Addr : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Policy.Doc,
           XPath => "/system/memory/memory[@type='system_vtd_root']",
           Name  => "physicalAddress");
      IRT_Phys_Addr_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Policy.Doc,
           XPath => "/system/memory/memory[@type='kernel_vtd_ir']",
           Name  => "physicalAddress");
      IRT_Virt_Addr_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Policy.Doc,
           XPath => "/system/kernel/memory/cpu[@id='0']/memory"
           & "[@physical='vtd_ir']",
           Name  => "virtualAddress");
      IRT_Phys_Addr : Interfaces.Unsigned_64
        := (if IRT_Phys_Addr_Str'Length > 0
            then Interfaces.Unsigned_64'Value (IRT_Phys_Addr_Str) else 0);
      IRT_Virt_Addr : constant Interfaces.Unsigned_64
        := (if IRT_Virt_Addr_Str'Length > 0
            then Interfaces.Unsigned_64'Value (IRT_Virt_Addr_Str) else 0);
      IOMMUs : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data    => Policy,
           Left_XPath  => "/system/kernel/devices/device/"
           & "memory[@logical='mmio']",
           Right_XPath => "/system/platform/devices/device[capabilities/"
           & "capability/@name='iommu']",
           Match       => Mutools.Match.Is_Valid_Reference_Lparent'Access);
      IOMMU_Count : constant Natural := DOM.Core.Nodes.Length
        (List => IOMMUs.Right);
      IOMMU_PT_Levels : constant Mutools.XML_Utils.IOMMU_Paging_Level
        := Mutools.XML_Utils.Get_IOMMU_Paging_Levels (Data => Policy);
      Tmpl : Mutools.Templates.Template_Type;
   begin
      Mulog.Log (Msg => "Writing IOMMU spec to '" & Filename & "'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_iommu_ads);

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__root_table_addr__",
         Content  => (if Root_Addr'Length > 0 then Root_Addr else "0"));

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__base_addr__",
         Content  => Get_Base_Addr (Nodes => IOMMUs.Left));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__iommu_device_range__",
         Content  => "1 .." & IOMMU_Count'Img);

      --  Shifted, 4KB aligned IR table address (see Intel VT-d specification,
      --  section 10.4.29).

      IRT_Phys_Addr := IRT_Phys_Addr / 2 ** 12;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__ir_table_phys_addr__",
         Content  => Mutools.Utils.To_Hex (Number => IRT_Phys_Addr));

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__ir_table_virt_addr__",
         Content  => Mutools.Utils.To_Hex (Number => IRT_Virt_Addr));

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__cap_agaw_bit__",
         Content  => Ada.Strings.Fixed.Trim
           (Source => Positive'Image (IOMMU_PT_Levels - 1),
            Side   => Ada.Strings.Left));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Filename);
   end Write_IOMMU;

end Spec.Features;
