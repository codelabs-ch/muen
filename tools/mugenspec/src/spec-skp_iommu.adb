--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with Mutools.XML_Utils;
with Mutools.Templates;

with String_Templates;

package body Spec.Skp_IOMMU
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_64;

      --  Return the lowest virtualAddress value string of the memory regions
      --  given as node list.
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
           XPath => "/system/memory/memory[@type='system_vtd_ir']",
           Name  => "physicalAddress");
      IRT_Phys_Addr : Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value (IRT_Phys_Addr_Str);
      IOMMUs : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data    => Policy,
           Left_XPath  => "/system/kernel/devices/device/"
           & "memory[@logical='mmio']",
           Right_XPath => "/system/hardware/devices/device[capabilities/"
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
         Pattern  => "__cap_agaw_bit__",
         Content  => Ada.Strings.Fixed.Trim
           (Source => Positive'Image (IOMMU_PT_Levels - 1),
            Side   => Ada.Strings.Left));

      for I in 1 .. DOM.Core.Nodes.Length (List => IOMMUs.Right) loop
         declare
            --  Intel VT-d spec, 10.4.8.1
            IOTLB_Invalidate_Size_Bits : constant := 64;
            --  Intel VT-d spec, 10.4.14
            Fault_Recording_Size_Bits  : constant := 128;

            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => IOMMUs.Right,
                                      Index => I - 1);
            Fro_Cap : constant String
              := Muxml.Utils.Get_Element_Value
                (Doc   => Node,
                 XPath => "capabilities/capability[@name='fr_offset']");
            Fro_Cap_Val : constant Positive
              := Positive'Value (Fro_Cap);
            Iotlb_Inv_Cap : constant String
              := Muxml.Utils.Get_Element_Value
                (Doc   => Node,
                 XPath => "capabilities/capability"
                 & "[@name='iotlb_invalidate_offset']");
            Iotlb_Inv_Cap_Val : constant Positive
              := Positive'Value (Iotlb_Inv_Cap);
            Suffix : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            Larger_Offset : constant Positive
              := Positive'Max (Iotlb_Inv_Cap_Val, Fro_Cap_Val);
            Register_Size : constant Positive
              := (if Iotlb_Inv_Cap_Val > Fro_Cap_Val
                  then IOTLB_Invalidate_Size_Bits
                  else Fault_Recording_Size_Bits);
         begin
            Mutools.Templates.Replace
              (Template => Tmpl,
               Pattern  => "__cap_fr_offset_value_" & Suffix & "__",
               Content  => Fro_Cap);
            Mutools.Templates.Replace
              (Template => Tmpl,
               Pattern  => "__cap_iotlb_inv_offset_value_" & Suffix & "__",
               Content  => Iotlb_Inv_Cap);
            Mutools.Templates.Replace
              (Template => Tmpl,
               Pattern  => "__iommu_type_size_" & Suffix & "__",
               Content  => "8 *" & Larger_Offset'Img & " +"
               & Register_Size'Img);
         end;
      end loop;

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Filename);

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_iommu_adb);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__base_addr__",
         Content  => Get_Base_Addr (Nodes => IOMMUs.Left));
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-iommu.adb");
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Empty (Output_Dir : String)
   is
      Tmpl : Mutools.Templates.Template_Type;
   begin
      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_iommu_empty_ads);
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-iommu.ads");
      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_iommu_empty_adb);
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-iommu.adb");
   end Write_Empty;

end Spec.Skp_IOMMU;
