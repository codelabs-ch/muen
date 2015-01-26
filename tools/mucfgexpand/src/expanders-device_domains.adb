--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;

with Expanders.Config;
with Expanders.XML_Utils;

package body Expanders.Device_Domains
is

   -------------------------------------------------------------------------

   procedure Add_Domain_IDs (Data : in out Muxml.XML_Data_Type)
   is
      Domains : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/deviceDomains/domain");
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Domains) loop
         declare
            Domain : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Domains,
                 Index => I - 1);
            Name   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Domain,
                 Name => "name");
            ID_Str : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
         begin
            Mulog.Log (Msg => "Setting ID of device security domain '" & Name
                       & "' to " & ID_Str);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Domain,
               Name  => "id",
               Value => ID_Str);
         end;
      end loop;
   end Add_Domain_IDs;

   -------------------------------------------------------------------------

   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      DD_Node     : DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/deviceDomains");
      Events_Node : DOM.Core.Node;
   begin
      if DD_Node = null then
         Events_Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/events");

         DD_Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "deviceDomains");
         DD_Node := DOM.Core.Nodes.Insert_Before
           (N         => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
            New_Child => DD_Node,
            Ref_Child => Events_Node);
         pragma Unreferenced (DD_Node);
      end if;
   end Add_Section_Skeleton;

   -------------------------------------------------------------------------

   procedure Add_Tables (Data : in out Muxml.XML_Data_Type)
   is
      Domains : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/deviceDomains/domain");
      BSP     : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/kernel/memory/cpu[@id='0']");
   begin

      --  DMAR root table.

      Mulog.Log (Msg => "Adding VT-d DMAR root table");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "vtd_root",
         Address     => "",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "system_vtd_root",
         File_Name   => "vtd_root",
         File_Offset => "none");

      --  Interrupt Remapping (IR) table.

      Mulog.Log (Msg => "Adding VT-d IR table");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "vtd_ir",
         Address     => "",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "kernel_vtd_ir",
         File_Name   => "vtd_ir",
         File_Offset => "none");

      --  Map IRT on BSP kernel.

      Muxml.Utils.Append_Child
        (Node      => BSP,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "vtd_ir",
            Physical_Name => "vtd_ir",
            Address       => Mutools.Utils.To_Hex
              (Number => Config.VTd_IRT_Virtual_Addr),
            Writable      => True,
            Executable    => False));

      --  Do not expand regions used for context and address translation tables
      --  if no device domains are specified in the policy.

      if DOM.Core.Nodes.Length (List => Domains) = 0 then
         return;
      end if;

      --  DMAR context table for each occupied PCI bus.

      declare
         PCI_Buses : constant Mutools.XML_Utils.PCI_Bus_Set.Set
           := Mutools.XML_Utils.Get_Occupied_PCI_Buses
             (Data => Data);
         Curr_Idx  : Mutools.XML_Utils.PCI_Bus_Set.Cursor := PCI_Buses.First;
         Curr_Bus  : Mutools.XML_Utils.PCI_Bus_Range;
      begin
         while Mutools.XML_Utils.PCI_Bus_Set.Has_Element (Position => Curr_Idx)
         loop
            Curr_Bus := Mutools.XML_Utils.PCI_Bus_Set.Element
              (Position => Curr_Idx);
            declare
               Curr_Bus_Hx : constant String := Mutools.Utils.To_Hex
                 (Number    => Interfaces.Unsigned_64 (Curr_Bus),
                  Normalize => False);
            begin
               Mulog.Log (Msg => "Adding VT-d DMAR context table for PCI bus "
                          & "16#" & Curr_Bus_Hx & "#");
               Mutools.XML_Utils.Add_Memory_Region
                 (Policy      => Data,
                  Name        => "vtd_context_" & Curr_Bus_Hx,
                  Address     => "",
                  Size        => "16#1000#",
                  Caching     => "WB",
                  Alignment   => "16#1000#",
                  Memory_Type => "system_vtd_context",
                  File_Name   => "vtd_context_bus_" & Curr_Bus_Hx,
                  File_Offset => "none");
            end;
            Mutools.XML_Utils.PCI_Bus_Set.Next (Position => Curr_Idx);
         end loop;
      end;

      --  Second-level address translation tables for each domain.

      for I in 0 .. DOM.Core.Nodes.Length (List => Domains) - 1 loop
         declare
            Domain   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Domains,
                 Index => I);
            Name     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Domain,
                 Name => "name");
            Size_Str : constant String
              := Mutools.Utils.To_Hex
                (Number => XML_Utils.Calculate_PT_Size
                   (Policy             => Data,
                    Paging_Levels      =>
                      Mutools.XML_Utils.Get_IOMMU_Paging_Levels (Data => Data),
                    Large_Pages        => False,
                    Dev_Virt_Mem_XPath => "none",
                    Virt_Mem_XPath     => "/system/deviceDomains/domain"
                    & "[@name='" & Name & "']/memory/memory"));
            Descr    : constant String := "vtd_" & Name & "_pt";
         begin
            Mulog.Log (Msg => "Adding VT-d DMAR second-level paging entries "
                       & "for domain '" & Name & "' with size " & Size_Str);
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Descr,
               Address     => "",
               Size        => Size_Str,
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "system_pt",
               File_Name   => Descr,
               File_Offset => "none");
         end;
      end loop;
   end Add_Tables;

end Expanders.Device_Domains;
