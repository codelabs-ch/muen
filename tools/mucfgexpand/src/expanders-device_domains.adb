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
with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;
with DOM.Core.Append_Node;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;
with Mutools.System_Config;

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

   procedure Add_Reserved_Memory_Region_Mappings
     (Data : in out Muxml.XML_Data_Type)
   is
      Phys_Regions : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/memory/memory[@type='device_rmrr']");
      Domains : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/deviceDomains/domain");

      --  Return list of physical reserved memory regions that are referenced
      --  by devices of the given device domain.
      function Get_Physical_Reserved_Regions
        (Device_Domain : DOM.Core.Node)
         return DOM.Core.Node_List;

      --  Map each physical memory region in the given list into the specified
      --  device domain.
      procedure Map_Memory_Regions
        (Device_Domain : DOM.Core.Node;
         Mem_Regions   : DOM.Core.Node_List);

      ----------------------------------------------------------------------

      function Get_Physical_Reserved_Regions
        (Device_Domain : DOM.Core.Node)
         return DOM.Core.Node_List
      is
         Dev_Refs : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Device_Domain,
              XPath => "devices/device[@mapReservedMemory='true']");
         Regions  : DOM.Core.Node_List;
      begin
         for I in 1 .. DOM.Core.Nodes.Length (List => Dev_Refs) loop
            declare
               Dev_Ref : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Dev_Refs,
                    Index => I - 1);
               Dev_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Dev_Ref,
                    Name => "physical");
               RMRR_Refs : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Data.Doc,
                    XPath => "/system/hardware/devices/device[@name='"
                    & Dev_Name & "']/reservedMemory");
            begin
               for J in 1 .. DOM.Core.Nodes.Length (List => RMRR_Refs) loop
                  declare
                     RMRR_Ref : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                         (List  => RMRR_Refs,
                          Index => J - 1);
                     RMRR_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => RMRR_Ref,
                          Name => "ref");
                     RMRR : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Nodes     => Phys_Regions,
                          Ref_Attr  => "name",
                          Ref_Value => RMRR_Name);
                  begin
                     if not Muxml.Utils.Contains (List => Regions,
                                                  Node => RMRR)
                     then

                        --  Only add region to list if it was not already
                        --  referenced by a prior device.

                        DOM.Core.Append_Node (List => Regions,
                                              N    => RMRR);
                     end if;
                  end;
               end loop;
            end;
         end loop;

         return Regions;
      end Get_Physical_Reserved_Regions;

      ----------------------------------------------------------------------

      procedure Map_Memory_Regions
        (Device_Domain : DOM.Core.Node;
         Mem_Regions   : DOM.Core.Node_List)
      is
         use type DOM.Core.Node;

         Region_Count : constant Natural
           := DOM.Core.Nodes.Length (List => Mem_Regions);
         Domain_Name  : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Device_Domain,
              Name => "name");
         Domain_Mem   : DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Device_Domain,
            XPath => "memory");
      begin
         if Region_Count < 1 then
            return;
         end if;

         --  Create memory child element if not yet present.

         if Domain_Mem = null then
            Domain_Mem := DOM.Core.Documents.Create_Element
              (Doc      => Data.Doc,
               Tag_Name => "memory");
            Domain_Mem := DOM.Core.Nodes.Insert_Before
              (N         => Device_Domain,
               New_Child => Domain_Mem,
               Ref_Child => Muxml.Utils.Get_Element
                 (Doc   => Device_Domain,
                  XPath => "devices"));
         end if;

         --  Create mapping for each memory region.

         for I in 1 .. Region_Count loop
            declare
               Phys_Region : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Mem_Regions,
                                         Index => I - 1);
               Phys_Name   : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Region,
                    Name => "name");
               Phys_Addr   : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Region,
                    Name => "physicalAddress");
               Mapping     : constant DOM.Core.Node
                 := Mutools.XML_Utils.Create_Virtual_Memory_Node
                   (Policy        => Data,
                    Logical_Name  => Phys_Name,
                    Physical_Name => Phys_Name,
                    Address       => Phys_Addr,
                    Writable      => True,
                    Executable    => False);
            begin
               Mulog.Log (Msg => "Mapping RMRR region '" & Phys_Name
                          & "' into device security domain '"
                          & Domain_Name & "'");
               Muxml.Utils.Append_Child
                 (Node      => Domain_Mem,
                  New_Child => Mapping);
            end;
         end loop;
      end Map_Memory_Regions;
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Domains) loop
         declare
            Domain : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Domains,
                 Index => I - 1);
            Reserved_Regions : constant DOM.Core.Node_List
              := Get_Physical_Reserved_Regions (Device_Domain => Domain);
         begin
            Map_Memory_Regions (Device_Domain => Domain,
                                Mem_Regions   => Reserved_Regions);
         end;
      end loop;
   end Add_Reserved_Memory_Region_Mappings;

   -------------------------------------------------------------------------

   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Muxml.Utils.Add_Child
        (Parent     => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
         Child_Name => "deviceDomains",
         Ref_Names  => (1 => Ada.Strings.Unbounded.To_Unbounded_String
                        ("events")));
   end Add_Section_Skeleton;

   -------------------------------------------------------------------------

   procedure Add_Tables (Data : in out Muxml.XML_Data_Type)
   is
      Domains : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/deviceDomains/domain");
      IOMMU : constant Boolean
        := Mutools.System_Config.Get_Value
          (Data => Data,
           Name => "iommu_enabled");
   begin

      --  DMAR root table.

      Mulog.Log (Msg => "Adding VT-d DMAR root table");
      if IOMMU then
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
      else

         --  The filled region guarantees allocation in lower memory.

         Mutools.XML_Utils.Add_Memory_Region
           (Policy      => Data,
            Name        => "vtd_root",
            Address     => "",
            Size        => "16#1000#",
            Caching     => "WB",
            Alignment   => "16#1000#",
            Memory_Type => "system_vtd_root",
            Fill_Pattern => "16#00#");
      end if;

      --  Interrupt Remapping (IR) table.

      Mulog.Log (Msg => "Adding VT-d IR table");
      if IOMMU then
         Mutools.XML_Utils.Add_Memory_Region
           (Policy      => Data,
            Name        => "vtd_ir",
            Address     => "",
            Size        => "16#1000#",
            Caching     => "WB",
            Alignment   => "16#1000#",
            Memory_Type => "system_vtd_ir",
            File_Name   => "vtd_ir",
            File_Offset => "none");
      else

         --  The filled region guarantees allocation in lower memory.

         Mutools.XML_Utils.Add_Memory_Region
           (Policy       => Data,
            Name         => "vtd_ir",
            Address      => "",
            Size         => "16#1000#",
            Caching      => "WB",
            Alignment    => "16#1000#",
            Memory_Type  => "system_vtd_ir",
            Fill_Pattern => "16#00#");
      end if;

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

   -------------------------------------------------------------------------

   procedure Remove_Map_Reserved_Mem_Attribute
     (Data : in out Muxml.XML_Data_Type)
   is
      Devices :  constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/deviceDomains/domain/devices/"
           & "device[@mapReservedMemory]");
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Devices) loop
         declare
            Cur_Dev : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Devices,
               Index => I - 1);
         begin
            DOM.Core.Elements.Remove_Attribute
              (Elem => Cur_Dev,
               Name => "mapReservedMemory");
         end;
      end loop;
   end Remove_Map_Reserved_Mem_Attribute;

end Expanders.Device_Domains;
