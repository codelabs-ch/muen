--
--  Copyright (C) 2014-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Paging.EPT;
with Paging.Layouts;

with Mulog;
with Muxml.Utils;
with Mutools.PCI;
with Mutools.Files;
with Mutools.System_Config;
with Mutools.Utils;
with Mutools.XML_Utils;
with Mutools.Constants;

with VTd.Tables.DMAR;
with VTd.Tables.IR;
with VTd.Utils;

package body VTd.Generator
is

   package MX renames Mutools.XML_Utils;

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
   begin
      if not Mutools.System_Config.Get_Value
        (Data => Policy,
         Name => "iommu_enabled")
      then
         Mulog.Log (Msg => "IOMMU not enabled, exiting");
         return;
      end if;

      Write_Root_Table
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Write_Context_Tables
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Write_Domain_Pagetables
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Write_IR_Table
        (Output_Dir => Output_Dir,
         Policy     => Policy);
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Context_Tables
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Domains       : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/deviceDomains/domain");
      Domain_Devs   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/deviceDomains/domain/devices/device");
      Phys_Pci_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device/pci");
      PT_Memory     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[@type='system_pt']");
      Buses         : constant MX.PCI_Bus_Set.Set
          := MX.Get_Occupied_PCI_Buses
            (Data => Policy);
      Ctx_Pos       : MX.PCI_Bus_Set.Cursor := Buses.First;
      PT_Levels     : constant VTd.Tables.DMAR.Paging_Level
        := Mutools.XML_Utils.Get_IOMMU_Paging_Levels
          (Data => Policy);
   begin
      if DOM.Core.Nodes.Length (List => Domains) = 0 then
         Mulog.Log (Msg => "No VT-d device domains found, not creating VT-d "
                    & "context tables");
         return;
      end if;

      while MX.PCI_Bus_Set.Has_Element (Position => Ctx_Pos) loop
         declare
            Ctx_Table : Tables.DMAR.Context_Table_Type;
            Ctx_Bus   : constant MX.PCI_Bus_Range
              := MX.PCI_Bus_Set.Element (Position => Ctx_Pos);
            Bus_Str   : constant String
              := Mutools.Utils.To_Hex
                (Number    => Interfaces.Unsigned_64 (Ctx_Bus),
                 Normalize => False);
            Bus_Str_N : constant String
              := Mutools.Utils.To_Hex
                (Number     => Interfaces.Unsigned_64 (Ctx_Bus),
                 Byte_Short => True);
            Filename  : constant String := Output_Dir & "/"
              & "vtd_context_bus_" & Bus_Str;
            Devices   : constant DOM.Core.Node_List
              := Muxml.Utils.Get_Elements
                (Nodes     => Phys_Pci_Devs,
                 Ref_Attr  => "bus",
                 Ref_Value => Bus_Str_N);
         begin
            for I in 0 .. DOM.Core.Nodes.Length (List => Devices) - 1 loop
               declare
                  use type DOM.Core.Node;

                  PCI_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Devices,
                                            Index => I);
                  Dev      : constant Mutools.PCI.Device_Range
                    := Mutools.PCI.Device_Range'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => PCI_Node,
                          Name => "device"));
                  Func     : constant Mutools.PCI.Function_Range
                    := Mutools.PCI.Function_Range'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => PCI_Node,
                          Name => "function"));
                  Dev_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => DOM.Core.Nodes.Parent_Node (N => PCI_Node),
                       Name => "name");
                  Domain   : constant DOM.Core.Node
                    := Muxml.Utils.Ancestor_Node
                      (Node  => Muxml.Utils.Get_Element
                         (Nodes     => Domain_Devs,
                          Ref_Attr  => "physical",
                          Ref_Value => Dev_Name),
                       Level => 2);
               begin

                  --  Ignore devices which are not in a device domain
                  --  (i.e. they are not assigned to a subject). This is
                  --  enforced by the mucfgvalidate tool.

                  if Domain /= null then
                     declare
                        Dom_Name : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Domain,
                             Name => "name");
                        DID      : constant Tables.DMAR.Domain_Range
                          := Tables.DMAR.Domain_Range'Value
                            (DOM.Core.Elements.Get_Attribute
                               (Elem => Domain,
                                Name => "id"));
                        PT_Node  : constant DOM.Core.Node
                          := Muxml.Utils.Get_Element
                            (Nodes     => PT_Memory,
                             Ref_Attr  => "name",
                             Ref_Value => "vtd_" & Dom_Name & "_pt");
                        PT_Addr  : constant Tables.DMAR.Table_Pointer_Type
                          := Tables.DMAR.Table_Pointer_Type'Value
                            (DOM.Core.Elements.Get_Attribute
                               (Elem => PT_Node,
                                Name => "physicalAddress"));
                     begin
                        Mulog.Log
                          (Msg => "Adding context entry for device '"
                           & Dev_Name & "' BDF " & Bus_Str_N
                           & ":" & Mutools.Utils.To_Hex
                             (Number     => Interfaces.Unsigned_64 (Dev),
                              Byte_Short => True)
                           & ":" & Mutools.Utils.To_Hex
                             (Number     => Interfaces.Unsigned_64 (Func),
                              Byte_Short => True)
                           & " => Domain '" & Dom_Name & "', ID" & DID'Img
                           & ", SLPTPTR " & Mutools.Utils.To_Hex
                             (Number => Interfaces.Unsigned_64 (PT_Addr))
                           & ", PT-levels" & PT_Levels'Img);
                        Tables.DMAR.Add_Entry
                          (CT        => Ctx_Table,
                           Device    => Dev,
                           Func      => Func,
                           Domain    => DID,
                           PT_Levels => PT_Levels,
                           SLPTPTR   => PT_Addr);
                     end;
                  end if;
               end;
            end loop;

            Mulog.Log (Msg => "Writing VT-d context table for bus "
                       & Bus_Str_N & " to file '" & Filename & "'");
            Tables.DMAR.Serialize (CT       => Ctx_Table,
                                   Filename => Filename);
         end;

         MX.PCI_Bus_Set.Next (Position => Ctx_Pos);
      end loop;
   end Write_Context_Tables;

   -------------------------------------------------------------------------

   procedure Write_Domain_Pagetables
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Phys_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Domains : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/deviceDomains/domain");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Domains) - 1 loop
         declare
            Cur_Dom    : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Domains,
                                      Index => I);
            Dom_Name   : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Cur_Dom,
               Name => "name");
            PT_Node    : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes => Phys_Mem,
                 Refs  => ((Name  => U ("type"),
                            Value => U ("system_pt")),
                           (Name  => U ("name"),
                            Value => U ("vtd_" & Dom_Name & "_pt"))));
            Filename   : constant String := Muxml.Utils.Get_Attribute
              (Doc   => PT_Node,
               XPath => "file",
               Name  => "filename");
            Tables_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => PT_Node,
                    Name => "physicalAddress"));
            Memory      : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Cur_Dom,
                 XPath => "memory/memory");
            PT_Levels   : constant Paging.Paging_Level
              := Mutools.XML_Utils.Get_IOMMU_Paging_Levels
                (Data => Policy);
            Mem_Layout  : Paging.Layouts.Memory_Layout_Type
              (Levels => PT_Levels);
            File        : Ada.Streams.Stream_IO.File_Type;
         begin
            Mulog.Log (Msg => "Writing" & PT_Levels'Img & "-level VT-d "
                       & "pagetable of device domain '" & Dom_Name & "' to '"
                       & Output_Dir & "/" & Filename & "'");
            Paging.Layouts.Set_Large_Page_Support
              (Mem_Layout => Mem_Layout,
               State      => False);
            Paging.Layouts.Set_Address
              (Mem_Layout => Mem_Layout,
               Address    => Tables_Addr);

            for J in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
               declare
                  Mem_Node   : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Memory,
                                            Index => J);
                  Log_Name   : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Mem_Node,
                       Name => "logical");
                  Phys_Name  : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Mem_Node,
                       Name => "physical");
                  Phys_Node  : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Nodes     => Phys_Mem,
                       Ref_Attr  => "name",
                       Ref_Value => Phys_Name);
                  Size       : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Node,
                          Name => "size"));
                  PMA        : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Node,
                          Name => "physicalAddress"));
                  Mem_Type   : constant Paging.Caching_Type
                    := Paging.Caching_Type'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Node,
                          Name => "caching"));
                  VMA        : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Mem_Node,
                          Name => "virtualAddress"));
                  Executable : constant Boolean
                    := Boolean'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Mem_Node,
                          Name => "executable"));
                  Writable   : constant Boolean
                    := Boolean'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Mem_Node,
                          Name => "writable"));
               begin
                  Mulog.Log (Msg => "Adding region " & Log_Name
                             & "[" & Phys_Name & "] to domain '"
                             & Dom_Name & "'");
                  Paging.Layouts.Add_Memory_Region
                    (Mem_Layout       => Mem_Layout,
                     Physical_Address => PMA,
                     Virtual_Address  => VMA,
                     Size             => Size,
                     Caching          => Mem_Type,
                     Writable         => Writable,
                     Executable       => Executable);
               end;
            end loop;

            Paging.Layouts.Update_References (Mem_Layout => Mem_Layout);

            Mutools.Files.Open (Filename => Output_Dir & "/" & Filename,
                                File     => File);

            if PT_Levels = 3 then
               Paging.Layouts.Serialize
                 (Stream      => Ada.Streams.Stream_IO.Stream (File),
                  Mem_Layout  => Mem_Layout,
                  Serializers =>
                    (1 => Paging.EPT.Serialize_PDPT'Access,
                     2 => Paging.EPT.Serialize_PD'Access,
                     3 => Paging.EPT.Serialize_PT'Access));
            else
               Paging.Layouts.Serialize
                 (Stream      => Ada.Streams.Stream_IO.Stream (File),
                  Mem_Layout  => Mem_Layout,
                  Serializers =>
                    (1 => Paging.EPT.Serialize_PML4'Access,
                     2 => Paging.EPT.Serialize_PDPT'Access,
                     3 => Paging.EPT.Serialize_PD'Access,
                     4 => Paging.EPT.Serialize_PT'Access));
            end if;
            Ada.Streams.Stream_IO.Close (File => File);
         end;
      end loop;
   end Write_Domain_Pagetables;

   -------------------------------------------------------------------------

   procedure Write_IR_Table
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      subtype Entry_Range is Tables.IR_Entry_Range range 0 .. 255;

      package IR_Table is new Tables.IR
        (Index_Range => Entry_Range);

      IRT       : IR_Table.IR_Table_Type;
      IRT_File  : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/memory/memory[@type='system_vtd_ir']/file"
              & "[@filename='vtd_ir']"),
           Name => "filename");
      IRQs      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject/devices/device/irq");
      Phys_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device");
   begin
      if IRT_File'Length > 0 then
         Mulog.Log (Msg => "Writing VT-d interrupt remapping table to file '"
                    & Output_Dir & "/" & IRT_File & "'");

         for I in 0 .. DOM.Core.Nodes.Length (List => IRQs) - 1 loop
            declare
               use type Interfaces.Unsigned_8;
               use type DOM.Core.Node;

               IRQ : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => IRQs,
                    Index => I);
               IRQ_Ref : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => IRQ,
                    Name => "physical");
               Dev : constant DOM.Core.Node
                 := DOM.Core.Nodes.Parent_Node (N => IRQ);
               Dev_Ref : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Dev,
                    Name => "physical");
               Dev_Phys : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Phys_Devs,
                    Ref_Attr  => "name",
                    Ref_Value => Dev_Ref);
               IRQ_Kind : constant MX.IRQ_Kind
                 := MX.Get_IRQ_Kind (Dev => Dev_Phys);
               PCI_BDF : constant Mutools.PCI.BDF_Type
                 := Mutools.PCI.Get_BDF (Dev => Dev_Phys);
               IRQ_Phys : constant Entry_Range
                 := Entry_Range'Value
                   (Muxml.Utils.Get_Attribute
                      (Doc   => Dev_Phys,
                       XPath => "irq[@name='" & IRQ_Ref & "']",
                       Name  => "number"));
               Host_Vector : constant Interfaces.Unsigned_8
                 := Interfaces.Unsigned_8 (IRQ_Phys)
                 + Mutools.Constants.Host_IRQ_Remap_Offset;
               CPU_ID : constant Natural
                 := Natural'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Muxml.Utils.Ancestor_Node
                         (Node  => Dev,
                          Level => 2),
                       Name => "cpu"));
               APIC_ID : constant Interfaces.Unsigned_32
                 := Interfaces.Unsigned_32
                   (Mutools.XML_Utils.To_APIC_ID
                      (Policy => Policy,
                       CPU_ID => CPU_ID));
               TM  : Tables.Bit_Type;
               SID : Interfaces.Unsigned_16;
            begin
               Utils.Get_IR_TM_SID (Kind => IRQ_Kind,
                                    BDF  => PCI_BDF,
                                    TM   => TM,
                                    SID  => SID);
               Mulog.Log
                 (Msg => "IRT index" & IRQ_Phys'Img & ", " & IRQ_Kind'Img
                  & ", device '" & Dev_Ref & "' (SID " & Mutools.Utils.To_Hex
                    (Number => Interfaces.Unsigned_64 (SID)) & ")" & ", host "
                  & "vector" & Host_Vector'Img & ", CPU" & CPU_ID'Img
                  & ", APIC ID" & APIC_ID'Img);

               IR_Table.Add_Entry
                 (IRT    => IRT,
                  Index  => IRQ_Phys,
                  Vector => Host_Vector,
                  DST    => APIC_ID,
                  SID    => SID,
                  TM     => TM);
            end;
         end loop;

         IR_Table.Serialize (IRT      => IRT,
                             Filename => Output_Dir & "/" & IRT_File);
      end if;
   end Write_IR_Table;

   -------------------------------------------------------------------------

   procedure Write_Root_Table
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Domains   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/deviceDomains/domain");
      Ctx_Files : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[@type='system_vtd_context']/file");
      Buses     : constant MX.PCI_Bus_Set.Set := MX.Get_Occupied_PCI_Buses
        (Data => Policy);
      Ctx_Pos   : MX.PCI_Bus_Set.Cursor       := Buses.First;
      Root      : Tables.DMAR.Root_Table_Type;
      Root_File : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/memory/memory[@type='system_vtd_root']/file"
              & "[@filename='vtd_root']"),
           Name => "filename");
   begin
      if DOM.Core.Nodes.Length (List => Domains) /= 0 then
         while MX.PCI_Bus_Set.Has_Element (Position => Ctx_Pos) loop
            declare
               Ctx_Bus   : constant MX.PCI_Bus_Range
                 := MX.PCI_Bus_Set.Element (Position => Ctx_Pos);
               Bus_Str   : constant String
                 := Mutools.Utils.To_Hex
                   (Number    => Interfaces.Unsigned_64 (Ctx_Bus),
                    Normalize => False);
               Bus_Str_N : constant String
                 := Mutools.Utils.To_Hex
                   (Number     => Interfaces.Unsigned_64 (Ctx_Bus),
                    Byte_Short => True);
               Filename  : constant String := "vtd_context_bus_" & Bus_Str;
               Mem_Node  : constant DOM.Core.Node
                 := DOM.Core.Nodes.Parent_Node
                   (N => Muxml.Utils.Get_Element
                      (Nodes     => Ctx_Files,
                       Ref_Attr  => "filename",
                       Ref_Value => Filename));
               Ctx_Addr  : constant Tables.DMAR.Table_Pointer_Type
                 := Tables.DMAR.Table_Pointer_Type'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Mem_Node,
                       Name => "physicalAddress"));
            begin
               Mulog.Log (Msg => "Adding root entry for PCI bus " & Bus_Str_N
                          & ": " & Mutools.Utils.To_Hex
                            (Number => Interfaces.Unsigned_64 (Ctx_Addr)));
               Tables.DMAR.Add_Entry
                 (RT  => Root,
                  Bus => Tables.DMAR.Table_Index_Type (Ctx_Bus),
                  CTP => Ctx_Addr);
            end;

            MX.PCI_Bus_Set.Next (Position => Ctx_Pos);
         end loop;
      end if;

      Mulog.Log (Msg => "Writing VT-d root table to file '" & Output_Dir & "/"
                 & Root_File & "'");
      Tables.DMAR.Serialize (RT       => Root,
                             Filename => Output_Dir & "/" & Root_File);
   end Write_Root_Table;

end VTd.Generator;
