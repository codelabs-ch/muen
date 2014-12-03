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

with McKae.XML.XPath.XIA;

with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.Constants;
with Mutools.XML_Utils;

with Expanders.Config;

package body Expanders.Kernel
is

   -------------------------------------------------------------------------

   procedure Add_Binary_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      CPU_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/kernel/memory/cpu");
   begin
      Mulog.Log (Msg => "Adding binary memory mappings for"
                 & DOM.Core.Nodes.Length (List => CPU_Nodes)'Img
                 & " kernel(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => CPU_Nodes) - 1 loop
         declare
            CPU_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => CPU_Nodes,
                 Index => I);
            CPU_Str  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => CPU_Node,
                 Name => "id");
         begin
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "text",
                  Physical_Name => "kernel_text",
                  Address       => "16#0010_0000#",
                  Writable      => False,
                  Executable    => True));
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "data",
                  Physical_Name => "kernel_data",
                  Address       => "16#0011_0000#",
                  Writable      => True,
                  Executable    => False));
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "bss",
                  Physical_Name => "kernel_bss",
                  Address       => "16#0011_1000#",
                  Writable      => True,
                  Executable    => False));
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "ro",
                  Physical_Name => "kernel_ro",
                  Address       => Mutools.Utils.To_Hex
                    (Number => Config.Kernel_RO_Section_Addr),
                  Writable      => False,
                  Executable    => False));
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "stack",
                  Physical_Name => "kernel_stack_" & CPU_Str,
                  Address       => "16#0011_3000#",
                  Writable      => True,
                  Executable    => False));
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "store",
                  Physical_Name => "kernel_store_" & CPU_Str,
                  Address       => "16#0011_6000#",
                  Writable      => True,
                  Executable    => False));
         end;
      end loop;
   end Add_Binary_Mappings;

   -------------------------------------------------------------------------

   procedure Add_Devices (Data : in out Muxml.XML_Data_Type)
   is

      --  Base address of kernel device mappings.
      Base_Address : Interfaces.Unsigned_64
        := Config.Kernel_Devices_Virtual_Addr;

      --  Create device reference with given device, MMIO region name and MMIO
      --  address.
      function Create_Device_Reference
        (Device_Name : String;
         MMIO_Name   : String;
         MMIO_Addr   : String)
         return DOM.Core.Node;

      --  Add I/O APIC.
      procedure Add_IO_APIC (Devices : DOM.Core.Node);

      --  Add IOMMUs (if present).
      procedure Add_IOMMUs (Devices : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Add_IO_APIC (Devices : DOM.Core.Node)
      is
         use type Interfaces.Unsigned_64;

         Addr     : constant String := Mutools.Utils.To_Hex
           (Number => Base_Address);
         Mem_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              XPath => "/system/platform/devices/device[@name='ioapic']"
              & "/memory");
         Mem_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Mem_Node,
            Name => "name");
         Mem_Size : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Mem_Node,
                 Name => "size"));
      begin
         Mulog.Log (Msg => "Adding I/O APIC to kernel devices, MMIO: " & Addr);
         Muxml.Utils.Append_Child
           (Node      => Devices,
            New_Child => Create_Device_Reference
              (Device_Name => "ioapic",
               MMIO_Name   => Mem_Name,
               MMIO_Addr   => Addr));
         Base_Address := Base_Address + Mem_Size;
      end Add_IO_APIC;

      ----------------------------------------------------------------------

      procedure Add_IOMMUs (Devices : DOM.Core.Node)
      is
         Physdevs : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Data.Doc,
              XPath => "/system/platform/devices/device[starts-with"
              & "(string(@name),'iommu')]");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Physdevs) - 1 loop
            declare
               use type Interfaces.Unsigned_64;

               Addr_Str : constant String
                 := Mutools.Utils.To_Hex (Number => Base_Address);
               IOMMU    : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Physdevs,
                                         Index => I);
               Dev_Name : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => IOMMU,
                  Name => "name");
               Mem_Node : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element (Doc   => IOMMU,
                                             XPath => "memory");
               Mem_Name : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Mem_Node,
                  Name => "name");
               Mem_Size : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Mem_Node,
                       Name => "size"));
            begin
               Mulog.Log (Msg => "Adding IOMMU '" & Dev_Name
                          & "' to kernel devices, MMIO: " & Addr_Str);
               Muxml.Utils.Append_Child
                 (Node      => Devices,
                  New_Child => Create_Device_Reference
                    (Device_Name => Dev_Name,
                     MMIO_Name   => Mem_Name,
                     MMIO_Addr   => Addr_Str));

               Base_Address := Base_Address + Mem_Size;
            end;
         end loop;
      end Add_IOMMUs;

      ----------------------------------------------------------------------

      function Create_Device_Reference
        (Device_Name : String;
         MMIO_Name   : String;
         MMIO_Addr   : String)
         return DOM.Core.Node
      is
         Ref : constant DOM.Core.Node
           := DOM.Core.Documents.Create_Element
             (Doc      => Data.Doc,
              Tag_Name => "device");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Ref,
            Name  => "logical",
            Value => Device_Name);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Ref,
            Name  => "physical",
            Value => Device_Name);

         Muxml.Utils.Append_Child
           (Node      => Ref,
            New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
              (Policy        => Data,
               Logical_Name  => MMIO_Name,
               Physical_Name => MMIO_Name,
               Address       => MMIO_Addr,
               Writable      => True,
               Executable    => False));

         return Ref;
      end Create_Device_Reference;

      Devices_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/kernel/devices");
   begin
      Add_IO_APIC (Devices => Devices_Node);
      Add_IOMMUs  (Devices => Devices_Node);
   end Add_Devices;

   -------------------------------------------------------------------------

   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type)
   is
      CPU_Count     : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Data);
      Kernel_Node   : DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "kernel");
      Memory_Node   : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "memory");
      Subjects_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/subjects");
   begin
      Kernel_Node := DOM.Core.Nodes.Insert_Before
        (N         => DOM.Core.Nodes.Parent_Node (N => Subjects_Node),
         New_Child => Kernel_Node,
         Ref_Child => Subjects_Node);

      Muxml.Utils.Append_Child
        (Node      => Kernel_Node,
         New_Child => Memory_Node);
      Muxml.Utils.Append_Child
        (Node      => Kernel_Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "devices"));

      for I in 0 .. CPU_Count - 1 loop
         declare
            CPU_Str  : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            CPU_Node : constant DOM.Core.Node
              := DOM.Core.Documents.Create_Element
                (Doc      => Data.Doc,
                 Tag_Name => "cpu");
         begin
            DOM.Core.Elements.Set_Attribute
              (Elem  => CPU_Node,
               Name  => "id",
               Value => CPU_Str);
            Muxml.Utils.Append_Child
              (Node      => Memory_Node,
               New_Child => CPU_Node);
         end;
      end loop;
   end Add_Section_Skeleton;

   -------------------------------------------------------------------------

   procedure Add_Subj_State_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      State_Start : constant := Config.Subject_States_Virtual_Addr;
      CPU_Nodes   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/kernel/memory/cpu");
      Subj_Nodes  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => CPU_Nodes) - 1 loop
         declare
            CPU      : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => CPU_Nodes,
                 Index => I);
            CPU_Id   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => CPU,
                 Name => "id");
            Subjects : constant DOM.Core.Node_List
              := Muxml.Utils.Get_Elements
                (Nodes     => Subj_Nodes,
                 Ref_Attr  => "cpu",
                 Ref_Value => CPU_Id);
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
               declare
                  use type Interfaces.Unsigned_64;

                  Subj      : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Subjects,
                       Index => J);
                  Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Subj,
                       Name => "name");
                  Subj_Id   : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Subj,
                       Name => "id");
                  Address   : constant Interfaces.Unsigned_64
                    := State_Start + Interfaces.Unsigned_64'Value (Subj_Id)
                    * Mutools.Constants.Page_Size;
               begin
                  Mulog.Log (Msg => "Mapping state of subject '" & Subj_Name
                             & "' to address " & Mutools.Utils.To_Hex
                               (Number => Address) & " on CPU " & CPU_Id);
                  Muxml.Utils.Append_Child
                    (Node      => CPU,
                     New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                       (Policy        => Data,
                        Logical_Name  => Subj_Name & "_state",
                        Physical_Name => Subj_Name & "_state",
                        Address       => Mutools.Utils.To_Hex
                          (Number => Address),
                        Writable      => True,
                        Executable    => False));
               end;
            end loop;
         end;
      end loop;
   end Add_Subj_State_Mappings;

   -------------------------------------------------------------------------

   procedure Add_Subj_Timer_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      Start_Addr : constant := Config.Subject_Timers_Virtual_Addr;
      CPU_Nodes  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/kernel/memory/cpu");
      Subj_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => CPU_Nodes) - 1 loop
         declare
            CPU      : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => CPU_Nodes,
                 Index => I);
            CPU_Id   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => CPU,
                 Name => "id");
            Subjects : constant DOM.Core.Node_List
              := Muxml.Utils.Get_Elements
                (Nodes     => Subj_Nodes,
                 Ref_Attr  => "cpu",
                 Ref_Value => CPU_Id);
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
               declare
                  use type Interfaces.Unsigned_64;

                  Subj      : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Subjects,
                       Index => J);
                  Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Subj,
                       Name => "name");
                  Subj_Id   : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Subj,
                       Name => "id");
                  Address   : constant Interfaces.Unsigned_64
                    := Start_Addr + Interfaces.Unsigned_64'Value (Subj_Id)
                    * Mutools.Constants.Page_Size;
               begin
                  Mulog.Log (Msg => "Mapping timer page of subject '"
                             & Subj_Name & "' to address "
                             & Mutools.Utils.To_Hex (Number => Address)
                             & " on CPU " & CPU_Id);
                  Muxml.Utils.Append_Child
                    (Node      => CPU,
                     New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                       (Policy        => Data,
                        Logical_Name  => Subj_Name & "_timer",
                        Physical_Name => Subj_Name & "_timer",
                        Address       => Mutools.Utils.To_Hex
                          (Number => Address),
                        Writable      => True,
                        Executable    => False));
               end;
            end loop;
         end;
      end loop;
   end Add_Subj_Timer_Mappings;

   -------------------------------------------------------------------------

   procedure Map_Tau0_Interface (Data : in out Muxml.XML_Data_Type)
   is
      BSP : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='0']");
   begin
      Mulog.Log (Msg => "Mapping tau0 system interface on CPU 0");

      Muxml.Utils.Append_Child
        (Node      => BSP,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "tau0_interface",
            Physical_Name => "sys_interface",
            Address       => Mutools.Utils.To_Hex
              (Number => Config.Tau0_Interface_Virtual_Addr),
            Writable      => False,
            Executable    => False));
   end Map_Tau0_Interface;

end Expanders.Kernel;
