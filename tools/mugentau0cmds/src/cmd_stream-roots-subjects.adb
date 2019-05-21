--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Paging.Layouts;

with Muxml.Utils;
with Mutools.Constants;
with Mutools.Utils;

with Cmd_Stream.Constants;
with Cmd_Stream.XML_Utils;

package body Cmd_Stream.Roots.Subjects
is

   package MC renames Mutools.Constants;

   --  Assign devices to given subject.
   procedure Assign_Devices
     (Stream_Doc : Muxml.XML_Data_Type;
      Policy     : Muxml.XML_Data_Type;
      Subj_Attr  : Cmd_Stream.XML_Utils.Attribute_Type;
      Subj_Node  : DOM.Core.Node);

   --  Assign MSRs to given subject.
   procedure Assign_MSRs
     (Stream_Doc : Muxml.XML_Data_Type;
      Subj_Attr  : Cmd_Stream.XML_Utils.Attribute_Type;
      Subj_Node  : DOM.Core.Node);

   --  Assign I/O ports of device to given subject.
   procedure Assign_IO_Ports
     (Stream_Doc   : Muxml.XML_Data_Type;
      Subj_Attr    : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Attr     : Cmd_Stream.XML_Utils.Attribute_Type;
      Logical_Dev  : DOM.Core.Node;
      Physical_Dev : DOM.Core.Node);

   --  Assign IRQs of device to given subject.
   procedure Assign_IRQs
     (Stream_Doc   : Muxml.XML_Data_Type;
      Subj_Attr    : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Attr     : Cmd_Stream.XML_Utils.Attribute_Type;
      Logical_Dev  : DOM.Core.Node;
      Physical_Dev : DOM.Core.Node);

   --  Assign memory to given subject.
   procedure Assign_Memory
     (Stream_Doc   : Muxml.XML_Data_Type;
      Physical_Mem : DOM.Core.Node_List;
      Subj_Attr    : Cmd_Stream.XML_Utils.Attribute_Type;
      Subj_Node    : DOM.Core.Node);

   --  Next free page table index.
   Next_Table_Index : Positive := 1;

   -------------------------------------------------------------------------

   procedure Assign_Devices
     (Stream_Doc : Muxml.XML_Data_Type;
      Policy     : Muxml.XML_Data_Type;
      Subj_Attr  : Cmd_Stream.XML_Utils.Attribute_Type;
      Subj_Node  : DOM.Core.Node)
   is
      Phys_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device");
      Devices   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Subj_Node,
           XPath => "devices/device");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Devices) - 1 loop
         declare
            Logical_Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Devices,
                 Index => I);
            Physical_Dev : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Phys_Devs,
                 Ref_Attr  => "name",
                 Ref_Value => DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Dev,
                    Name => "physical"));
            Dev_Attr : constant XML_Utils.Attribute_Type
              := (Attr  => U ("device"),
                  Value => U (DOM.Core.Elements.Get_Attribute
                    (Elem => Physical_Dev,
                     Name => "tau0DeviceId")));
         begin
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "assignDeviceSubject",
               Attrs      => (Subj_Attr, Dev_Attr));
            Assign_IO_Ports
              (Stream_Doc   => Stream_Doc,
               Subj_Attr    => Subj_Attr,
               Dev_Attr     => Dev_Attr,
               Logical_Dev  => Logical_Dev,
               Physical_Dev => Physical_Dev);
            Assign_IRQs
              (Stream_Doc   => Stream_Doc,
               Subj_Attr    => Subj_Attr,
               Dev_Attr     => Dev_Attr,
               Logical_Dev  => Logical_Dev,
               Physical_Dev => Physical_Dev);
         end;
      end loop;
   end Assign_Devices;

   -------------------------------------------------------------------------

   procedure Assign_IO_Ports
     (Stream_Doc   : Muxml.XML_Data_Type;
      Subj_Attr    : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Attr     : Cmd_Stream.XML_Utils.Attribute_Type;
      Logical_Dev  : DOM.Core.Node;
      Physical_Dev : DOM.Core.Node)
   is
      Ports : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Logical_Dev,
           XPath => "ioPort");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Ports) - 1 loop
         declare
            Phys_Port_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Item
                   (List  => Ports,
                    Index => I),
                 Name => "physical");
            Phys_Port : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Physical_Dev,
                 XPath => "ioPort[@name='" & Phys_Port_Name & "']");
            From : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Phys_Port,
                 Name => "start");
            To : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Phys_Port,
                 Name => "end");
         begin
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "setIOPortRange",
               Attrs      => (Subj_Attr,
                              Dev_Attr,
                              (Attr  => U ("from"),
                               Value => U (From)),
                              (Attr  => U ("to"),
                               Value => U (To)),
                              (Attr  => U ("mode"),
                               Value => U ("allowed"))));
         end;
      end loop;
   end Assign_IO_Ports;

   -------------------------------------------------------------------------

   procedure Assign_IRQs
     (Stream_Doc   : Muxml.XML_Data_Type;
      Subj_Attr    : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Attr     : Cmd_Stream.XML_Utils.Attribute_Type;
      Logical_Dev  : DOM.Core.Node;
      Physical_Dev : DOM.Core.Node)
   is
      IRQs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Logical_Dev,
           XPath => "irq");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => IRQs) - 1 loop
         declare
            Phys_IRQ_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Item
                   (List  => IRQs,
                    Index => I),
                 Name => "physical");
            Phys_IRQ : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Physical_Dev,
                 XPath => "irq[@name='" & Phys_IRQ_Name & "']");
            IRQ : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Phys_IRQ,
                 Name => "number");
         begin
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "assignIRQSubject",
               Attrs      => (Subj_Attr,
                              Dev_Attr,
                              (Attr  => U ("irq"),
                               Value => U (IRQ))));
         end;
      end loop;
   end Assign_IRQs;

   -------------------------------------------------------------------------

   procedure Assign_Memory
     (Stream_Doc   : Muxml.XML_Data_Type;
      Physical_Mem : DOM.Core.Node_List;
      Subj_Attr    : Cmd_Stream.XML_Utils.Attribute_Type;
      Subj_Node    : DOM.Core.Node)
   is
      Virt_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Subj_Node,
           XPath => "memory/memory");
      Mem_Layout : Paging.Layouts.Memory_Layout_Type (Levels => 4);
      Map_Cmd_Buf : XML_Utils.Command_Buffer_Type;
   begin
      Paging.Layouts.Set_Address
        (Mem_Layout => Mem_Layout,
         Address    => PT_Addr);
      Paging.Layouts.Set_Large_Page_Support
        (Mem_Layout => Mem_Layout,
         State      => False);
      for I in 0 .. DOM.Core.Nodes.Length (List => Virt_Memory) - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            Virt_Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Virt_Memory,
                                      Index => I);
            Virt_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Virt_Mem,
                    Name => "virtualAddress"));
            Writable : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Virt_Mem,
                 Name => "writable");
            Executable : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Virt_Mem,
                 Name => "executable");
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Virt_Mem,
                                                  Name => "physical");
            Phys_Mem : constant DOM.Core.Node
              := Muxml.Utils.Get_Element (Nodes     => Physical_Mem,
                                          Ref_Attr  => "name",
                                          Ref_Value => Phys_Name);
            Phys_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Mem,
                    Name => "physicalAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Mem,
                    Name => "size"));
            Caching : constant Paging.Caching_Type
              := Paging.Caching_Type'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Mem,
                    Name => "caching"));
            MR_ID_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Phys_Mem,
                 Name => Constants.MR_ID_Attr_Name);
            Region_Attr : constant XML_Utils.Attribute_Type
              := (Attr  => U ("region"),
                  Value => U (MR_ID_Str));
            Cur_Table_Idx : constant Natural := Next_Table_Index;
            Table_Idx_Attr : constant XML_Utils.Attribute_Type
              := (Attr  => U ("tableIndex"),
                  Value => U (Trim (Cur_Table_Idx'Img)));
         begin
            Next_Table_Index := Next_Table_Index + 1;
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "attachMemoryRegionSubject",
               Attrs      => (Subj_Attr,
                              Region_Attr,
                              (Attr  => U ("offset"),
                               Value => U ("0")),
                              (Attr  => U ("length"),
                               Value => U (Trim
                                 (Interfaces.Unsigned_64'Image
                                    (Size / MC.Page_Size)))),
                              Table_Idx_Attr,
                              (Attr  => U ("writable"),
                               Value => U (Writable)),
                              (Attr  => U ("executable"),
                               Value => U (Executable))));

            declare
               End_Virt_Addr : constant Interfaces.Unsigned_64
                 := Virt_Addr + Size;
               Cur_Offset : Interfaces.Unsigned_64 := 0;
            begin
               while Virt_Addr + Cur_Offset < End_Virt_Addr loop
                  XML_Utils.Append_Command
                    (Buffer     => Map_Cmd_Buf,
                     Stream_Doc => Stream_Doc,
                     Name       => "mapPageSubject",
                     Attrs      => (Subj_Attr,
                                    Table_Idx_Attr,
                                    (Attr  => U ("virtualAddress"),
                                     Value => U (Mutools.Utils.To_Hex
                                       (Number => Virt_Addr + Cur_Offset))),
                                    (Attr  => U ("offset"),
                                     Value => U
                                       (Trim (Interfaces.Unsigned_64'Image
                                        (Cur_Offset / MC.Page_Size))))));
                  Cur_Offset := Cur_Offset + MC.Page_Size;
               end loop;
            end;

            Paging.Layouts.Add_Memory_Region
              (Mem_Layout       => Mem_Layout,
               Physical_Address => Phys_Addr,
               Virtual_Address  => Virt_Addr,
               Size             => Size,
               Caching          => Caching,
               Writable         => Boolean'Value (Writable),
               Executable       => Boolean'Value (Executable));
         end;
      end loop;

      XML_Utils.Append_Commands
        (Stream_Doc => Stream_Doc,
         Buffer     => Map_Cmd_Buf);
   end Assign_Memory;

   -------------------------------------------------------------------------

   procedure Assign_MSRs
     (Stream_Doc : Muxml.XML_Data_Type;
      Subj_Attr  : Cmd_Stream.XML_Utils.Attribute_Type;
      Subj_Node  : DOM.Core.Node)
   is
      MSRs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Subj_Node,
           XPath => "vcpu/registers/msrs/msr");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => MSRs) - 1 loop
         declare
            MSR : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => MSRs,
                 Index => I);
            From : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => MSR,
                 Name => "start");
            To : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => MSR,
                 Name => "end");
            Mode : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => MSR,
                 Name => "mode");
         begin
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "setMSRRange",
               Attrs      => (Subj_Attr,
                              (Attr  => U ("from"),
                               Value => U (From)),
                              (Attr  => U ("to"),
                               Value => U (To)),
                              (Attr  => U ("mode"),
                               Value => U (Mode))));
         end;
      end loop;
   end Assign_MSRs;

   -------------------------------------------------------------------------

   procedure Create_Subjects
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out Muxml.XML_Data_Type)
   is
      Phys_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            Subj : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj,
                 Name => "name");
            Root_ID : constant Natural := Allocate_Root;
            CPU : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj,
                 Name => "cpu");
            Msrbm_Addr : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Muxml.Utils.Get_Element
                   (Nodes => Phys_Mem,
                    Refs  => ((Name  => U ("type"),
                               Value => U ("system_msrbm")),
                              (Name  => U ("name"),
                               Value => U (Name & "|msrbm")))),
                 Name => "physicalAddress");
            Iobm_Addr_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Muxml.Utils.Get_Element
                   (Nodes => Phys_Mem,
                    Refs  => ((Name  => U ("type"),
                               Value => U ("system_iobm")),
                              (Name  => U ("name"),
                               Value => U (Name & "|iobm")))),
                 Name => "physicalAddress");
            Iobm_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value (Iobm_Addr_Str);
            Paging : constant String
              := (if Muxml.Utils.Get_Element_Value
                  (Doc   => Subj,
                   XPath => "vcpu/vmx/controls/proc2/EnableEPT") = "1"
                  then "EPT" else "IA32e");
            Subj_Attr : constant XML_Utils.Attribute_Type
              := (Attr  => U ("subject"),
                  Value => U (Trim (Root_ID'Img)));
         begin
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "clearPage",
               Attrs      => (1 => (Attr  => U ("page"),
                                    Value => U (Msrbm_Addr))));
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "clearPage",
               Attrs      => (1 => (Attr  => U ("page"),
                                    Value => U (Iobm_Addr_Str))));
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "clearPage",
               Attrs      => (1 => (Attr  => U ("page"),
                                    Value => U (Mutools.Utils.To_Hex
                                      (Number => Iobm_Addr + MC.Page_Size)))));

            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "createSubject",
               Attrs      => (Subj_Attr,
                              (Attr  => U ("cpu"),
                               Value => U (CPU)),
                              (Attr  => U ("paging"),
                               Value => U (Paging)),
                              (Attr  => U ("msrBitmap"),
                               Value => U (Msrbm_Addr)),
                              (Attr  => U ("ioBitmap"),
                               Value => U (Iobm_Addr_Str))));
            Assign_Devices (Stream_Doc => Stream_Doc,
                            Policy     => Policy,
                            Subj_Attr  => Subj_Attr,
                            Subj_Node  => Subj);
            Assign_MSRs (Stream_Doc => Stream_Doc,
                         Subj_Attr  => Subj_Attr,
                         Subj_Node  => Subj);
            Assign_Memory
              (Stream_Doc   => Stream_Doc,
               Physical_Mem => Phys_Mem,
               Subj_Attr    => Subj_Attr,
               Subj_Node    => Subj);
         end;
      end loop;
   end Create_Subjects;

end Cmd_Stream.Roots.Subjects;
