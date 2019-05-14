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

with Muxml.Utils;
with Mutools.Constants;

with Cmd_Stream.XML_Utils;

package body Cmd_Stream.Devices
is

   Current_Device : Natural := 0;

   --  Create physical device for device node with specified command and
   --  attributes. The first attribute is expected to be 'device'.
   procedure Create_Physical_Device
     (Dev_Node   :        DOM.Core.Node;
      Command    :        String;
      Attributes :        XML_Utils.Attribute_Array;
      Stream_Doc : in out Muxml.XML_Data_Type);

   --  Assign I/O ports to given device.
   procedure Assign_IO_Ports
     (Stream_Doc : Muxml.XML_Data_Type;
      Dev_Attr   : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Node   : DOM.Core.Node);

   --  Assign IRQs to given device.
   procedure Assign_IRQs
     (Stream_Doc : Muxml.XML_Data_Type;
      Dev_Attr   : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Node   : DOM.Core.Node);

   --  Assign device memory to given device.
   procedure Assign_Memory
     (Stream_Doc : Muxml.XML_Data_Type;
      Dev_Attr   : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Node   : DOM.Core.Node);

   -------------------------------------------------------------------------

   procedure Assign_IO_Ports
     (Stream_Doc : Muxml.XML_Data_Type;
      Dev_Attr   : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Node   : DOM.Core.Node)
   is
      Ports : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Dev_Node,
           XPath => "ioPort");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Ports) - 1 loop
         declare
            Port : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Ports,
                 Index => I);
         begin
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "addIOPortRangeDevice",
               Attrs      => (Dev_Attr,
                              (Attr  => U ("from"),
                               Value => U
                                 (DOM.Core.Elements.Get_Attribute
                                    (Elem => Port,
                                     Name => "start"))),
                              (Attr  => U ("to"),
                               Value => U
                                 (DOM.Core.Elements.Get_Attribute
                                    (Elem => Port,
                                     Name => "end")))));
         end;
      end loop;
   end Assign_IO_Ports;

   -------------------------------------------------------------------------

   procedure Assign_IRQs
     (Stream_Doc : Muxml.XML_Data_Type;
      Dev_Attr   : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Node   : DOM.Core.Node)
   is
      IRQs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Dev_Node,
           XPath => "irq");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => IRQs) - 1 loop
         XML_Utils.Append_Command
           (Stream_Doc => Stream_Doc,
            Name       => "addIRQDevice",
            Attrs      => (Dev_Attr,
                           (Attr  => U ("irq"),
                            Value => U
                              (DOM.Core.Elements.Get_Attribute
                                 (Elem => DOM.Core.Nodes.Item
                                      (List  => IRQs,
                                       Index => I),
                                  Name => "number")))));
      end loop;
   end Assign_IRQs;

   -------------------------------------------------------------------------

   procedure Assign_Memory
     (Stream_Doc : Muxml.XML_Data_Type;
      Dev_Attr   : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Node   : DOM.Core.Node)
   is
      Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Dev_Node,
           XPath => "memory");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Memory,
                 Index => I);
            Caching : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Mem,
                 Name => "caching");
            Address : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Mem,
                 Name => "physicalAddress");
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Mem,
                    Name => "size"));
         begin
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "addMemoryDevice",
               Attrs      =>
                 (Dev_Attr,
                  (Attr  => U ("caching"),
                   Value => U (Caching)),
                  (Attr  => U ("address"),
                   Value => U (Address)),
                  (Attr  => U ("size"),
                   Value => U (Trim (Interfaces.Unsigned_64'Image
                     (Size / Mutools.Constants.Page_Size))))));
         end;
      end loop;
   end Assign_Memory;

   -------------------------------------------------------------------------

   procedure Create_Physical_Device
     (Dev_Node   :        DOM.Core.Node;
      Command    :        String;
      Attributes :        XML_Utils.Attribute_Array;
      Stream_Doc : in out Muxml.XML_Data_Type)
   is
   begin
      XML_Utils.Append_Command
        (Stream_Doc => Stream_Doc,
         Name       => Command,
         Attrs      => Attributes);

      Assign_IO_Ports
        (Stream_Doc => Stream_Doc,
         Dev_Attr   => Attributes (Attributes'First),
         Dev_Node   => Dev_Node);
      Assign_IRQs
        (Stream_Doc => Stream_Doc,
         Dev_Attr   => Attributes (Attributes'First),
         Dev_Node   => Dev_Node);
      Assign_Memory
        (Stream_Doc => Stream_Doc,
         Dev_Attr   => Attributes (Attributes'First),
         Dev_Node   => Dev_Node);

      XML_Utils.Append_Command
        (Stream_Doc => Stream_Doc,
         Name       => "activateDevice",
         Attrs      => (1 => Attributes (Attributes'First)));

      DOM.Core.Elements.Set_Attribute
        (Elem  => Dev_Node,
         Name  => "tau0DeviceId",
         Value => Trim (Current_Device'Img));
      Current_Device := Current_Device + 1;
   end Create_Physical_Device;

   -------------------------------------------------------------------------

   procedure Create_Physical_Legacy_Devices
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out Muxml.XML_Data_Type)
   is
      Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device[not(pci)]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Devs) - 1 loop
            Create_Physical_Device
              (Dev_Node   => DOM.Core.Nodes.Item
                 (List  => Devs,
                  Index => I),
               Command    => "createISADevice",
               Attributes => (1 => (Attr  => U ("device"),
                                    Value => U (Trim (Current_Device'Img)))),
               Stream_Doc => Stream_Doc);
      end loop;
   end Create_Physical_Legacy_Devices;

   -------------------------------------------------------------------------

   procedure Create_Physical_PCI_Devices
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out Muxml.XML_Data_Type)
   is
      Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device[pci]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Devs) - 1 loop
         declare
            Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Devs,
                 Index => I);
            PCI : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Dev,
                 XPath => "pci");
            Bus : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => PCI,
                 Name => "bus");
            Device : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => PCI,
                 Name => "device");
            Func : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => PCI,
                 Name => "function");
            MSI : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => PCI,
                 Name => "msi");
            Dev_Attr : constant XML_Utils.Attribute_Type
              := (Attr  => U ("device"),
                  Value => U (Trim (Current_Device'Img)));
         begin
            Create_Physical_Device
              (Dev_Node   => Dev,
               Command    => "createPCIDevice",
               Attributes => (Dev_Attr,
                              (Attr  => U ("bus"),
                               Value => U (Bus)),
                              (Attr  => U ("dev"),
                               Value => U (Device)),
                              (Attr  => U ("func"),
                               Value => U (Func)),
                              (Attr  => U ("usesMSI"),
                               Value => U (MSI))),
               Stream_Doc => Stream_Doc);
         end;
      end loop;
   end Create_Physical_PCI_Devices;

end Cmd_Stream.Devices;
