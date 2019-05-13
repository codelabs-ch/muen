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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Cmd_Stream.XML_Utils;

package body Cmd_Stream.Roots.Devices
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   --  Append IRQs to given device.
   procedure Append_IRQs
     (Stream_Doc : Muxml.XML_Data_Type;
      Dev_Attr   : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Node   : DOM.Core.Node);

   -------------------------------------------------------------------------

   procedure Append_IRQs
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
   end Append_IRQs;

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
                  Value => U (Ada.Strings.Fixed.Trim
                    (Source => I'Img,
                     Side   => Ada.Strings.Left)));
         begin
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "createPCIDevice",
               Attrs      => (Dev_Attr,
                              (Attr  => U ("bus"),
                               Value => U (Bus)),
                              (Attr  => U ("dev"),
                               Value => U (Device)),
                              (Attr  => U ("func"),
                               Value => U (Func)),
                              (Attr  => U ("usesMSI"),
                               Value => U (MSI))));

            Append_IRQs (Stream_Doc => Stream_Doc,
                         Dev_Attr   => Dev_Attr,
                         Dev_Node   => Dev);

            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "activateDevice",
               Attrs      => (1 => Dev_Attr));
         end;
      end loop;
   end Create_Physical_PCI_Devices;

end Cmd_Stream.Roots.Devices;
