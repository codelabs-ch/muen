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
with Mutools.Utils;

with Cmd_Stream.XML_Utils;

package body Cmd_Stream.Roots.Subjects
is

   --  Assign devices to given subject.
   procedure Assign_Devices
     (Stream_Doc : Muxml.XML_Data_Type;
      Policy     : Muxml.XML_Data_Type;
      Subj_Attr  : Cmd_Stream.XML_Utils.Attribute_Type;
      Subj_Node  : DOM.Core.Node);

   --  Assign I/O ports of device to given subject.
   procedure Assign_IO_Ports
     (Stream_Doc   : Muxml.XML_Data_Type;
      Subj_Attr    : Cmd_Stream.XML_Utils.Attribute_Type;
      Dev_Attr     : Cmd_Stream.XML_Utils.Attribute_Type;
      Logical_Dev  : DOM.Core.Node;
      Physical_Dev : DOM.Core.Node);

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
            package MC renames Mutools.Constants;

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
                              (Attr  => U ("policy"),
                               Value => U ("0")), -- TODO: Remove or fix
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
         end;
      end loop;
   end Create_Subjects;

end Cmd_Stream.Roots.Subjects;
