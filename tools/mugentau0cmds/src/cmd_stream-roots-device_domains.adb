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

with Paging;

with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;

with Cmd_Stream.XML_Utils;
with Cmd_Stream.Roots.Utils;

package body Cmd_Stream.Roots.Device_Domains
is

   --  Assign devices to given device domain.
   procedure Assign_Devices
     (Stream_Doc    : Muxml.XML_Data_Type;
      Dom_Attr      : Cmd_Stream.XML_Utils.Attribute_Type;
      Physical_Devs : DOM.Core.Node_List;
      Logical_Devs  : DOM.Core.Node_List);

   -------------------------------------------------------------------------

   procedure Assign_Devices
     (Stream_Doc    : Muxml.XML_Data_Type;
      Dom_Attr      : Cmd_Stream.XML_Utils.Attribute_Type;
      Physical_Devs : DOM.Core.Node_List;
      Logical_Devs  : DOM.Core.Node_List)
   is
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Logical_Devs) - 1 loop
         declare
            Logical_Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Logical_Devs,
                 Index => I);
            Physical_Dev : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Devs,
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
               Name       => "addDeviceToDeviceDomain",
               Attrs      => (Dom_Attr, Dev_Attr));
         end;
      end loop;
   end Assign_Devices;

   -------------------------------------------------------------------------

   procedure Create
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out Muxml.XML_Data_Type;
      Phys_Mem   :        DOM.Core.Node_List;
      Phys_Devs  :        DOM.Core.Node_List)
   is
      Domains : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/deviceDomains/domain");
      Paging_Lvls : constant Paging.Paging_Level
        := Mutools.XML_Utils.Get_IOMMU_Paging_Levels (Data => Policy);
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Domains) - 1 loop
         declare
            Domain : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Domains,
                 Index => I);
            Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Domain,
                 Name => "name");
            DID : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Domain,
                    Name => "id"));
            Root_ID : constant Natural := Allocate_Root;
            Dom_Attr : constant XML_Utils.Attribute_Type
              := (Attr  => U ("domain"),
                  Value => U (Trim (Root_ID'Img)));
            Empty_List : DOM.Core.Node_List;
         begin
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "createDeviceDomain",
               Attrs      => (Dom_Attr,
                              (Attr  => U ("id"),
                               Value => U (Mutools.Utils.To_Hex
                                 (Number => DID))),
                              (Attr  => U ("level"),
                               Value => U (Trim (Natural'Image
                                 (Paging_Lvls + 1))))));

            Assign_Devices
              (Stream_Doc => Stream_Doc,
               Dom_Attr   => Dom_Attr,
               Physical_Devs => Phys_Devs,
               Logical_Devs  => McKae.XML.XPath.XIA.XPath_Query
                 (N     => Domain,
                  XPath => "devices/device"));
            Utils.Assign_Memory
              (Stream_Doc    => Stream_Doc,
               Physical_Mem  => Phys_Mem,
               Physical_Devs => Empty_List,
               Logical_Mem   => McKae.XML.XPath.XIA.XPath_Query
                 (N     => Domain,
                  XPath => "memory/memory"),
               Logical_Devs  => Empty_List,
               Object_Attr   => Dom_Attr,
               Object_Kind   => "DeviceDomain",
               Entity_Name   => "vtd_" & Name & "_pt",
               Paging_Levels => Paging_Lvls);

            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "activateDeviceDomain",
               Attrs      => (1 => Dom_Attr));
         end;
      end loop;
   end Create;

end Cmd_Stream.Roots.Device_Domains;
