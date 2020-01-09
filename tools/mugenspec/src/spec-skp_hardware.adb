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

with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Templates;

with String_Templates;

package body Spec.Skp_Hardware
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Tmpl : Mutools.Templates.Template_Type;

      --  Write port I/O device resources.
      procedure Write_Port_IO_Devices;

      ----------------------------------------------------------------------

      procedure Write_Port_IO_Devices
      is
         use Ada.Strings.Unbounded;

         Res : Unbounded_String;

         Phys_Devs    : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "/system/hardware/devices/device");
         Logical_Devs : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "/system/kernel/devices/device[ioPort]");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Logical_Devs) - 1 loop
            declare
               Logical_Dev : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Logical_Devs,
                    Index => I);
               Logical_Dev_Name : constant String
                 := Mutools.Utils.To_Ada_Identifier
                   (Str => DOM.Core.Elements.Get_Attribute
                      (Elem => Logical_Dev,
                       Name => "logical"));
               Logical_Memory : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Logical_Dev,
                    XPath => "memory");
               Logical_Ports : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Logical_Dev,
                    XPath => "ioPort");
               Phys_Dev_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Dev,
                    Name => "physical");
               Phys_Dev : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Phys_Devs,
                    Ref_Attr  => "name",
                    Ref_Value => Phys_Dev_Name);
               Caps : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Phys_Dev,
                    XPath => "capabilities/capability[text()]");
            begin
               Write_Ports :
               for J in 0 .. DOM.Core.Nodes.Length (List => Logical_Ports) - 1
               loop
                  declare
                     Logical_Port : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                         (List  => Logical_Ports,
                          Index => J);
                     Logical_Port_Name : constant String
                       := Mutools.Utils.To_Ada_Identifier
                         (Str => DOM.Core.Elements.Get_Attribute
                            (Elem => Logical_Port,
                             Name => "logical"));
                     Phys_Port_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Logical_Port,
                          Name => "physical");
                     Phys_Address : constant String
                       := Muxml.Utils.Get_Attribute
                         (Doc   => Phys_Dev,
                          XPath => "ioPort[@name='" & Phys_Port_Name & "']",
                          Name  => "start");
                  begin
                     Res := Res & Mutools.Utils.Indent & Logical_Dev_Name
                       & "_" & Logical_Port_Name & " : constant := "
                       & Phys_Address & ";" & ASCII.LF;
                  end;
               end loop Write_Ports;

               Write_Memory :
               for J in 0 .. DOM.Core.Nodes.Length (List => Logical_Memory) - 1
               loop
                  declare
                     Logical_Mem : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                         (List  => Logical_Memory,
                          Index => J);
                     Logical_Mem_Name : constant String
                       := Mutools.Utils.To_Ada_Identifier
                         (Str => DOM.Core.Elements.Get_Attribute
                            (Elem => Logical_Mem,
                             Name => "logical"));
                     Log_Address : constant String
                       := DOM.Core.Elements.Get_Attribute
                            (Elem => Logical_Mem,
                             Name => "virtualAddress");
                  begin
                     Res := Res & Mutools.Utils.Indent & Logical_Dev_Name
                       & "_" & Logical_Mem_Name & " : constant := "
                       & Log_Address & ";" & ASCII.LF;
                  end;
               end loop Write_Memory;

               Write_Caps :
               for J in 0 .. DOM.Core.Nodes.Length (List => Caps) - 1
               loop
                  declare
                     Cap : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                         (List  => Caps,
                          Index => J);
                     Cap_Name : constant String
                       := Mutools.Utils.To_Ada_Identifier
                         (Str => DOM.Core.Elements.Get_Attribute
                            (Elem => Cap,
                             Name => "name"));
                     Cap_Value : constant String
                       := Mutools.Utils.To_Hex
                         (Interfaces.Unsigned_64'Value
                            (DOM.Core.Nodes.Node_Value
                               (N => DOM.Core.Nodes.First_Child (N => Cap))));
                  begin
                     Res := Res & Mutools.Utils.Indent & Logical_Dev_Name
                       & "_" & Cap_Name & " : constant := "
                       & Cap_Value & ";" & ASCII.LF;
                  end;
               end loop Write_Caps;
            end;
         end loop;

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__port_io_devices__",
            Content  => To_String (Res));
      end Write_Port_IO_Devices;
   begin
      Mulog.Log (Msg => "Writing hardware spec to '"
                 & Output_Dir & "/skp-hardware.ads'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_hardware_ads);

      Write_Port_IO_Devices;

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-hardware.ads");
   end Write;

end Spec.Skp_Hardware;
