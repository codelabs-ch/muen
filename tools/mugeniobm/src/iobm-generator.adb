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

with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Interfaces;

with Mulog;
with Muxml.Utils;
with Mutools.Files;

with Iobm.IO_Ports;

package body Iobm.Generator
is

   use Ada.Strings.Unbounded;

   --  Write I/O bitmap for given port nodes to specified file.
   procedure Write_IO_Bitmap
     (Policy   : Muxml.XML_Data_Type;
      Ports    : DOM.Core.Node_List;
      Filename : String);

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Phys_Mem :  constant DOM.Core.Node_List
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
            Cur_Subj : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Name     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Cur_Subj,
                 Name => "name");
            Ports    : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Cur_Subj,
                 XPath => "devices/device/ioPort");
            Iobm_Mem : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
             (Nodes => Phys_Mem,
              Refs  => ((Name  => To_Unbounded_String ("type"),
                         Value => To_Unbounded_String ("system_iobm")),
                        (Name  => To_Unbounded_String ("name"),
                         Value => To_Unbounded_String (Name & "|iobm"))));
            Filename : constant String
              := Output_Dir & "/" & Muxml.Utils.Get_Attribute
                (Doc   => Iobm_Mem,
                 XPath => "file",
                 Name  => "filename");
         begin
            Mulog.Log (Msg => "Writing I/O bitmap of subject '" & Name
                       & "' to '" & Filename & "'");
            Write_IO_Bitmap (Policy   => Policy,
                             Ports    => Ports,
                             Filename => Filename);
         end;
      end loop;
   end Write;

   -------------------------------------------------------------------------

   procedure Write_IO_Bitmap
     (Policy   : Muxml.XML_Data_Type;
      Ports    : DOM.Core.Node_List;
      Filename : String)
   is
      File   : Ada.Streams.Stream_IO.File_Type;
      Bitmap : Iobm.IO_Ports.IO_Bitmap_Type := Iobm.IO_Ports.Null_IO_Bitmap;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Ports) - 1 loop
         declare
            Cur_Node   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Ports,
                 Index => I);
            Phys_Name  :  constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Cur_Node,
                 Name => "physical");
            Dev_Name   :  constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Cur_Node),
                 Name => "physical");
            Phys_Port  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Policy.Doc,
                 XPath => "/system/hardware/devices/device[@name='" & Dev_Name
                 & "']/ioPort[@name='" & Phys_Name & "']");
            Start_Port : constant Interfaces.Unsigned_16
              := Interfaces.Unsigned_16'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Port,
                    Name => "start"));
            End_Port   : constant Interfaces.Unsigned_16
              := Interfaces.Unsigned_16'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Port,
                    Name => "end"));
         begin
            Iobm.IO_Ports.Allow_Ports
              (B          => Bitmap,
               Start_Port => Start_Port,
               End_Port   => End_Port);
         end;
      end loop;

      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      Ada.Streams.Stream_IO.Write (File => File,
                                   Item => Iobm.IO_Ports.To_Stream
                                     (B => Bitmap));
      Ada.Streams.Stream_IO.Close (File => File);
   end Write_IO_Bitmap;

end Iobm.Generator;
