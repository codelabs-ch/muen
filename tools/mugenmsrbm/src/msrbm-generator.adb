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

with Ada.Streams.Stream_IO;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Interfaces;

with Mulog;
with Muxml.Utils;
with Mutools.Files;
with Mutools.Types;

with Msrbm.MSRs;

package body Msrbm.Generator
is

   --  Write MSR bitmap for given registers and write to specified file.
   procedure Write_MSR_Bitmap
     (Registers : DOM.Core.Node_List;
      Filename  : String);

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Subjects : DOM.Core.Node_List;
   begin
      Subjects := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject");

      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Cur_Subj  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Name      : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Cur_Subj,
                 Name => "name");
            Registers : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Cur_Subj,
                 XPath => "vcpu/registers/msrs/msr");
            Filename  : constant String
              := Output_Dir & "/" & Muxml.Utils.Get_Attribute
                (Doc   => Policy.Doc,
                 XPath => "/system/memory/memory[@type='system_msrbm' and "
                 & "contains(string(@name),'" & Name & "')]/file",
                 Name  => "filename");
         begin
            Mulog.Log (Msg => "Writing MSR bitmap of " & Name & " to '"
                       & Filename & "'");
            Write_MSR_Bitmap (Registers => Registers,
                              Filename  => Filename);
         end;
      end loop;
   end Write;

   -------------------------------------------------------------------------

   procedure Write_MSR_Bitmap
     (Registers : DOM.Core.Node_List;
      Filename  : String)
   is
      File   : Ada.Streams.Stream_IO.File_Type;
      Bitmap : MSRs.MSR_Bitmap_Type := MSRs.Null_MSR_Bitmap;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Registers) - 1 loop
         declare
            Cur_MSR     : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Registers,
               Index => I);
            Start_Addr  : constant Interfaces.Unsigned_32
              := Interfaces.Unsigned_32'Value
                (DOM.Core.Elements.Get_Attribute (Elem => Cur_MSR,
                                                  Name => "start"));
            End_Addr    : constant Interfaces.Unsigned_32
              := Interfaces.Unsigned_32'Value
                (DOM.Core.Elements.Get_Attribute (Elem => Cur_MSR,
                                                  Name => "end"));
            Access_Mode : constant Mutools.Types.MSR_Mode_Type
              := Mutools.Types.MSR_Mode_Type'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Cur_MSR,
                    Name => "mode"));
         begin
            MSRs.Allow_MSRs (Bitmap     => Bitmap,
                             Start_Addr => Start_Addr,
                             End_Addr   => End_Addr,
                             Mode       => Access_Mode);
         end;
      end loop;

      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      Ada.Streams.Stream_IO.Write (File => File,
                                   Item => MSRs.To_Stream (Bitmap => Bitmap));
      Ada.Streams.Stream_IO.Close (File => File);
   end Write_MSR_Bitmap;

end Msrbm.Generator;
