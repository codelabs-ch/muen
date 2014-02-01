--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Exceptions;
with Ada.Text_IO.Text_Streams;

with DOM.Core.Nodes;
with Schema.Dom_Readers;
with Schema.Validators;
with Input_Sources.File;
with Sax.Readers;

with Muxml.Grammar;

package body Muxml
is

   package DR renames Schema.Dom_Readers;
   package SV renames Schema.Validators;

   -------------------------------------------------------------------------

   procedure Finalize (Object : in out XML_Data_Type)
   is
   begin
      DOM.Core.Nodes.Free (N => Object.Doc);
   end Finalize;

   -------------------------------------------------------------------------

   procedure Parse
     (Data : out XML_Data_Type;
      Kind :     Schema_Kind;
      File :     String)
   is
      Reader     : DR.Tree_Reader;
      File_Input : Input_Sources.File.File_Input;
   begin
      Reader.Set_Grammar (Grammar => Grammar.Get_Grammar (Kind));
      Reader.Set_Feature (Name  => Sax.Readers.Schema_Validation_Feature,
                          Value => True);

      begin
         Input_Sources.File.Open (Filename => File,
                                  Input    => File_Input);

         begin
            Reader.Parse (Input => File_Input);

         exception
            when others =>
               Input_Sources.File.Close (Input => File_Input);
               Data.Doc := Reader.Get_Tree;
               Reader.Free;
               raise;
         end;

         Input_Sources.File.Close (Input => File_Input);
         Data.Doc := Reader.Get_Tree;

      exception
         when SV.XML_Validation_Error =>
            raise Processing_Error with "XML processing error - "
              & Reader.Get_Error_Message;
         when E : others =>
            raise Processing_Error with "Error reading XML file '" & File
              & "' - " & Ada.Exceptions.Exception_Message (X => E);
      end;
   end Parse;

   -------------------------------------------------------------------------

   procedure Write
     (Data : XML_Data_Type;
      Kind : Schema_Kind;
      File : String)
   is
      use Ada.Text_IO;
      use Ada.Text_IO.Text_Streams;

      Output_File   : File_Type;
      pragma Unreferenced (Kind);
   begin
      Create (Output_File, Out_File, File);
      DOM.Core.Nodes.Write
         (Stream       => Stream (Output_File),
          N            => Data.Doc,
          Pretty_Print => True);
      Close (Output_File);
   end Write;

end Muxml;
