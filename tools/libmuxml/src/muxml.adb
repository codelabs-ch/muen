--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  Copyright (C) 2014        Alexander Senier <mail@senier.net>
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
with Input_Sources.Strings.Local;
with Sax.Readers;
with Unicode.CES.Utf8;
with DOM;
with Muxml.Grammar;
with Schema.Dom_Readers_With_Location;

package body Muxml
is

   package DR renames Schema.Dom_Readers;
   package SV renames Schema.Validators;

   --  Parse the contents of given XML input source into the DOM data
   --  structure. The XML data is validated against the built-in system policy
   --  XML schema.
   procedure Parse
     (Data         :    out XML_Data_Type;
      Input        : in out Input_Sources.Input_Source'Class;
      Kind         :        Schema_Kind;
      Add_Location :        Boolean := False);

   -------------------------------------------------------------------------

   procedure Finalize (Object : in out XML_Data_Type)
   is
   begin
      DOM.Core.Nodes.Free (N => Object.Doc);
   end Finalize;

   -------------------------------------------------------------------------

   procedure Parse
     (Data         :    out XML_Data_Type;
      Input        : in out Input_Sources.Input_Source'Class;
      Kind         :        Schema_Kind;
      Add_Location :        Boolean := False)
   is
      Reader_with_L : Schema.Dom_Readers_With_Location.Tree_Reader_With_Location;
      Reader_no_L   : DR.Tree_Reader;

      ----------------------------------------------------------------------

      -- do the actual parsing, depending on the actual type of Reader
      procedure Do_Parse (Reader : in out DR.Tree_Reader'Class);

      ----------------------------------------------------------------------

      procedure Do_Parse (Reader : in out DR.Tree_Reader'Class)
      is
      begin
         if Kind in Valid_Schema_Kind then
            Reader.Set_Grammar (Grammar => Grammar.Get_Grammar (Kind));
            Reader.Set_Feature (Name  => Sax.Readers.Schema_Validation_Feature,
                                Value => True);
         else
            Reader.Set_Feature (Name  => Sax.Readers.Schema_Validation_Feature,
                                Value => False);
         end if;

         begin
            Reader.Parse (Input => Input);
         exception
            when others =>
               Input.Close;
               Data.Doc := Reader.Get_Tree;
               Reader.Free;
               raise;
         end;

         Input.Close;
         Data.Doc := Reader.Get_Tree;

      exception
         when SV.XML_Validation_Error =>
            raise Validation_Error with "XML validation error - "
               & Reader.Get_Error_Message;
         when E : others =>
            raise Validation_Error with "Error validating XML data - "
               & Ada.Exceptions.Exception_Message (X => E);
      end Do_Parse;

   begin
      if Add_Location then
         Do_Parse (Reader => Reader_with_L);
      else
         Do_Parse (Reader => Reader_no_L);
      end if;
   end Parse;

   -------------------------------------------------------------------------

   procedure Parse
     (Data         : out XML_Data_Type;
      Kind         :     Schema_Kind;
      File         :     String;
      Add_Location :     Boolean := False)
   is
      File_Input : Input_Sources.File.File_Input;
   begin
      begin
         Input_Sources.File.Open (Filename => File,
                                  Input    => File_Input);

      exception
         when E : others =>
            raise XML_Input_Error with "Error reading XML file '" & File
              & "' - " & Ada.Exceptions.Exception_Message (X => E);
      end;

      Parse (Data         => Data,
             Input        => File_Input,
             Kind         => Kind,
             Add_Location => Add_Location);
   end Parse;

   -------------------------------------------------------------------------

   procedure Parse_String
     (Data : out XML_Data_Type;
      Kind :     Schema_Kind;
      XML  :     String)
   is
      Str_Input : Input_Sources.Strings.String_Input;
   begin
      begin
         Input_Sources.Strings.Local.Open
           (Str      => XML,
            Encoding => Unicode.CES.Utf8.Utf8_Encoding,
            Input    => Str_Input);

      exception
         when E : others =>
            raise XML_Input_Error with "Error reading XML string - "
              & Ada.Exceptions.Exception_Message (X => E);
      end;

      Parse (Data  => Data,
             Input => Str_Input,
             Kind  => Kind);
   end Parse_String;

   -------------------------------------------------------------------------

   procedure Write
     (Data : XML_Data_Type;
      Kind : Schema_Kind;
      File : String)
   is
      use Ada.Text_IO;
      use Ada.Text_IO.Text_Streams;

      Output_File : File_Type;
   begin
      Create (Output_File, Out_File, File);
      DOM.Core.Nodes.Write
        (Stream       => Stream (Output_File),
         N            => Data.Doc,
         Pretty_Print => True);
      Close (Output_File);

      if Kind in Valid_Schema_Kind then
         declare
            Unused_Data : XML_Data_Type;
         begin
            Parse (Data => Unused_Data,
                   Kind => Kind,
                   File => File);
         end;
      end if;
   end Write;

end Muxml;
