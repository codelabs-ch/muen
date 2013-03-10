-----------------------------------------------------------------------
--  disassemble -- Simple Disassembler
--  Copyright (C) 2006, 2012 Free Software Foundation, Inc.
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  This file is part of BfdAda.
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2,
--  or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, 51 Franklin Street - Fifth Floor,
--  Boston, MA 02110-1301, USA.  -->
-----------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Streams;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Directories;

with Bfd;
with Bfd.Files;
with Bfd.Sections;
with Bfd.Disassembler;
with Bfd.Symbols;

with Utils;
procedure Disassemble is

   procedure Disassemble_Section (File : in Bfd.Files.File_Type);

   --------------------------------------------------
   --  List the sections of the BFD file
   --------------------------------------------------
   procedure Disassemble_Section (File : in Bfd.Files.File_Type) is
      use type Bfd.Vma_Type;
      use Ada.Strings.Unbounded;

      Text_Section : constant Bfd.Sections.Section := Bfd.Sections.Find_Section (File, ".text");
      Size         : constant Ada.Streams.Stream_Element_Offset
        := Ada.Streams.Stream_Element_Offset (Text_Section.Size);
      Addr         : Bfd.Vma_Type := Text_Section.Vma;
      Section      : Ada.Streams.Stream_Element_Array (1 .. Size);
      Last         : Ada.Streams.Stream_Element_Offset;
      Info         : Utils.Small_Disassembler;
      Symbols      : Bfd.Symbols.Symbol_Table;
      Path         : Ada.Strings.Unbounded.Unbounded_String;
      Func         : Ada.Strings.Unbounded.Unbounded_String;
      Line         : Natural;
      Current_Name : Ada.Strings.Unbounded.Unbounded_String;
      Current_File : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Bfd.Symbols.Read_Symbols (File, Symbols);
      Bfd.Sections.Get_Section_Contents (File, Text_Section, 0, Section, Last);
      Bfd.Disassembler.Initialize (Info, File, "", Text_Section.Vma, Section);
      Info.Set_Symbol_Table (Symbols);
      loop
         Bfd.Symbols.Find_Nearest_Line (File, Text_Section, Symbols, Addr, Path, Func, Line);
         if Current_File /= Path then
            if Path /= "" then
               Ada.Text_IO.Put ("In '");
               Ada.Text_IO.Put (To_String (Path));
               Ada.Text_IO.Put_Line ("':");
               Current_Name := To_Unbounded_String (Ada.Directories.Simple_Name (To_String (Path)));
            else
               Current_Name := To_Unbounded_String ("");
            end if;
            Current_File := Path;
         end if;
         if Current_Name /= "" then
            Ada.Text_IO.Put (To_String (Current_Name));
            Ada.Text_IO.Put (":");
         end if;
         if Line > 0 then
            Utils.Print (Natural'Image (Line) & ":", 5);
         end if;
         if Func /= "" then
            Utils.Print ( "<" & To_String (Func) & ">", 20);
            Ada.Text_IO.Put (":");
         end if;

         Utils.Print (Utils.HexImage (Addr) & ":", 17);
         Bfd.Disassembler.Disassemble (Bfd.Disassembler.Memory_Disassembler_Info_Type'Class (Info),
                                       Addr, Addr);
         Ada.Text_IO.New_Line;
         exit when Addr >= Text_Section.Vma + Bfd.Vma_Type (Size);
      end loop;
   end Disassemble_Section;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;
   RC     : Ada.Command_Line.Exit_Status := 0;

begin
   Bfd.Set_Error_Program_Name (To => "disassemble");

   --  Open each file passed as argument and try dumping its
   --  sections and symbol table.
   for I in 1 .. Count loop
      declare
         Path : constant String := Ada.Command_Line.Argument (I);
         File : Bfd.Files.File_Type;
      begin
         Bfd.Files.Open (File, Path, "");
         if Bfd.Files.Check_Format (File, Bfd.Files.OBJECT) then
            Disassemble_Section (File);
         end if;
         Bfd.Files.Close (File);

      exception
         when Bfd.OPEN_ERROR =>
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Cannot open file '" & Path
                                  & "': " & Bfd.Get_Error_Message (Bfd.Get_Error));
            RC := 1;
      end;
   end loop;

   Ada.Command_Line.Set_Exit_Status (RC);
end Disassemble;
