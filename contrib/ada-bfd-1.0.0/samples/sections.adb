-----------------------------------------------------------------------
--  sections -- Simple Example to list ELF sections
--  Copyright (C) 2002, 2003, 2004, 2012 Free Software Foundation, Inc.
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
--  the Free Software Foundation,51 Franklin Street - Fifth Floor,
--  Boston, MA 02110-1301, USA.
-----------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO;

with Bfd;
with Bfd.Files;
with Bfd.Sections;

with Utils;
procedure Sections is

   RC : Ada.Command_Line.Exit_Status := 0;

   procedure List_Section (File : in Bfd.Files.File_Type);

   --------------------------------------------------
   --  List the sections of the BFD file
   --------------------------------------------------
   procedure List_Section (File : in Bfd.Files.File_Type) is
      use type Bfd.Section_Flags;

      Iter : Bfd.Sections.Section_Iterator := Bfd.Sections.Get_Sections (File);
   begin
      Utils.Print ("Name", 30);
      Utils.Print ("Size", -10);
      Utils.Print ("VMA", -17);
      Utils.Print ("LMA", -17);
      Utils.Print ("Flags", -10);
      Ada.Text_IO.New_Line;
      Utils.Print ("====", 30);
      Utils.Print ("====", -10);
      Utils.Print ("===", -17);
      Utils.Print ("===", -17);
      Utils.Print ("=====", -10);
      Ada.Text_IO.New_Line;
      while Bfd.Sections.Has_Element (Iter) loop
         declare
            S   : constant Bfd.Sections.Section := Bfd.Sections.Element (Iter);
            P   : String (1 .. 6) := (others => ' ');
            Pos : Positive := 1;
         begin
            Utils.Print (Bfd.Sections.Get_Name (S), 30);
            Utils.Print (Bfd.Size_Type'Image (S.Size), -10);
            Utils.Print (Utils.HexImage (S.Vma), -17);
            Utils.Print (Utils.HexImage (S.Lma), -17);

            if (S.Flags and Bfd.Sections.SEC_ALLOC) /= 0 then
               P (Pos) := 'A';
               Pos := Pos + 1;
            end if;

            if (S.Flags and Bfd.Sections.SEC_LOAD) /= 0 then
               P (Pos) := 'L';
               Pos := Pos + 1;
            end if;

            if (S.Flags and Bfd.Sections.SEC_READONLY) /= 0 then
               P (Pos) := 'R';
               Pos := Pos + 1;
            end if;

            if (S.Flags and Bfd.Sections.SEC_DATA) /= 0 then
               P (Pos) := 'W';
               Pos := Pos + 1;
            end if;

            if (S.Flags and Bfd.Sections.SEC_CODE) /= 0 then
               P (Pos) := 'X';
               Pos := Pos + 1;
            end if;

            Utils.Print (P, -10);
            Ada.Text_IO.New_Line;
         end;
         Bfd.Sections.Next (Iter);
      end loop;
   end List_Section;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;

begin
   Bfd.Set_Error_Program_Name (To => "sections");

   --  Open each file passed as argument and try dumping its
   --  sections and symbol table.
   for I in 1 .. Count loop
      declare
         Path : constant String := Ada.Command_Line.Argument (I);
         File : Bfd.Files.File_Type;
      begin
         Bfd.Files.Open (File, Path, "");
         if Bfd.Files.Check_Format (File, Bfd.Files.OBJECT) then
            List_Section (File);
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
end Sections;
