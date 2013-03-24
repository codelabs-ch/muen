-----------------------------------------------------------------------
--  BFD Tests -- Tests for BFD section Ada API
--  Copyright (C) 2002, 2003, 2012 Free Software Foundation, Inc.
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
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Streams;

with Bfd.Files;
with Bfd.Sections;
with Bfd.Constants;
package body Bfd.Sections.Tests is

   use Ada.Strings.Unbounded;

   --  --------------------
   --  Test basic sections operations
   --  --------------------
   procedure Test_Sections (T : in out Test_Case) is
      S    : Section_Iterator;

      Has_Code : Boolean := False;
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      S := Get_Sections (T.File.all);
      T.Assert (Has_Element (S),
                "Bfd.Create_Iterator returned null section iterator");

      while Has_Element (S) loop
         declare
            Sec : constant Section := Element (S);
         begin
            T.Assert (Get_Name (Sec)'Length > 0,
                      "Bfd.Sections.Get_Name returned empty name");

            if (Sec.Flags and SEC_CODE) /= 0 then
               T.Assert ((Sec.Flags and SEC_LOAD) /= 0,
                         "SEC_CODE is set but SEC_LOAD is not");
               Has_Code := True;
            end if;
         end;
         Next (S);
      end loop;
      T.Assert (Has_Code, "No SEC_CODE section found");
   end Test_Sections;


   --  --------------------
   --  Test find sections operations
   --  --------------------
   procedure Test_Find_Section (T : in out Test_Case) is
      Sec  : Section;
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      --  Check that Find_Section raises an exception when the section
      --  is not known.
      begin
         Sec := Find_Section (T.File.all, "toto section");

         T.Assert (False, "Bfd.Find_Section didn't raise an exception");
      exception
         when Bfd.NOT_FOUND =>
            null;
      end;

      --  Verify that Find_Section returns the good sections.
      declare
         It : Section_Iterator := Get_Sections (T.File.all);
         S  : Section;
      begin
         T.Assert (Has_Element (It),
                   "Bfd.Create_Iterator returned a null section");

         while Has_Element (It) loop
            S := Element (It);
            Sec := Find_Section (T.File.all, Get_Name (S));
            T.Assert (Sec = S, "Bfd.Find_Section returned a different section");
            Next (It);
         end loop;
      end;
   end Test_Find_Section;


   --  --------------------
   --  Test get section content operations
   --  --------------------
   procedure Test_Get_Section_Contents (T : in out Test_Case) is
      Sec  : Section;
      It   : Section_Iterator;
      Read_Something : Boolean := False;
   begin
      T.Assert (Bfd.Files.Check_Format (T.File.all, Bfd.Files.OBJECT),
                "Bfd.Check_Format returned false");

      It := Get_Sections (T.File.all);
      T.Assert (Has_Element (It),
                "Bfd.Create_Iterator returned a null section");

      --  Scan each section and load its content in memory.
      while Has_Element (It) loop
         Sec := Element (It);
         if Sec.Size /= 0 and (Sec.Flags and SEC_HAS_CONTENTS) /= 0 then
            declare
               use Ada.Streams;

               Cnt  : constant Stream_Element_Offset := Stream_Element_Offset (Sec.Size);
               Buf  : Stream_Element_Array (1 .. Cnt) := (others => 0);
               Last : Stream_Element_Offset;
               Seems_Filled : Boolean := False;
            begin
               --  Get section content in buffer.
               Get_Section_Contents (T.File.all, Sec, 0, Buf, Last);
               Util.Tests.Assert_Equals (T, Integer (Last), Integer (Cnt),
                                        "Cannot get content of section " & Get_Name (Sec));

               if Util.Tests.Verbose then
                  Ada.Text_IO.Put_Line ("Read content of " & Get_Name (Sec)
                                        & " " & Stream_Element_Offset'Image (Last)
                                        & " bytes read");
               end if;

               --  Crude test to check we got something in.
               if Get_Name (Sec) = ".text" then
                  for I in Buf'First .. Last loop
                     if Buf (I) /= 0 then
                        Seems_Filled := True;
                        exit;
                     end if;
                  end loop;
                  T.Assert (Seems_Filled,
                            "Section " & Get_Name (Sec) & " contains all 0");
               end if;
               Read_Something := True;
            end;
         end if;
         Next (It);
      end loop;

      --  Be sure we loaded some section.
      T.Assert (Read_Something, "No section was loaded");
   end Test_Get_Section_Contents;

   --  --------------------
   --  Add the tests in the testsuite
   --  --------------------
   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is

      procedure Add_Test (Test_Name : in String;
                          File_Name : in String;
                          Method    : in Bfd.Tests.Test_Method_Access);

      procedure Add_Test (Test_Name : in String;
                          File_Name : in String;
                          Method    : in Bfd.Tests.Test_Method_Access) is
         T : constant Bfd.Tests.Test_Case_Access := new Test_Case;
      begin
         T.File_Name := To_Unbounded_String (File_Name);
         T.Test_Name := To_Unbounded_String (Test_Name);
         T.Method    := Method;
         Suite.Add_Test (T.all'Access);
      end Add_Test;
   begin
      Add_Test ("Test Bfd.Sections.Create_Iterator/Is_Done/Next",
                "obj/bfd-tests.o", Test_Sections'Access);
      Add_Test ("Test Bfd.Sections.Find_Section",
                "obj/bfd-tests.o", Test_Find_Section'Access);
      Add_Test ("Test Bfd.Sections.Get_Section_Contents",
                "obj/bfd-tests.o", Test_Get_Section_Contents'Access);
   end Add_Tests;

end Bfd.Sections.Tests;
