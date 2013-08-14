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

with Ada.Directories;
with Ada.Exceptions;

with Skc.Subjects;

with Test_Utils;

package body Binary_Tests
is

   use Ahven;
   use Skc;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Binary tests");
      T.Add_Test_Routine
        (Routine => Read_Binary'Access,
         Name    => "Read binary information");
      T.Add_Test_Routine
        (Routine => Read_Nonexistent_File'Access,
         Name    => "Read nonexistent file");
      T.Add_Test_Routine
        (Routine => Read_Invalid_File'Access,
         Name    => "Read invalid file");
      T.Add_Test_Routine
        (Routine => Read_No_Symbols'Access,
         Name    => "Read file without symbols");
      T.Add_Test_Routine
        (Routine => Read_No_Global_Symbols'Access,
         Name    => "Read file without global symbols");
      T.Add_Test_Routine
        (Routine => Read_Undefined_Symbols'Access,
         Name    => "Read file with undefined symbols");
      T.Add_Test_Routine
        (Routine => Write_Binary_Spec'Access,
         Name    => "Write binary XML specification");
      T.Add_Test_Routine
        (Routine => Write_Memlayout_Spec'Access,
         Name    => "Write memory layout XML specification");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Read_Binary
   is
      use type Skc.Subjects.Binary_Type;

      Ref_Bin : constant Subjects.Binary_Type
        := (Entry_Point   => 16#0c#,
            Stack_Address => 16#18#);
      Bin     : Subjects.Binary_Type;
   begin
      Bin := Subjects.Read (Binary => "data/bin1");

      Assert (Condition => Bin = Ref_Bin,
              Message   => "Binary info mismatch");
   end Read_Binary;

   -------------------------------------------------------------------------

   procedure Read_Invalid_File
   is
      Bin : Subjects.Binary_Type;
      pragma Unreferenced (Bin);
   begin
      Bin := Subjects.Read (Binary => "data/bin1.S");
      Fail (Message => "Exception expected");

   exception
      when E : Subjects.Binary_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = "File 'data/bin1.S' has invalid format",
                 Message   => "Exception message mismatch");
   end Read_Invalid_File;

   -------------------------------------------------------------------------

   procedure Read_No_Global_Symbols
   is
      Bin : Subjects.Binary_Type;
      pragma Unreferenced (Bin);
   begin
      Bin := Subjects.Read (Binary => "data/noglobalentrysymbol");
      Fail (Message => "Exception expected");

   exception
      when E : Subjects.Binary_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = "subject_entry is not a global symbol",
                 Message   => "Exception message mismatch");
   end Read_No_Global_Symbols;

   -------------------------------------------------------------------------

   procedure Read_No_Symbols
   is
      Bin : Subjects.Binary_Type;
      pragma Unreferenced (Bin);
   begin
      begin
         Bin := Subjects.Read (Binary => "data/noentrysymbol");
         Fail (Message => "Exception expected");

      exception
         when E : Subjects.Binary_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message
                    (X => E) = "No subject_entry symbol",
                    Message   => "Exception message mismatch");
      end;

      begin
         Bin := Subjects.Read (Binary => "data/nostacksymbol");
         Fail (Message => "Exception expected");

      exception
         when E : Subjects.Binary_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message
                    (X => E) = "No stack symbol",
                    Message   => "Exception message mismatch");
      end;
   end Read_No_Symbols;

   -------------------------------------------------------------------------

   procedure Read_Nonexistent_File
   is
      Bin : Subjects.Binary_Type;
      pragma Unreferenced (Bin);
   begin
      Bin := Subjects.Read (Binary => "nonexistent");
      Fail (Message => "Exception expected");

   exception
      when Subjects.Open_Error => null;
   end Read_Nonexistent_File;

   -------------------------------------------------------------------------

   procedure Read_Undefined_Symbols
   is
      Bin : Subjects.Binary_Type;
      pragma Unreferenced (Bin);
   begin
      Bin := Subjects.Read (Binary => "data/undefentry");
      Fail (Message => "Exception expected");

   exception
      when E : Subjects.Binary_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = "Undefined symbol subject_entry",
                 Message   => "Exception message mismatch");
   end Read_Undefined_Symbols;

   -------------------------------------------------------------------------

   procedure Write_Binary_Spec
   is
      XML_Spec : constant String := "obj/bin1.xml";
   begin
      Subjects.Write
        (XML_File => XML_Spec,
         Subject  => (Entry_Point   => 16#0c#,
                      Stack_Address => 16#18#));

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/bin1.xml.ref",
               Filename2 => XML_Spec),
              Message   => "XML specification mismatch");

      Ada.Directories.Delete_File (Name => XML_Spec);
   end Write_Binary_Spec;

   -------------------------------------------------------------------------

   procedure Write_Memlayout_Spec
   is
      XML_Spec : constant String := "obj/bin1_mem.xml";
   begin
      Subjects.Write_Memory_Layout
        (XML_File      => XML_Spec,
         Binary        => "data/bin1",
         Start_Address => 16#216000#);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/bin1_mem.xml.ref",
               Filename2 => XML_Spec),
              Message   => "XML specification mismatch");

      Ada.Directories.Delete_File (Name => XML_Spec);
   end Write_Memlayout_Spec;

end Binary_Tests;
