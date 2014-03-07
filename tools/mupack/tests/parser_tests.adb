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

with Interfaces;

with Pack.Parser;

package body Parser_Tests
is

   use Ahven;
   use Pack;

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   procedure Extract_Files
   is
      use type Parser.File_Array;

      Ref_Files : constant Parser.File_Array
        := (1 => (Mem_Name => U ("linux|acpi_rsdp"),
                  Filename => U ("sections.ref"),
                  Path     => Ada.Strings.Unbounded.Null_Unbounded_String,
                  Address  => 16#0010_0000#,
                  Size     => 16#0001_3000#,
                  Offset   => 0,
                  Format   => Parser.Acpi_Rsdp),
            2 => (Mem_Name => U ("linux|bin"),
                  Filename => U ("obj1.o"),
                  Path     => Ada.Strings.Unbounded.Null_Unbounded_String,
                  Address  => 16#0011_3000#,
                  Size     => 16#0001_3000#,
                  Offset   => 4,
                  Format   => Parser.Elf));
   begin
      declare
         Files : constant Parser.File_Array
           := Parser.Parse (Policy => "data/test_policy.xml");
      begin
         Assert (Condition => Files'Length = 2,
                 Message   => "2 files expected");
         Assert (Condition => Files = Ref_Files,
                 Message   => "Files mismatch");
      end;
   end Extract_Files;

   -------------------------------------------------------------------------

   procedure Image_Size
   is
      use type Interfaces.Unsigned_64;

      Empty : constant Parser.File_Array (1 .. 0) := (others => <>);
      Files : constant Parser.File_Array
        := (1 => (Mem_Name => U ("test1"),
                  Filename => U ("sections.ref"),
                  Path     => U ("data"),
                  Address  => 16#0000_4000#,
                  Size     => 16#0000_3000#,
                  Offset   => 0,
                  Format   => Parser.Acpi_Rsdp),
            2 => (Mem_Name => U ("test2"),
                  Filename => U ("obj1.o"),
                  Path     => U ("data"),
                  Address  => 16#0000_1000#,
                  Size     => 16#0000_7000#,
                  Offset   => 16#0001_b000#,
                  Format   => Parser.Elf));
   begin
      Assert (Condition => Parser.Get_Image_Size (Files => Empty) = 0,
              Message   => "Zero expected");
      Assert (Condition => Parser.Get_Image_Size (Files => Files) = 16#8000#,
              Message   => "Image size mismatch");
   end Image_Size;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Parser tests");
      T.Add_Test_Routine
        (Routine => Extract_Files'Access,
         Name    => "Extract file information");
      T.Add_Test_Routine
        (Routine => Image_Size'Access,
         Name    => "Calculate image size");
   end Initialize;

end Parser_Tests;
