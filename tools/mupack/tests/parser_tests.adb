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
        := (1 => (Name    => U ("linux|acpi_rsdp"),
                  Path    => U ("sections.ref"),
                  Address => 16#0010_0000#,
                  Size    => 16#0001_3000#,
                  Offset  => 0,
                  Format  => Parser.Acpi_Rsdp),
            2 => (Name    => U ("linux|bin"),
                  Path    => U ("obj1.o"),
                  Address => 16#0011_3000#,
                  Size    => 16#0001_3000#,
                  Offset  => 16#0001_b000#,
                  Format  => Parser.Elf));
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

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Parser tests");
      T.Add_Test_Routine
        (Routine => Extract_Files'Access,
         Name    => "Extract file information");
   end Initialize;

end Parser_Tests;
