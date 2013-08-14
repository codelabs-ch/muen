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

with Test_Utils;

with Pack.OS;
with Pack.Image;

package body Image_Tests
is

   use Ahven;
   use Pack;

   -------------------------------------------------------------------------

   procedure Add_Section_To_Elf
   is
      Elf  : constant String := "obj/obj.o";
      Dump : constant String := "obj/sections";
   begin
      Ada.Directories.Copy_File (Source_Name => "data/obj1.o",
                                 Target_Name => Elf);

      Image.Add_Section (Image    => Elf,
                         Filename => "data/obj2.o",
                         Name     => "obj2",
                         Address  => 16#1000#);
      OS.Execute (Command => "objdump -h " & Elf & " > " & Dump);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Dump,
               Filename2 => "data/sections.ref"),
              Message   => "Sections mismatch");

      Ada.Directories.Delete_File (Name => Dump);
      Ada.Directories.Delete_File (Name => Elf);
   end Add_Section_To_Elf;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Image package tests");
      T.Add_Test_Routine
        (Routine => Add_Section_To_Elf'Access,
         Name    => "Add section to ELF");
   end Initialize;

end Image_Tests;
