--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Pack.Manifest;

package body Manifest_Tests
is

   use Ahven;
   use Pack;

   -------------------------------------------------------------------------

   procedure Add_Entries_To_Manifest
   is
      Fname : constant String := "obj/add_entries.manifest";
      Mf    : Manifest.Manifest_Type;
   begin
      Manifest.Add_Entry (Manifest => Mf,
                          Mem_Name => "some_name",
                          Format   => "some_format",
                          Content  => "testfile",
                          Address  => 16#100000#,
                          Size     => 16#1000#,
                          Offset   => 0);
      Manifest.Add_Entry (Manifest => Mf,
                          Mem_Name => "linux|acpi_rsdp",
                          Format   => "acpi_rsdp",
                          Content  => "data/sections.ref",
                          Address  => 16#101000#,
                          Size     => 16#13000#,
                          Offset   => 0);

      Manifest.Write (Manifest => Mf,
                      Filename => Fname);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Fname,
               Filename2 => "data/add_entries.manifest"),
              Message   => "Manifest mismatch");

      Ada.Directories.Delete_File (Name => Fname);
   end Add_Entries_To_Manifest;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Manifest tests");
      T.Add_Test_Routine
        (Routine => Add_Entries_To_Manifest'Access,
         Name    => "Add entries to manifest");
   end Initialize;

end Manifest_Tests;
