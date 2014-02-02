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

with Ada.Directories;

with Test_Utils;

with Muxml;

with Acpi.Generator;

package body Generator_Tests
is

   use Ahven;
   use Acpi;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Generator tests");
      T.Add_Test_Routine
        (Routine => Write_ACPI_Tables'Access,
         Name    => "Write ACPI tables");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_ACPI_Tables
   is
      Policy : Muxml.XML_Data_Type;

      Linux_RSDP : constant String := "obj/linux_rsdp";
      Linux_XSDT : constant String := "obj/linux_xsdt";
      Linux_FADT : constant String := "obj/linux_fadt";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Generator.Write (Output_Dir => "obj",
                       Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/linux_rsdp.ref",
               Filename2 => Linux_RSDP),
              Message => "RSDP table mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/linux_xsdt.ref",
               Filename2 => Linux_XSDT),
              Message => "XSDT table mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/linux_fadt.ref",
               Filename2 => Linux_FADT),
              Message => "FACP table mismatch");

      Ada.Directories.Delete_File (Name => Linux_RSDP);
      Ada.Directories.Delete_File (Name => Linux_XSDT);
      Ada.Directories.Delete_File (Name => Linux_FADT);
   end Write_ACPI_Tables;

end Generator_Tests;
