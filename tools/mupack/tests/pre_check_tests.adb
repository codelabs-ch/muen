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

with Ada.Exceptions;

with Muxml;
with Mutools.XML_Utils;

with Pack.Command_Line.Test;
with Pack.Pre_Checks;

package body Pre_Check_Tests
is

   use Ahven;
   use Pack;

   -------------------------------------------------------------------------

   procedure File_Existence
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Command_Line.Test.Set_Input_Dir (Path => "data");
      Command_Line.Test.Set_Output_Dir (Path => "obj");
      Command_Line.Test.Set_Policy (Path => "data/test_policy.xml");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Must not raise an exception.

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "mboot",
         Address     => "16#0010_0000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "system",
         File_Name   => "mboot",
         File_Format => "bin_raw",
         File_Offset => "none");
      Pre_Checks.Files_Exist (Data => Policy);

      --  Add entry with invalid filename.

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "linux|acpi_rsdp",
         Address     => "16#0010_0000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_acpi_rsdp",
         File_Name   => "nonexistent",
         File_Format => "bin_raw",
         File_Offset => "none");

      begin
         Pre_Checks.Files_Exist (Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Pre_Checks.Check_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "File 'data/nonexistent' referenced by physical memory "
                    & "region 'linux|acpi_rsdp' not found",
                    Message   => "Exception mismatch");
      end;
   end File_Existence;

   -------------------------------------------------------------------------

   procedure File_Larger_Than_Memory
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Command_Line.Test.Set_Input_Dir (Path => "data");
      Command_Line.Test.Set_Output_Dir (Path => "obj");
      Command_Line.Test.Set_Policy (Path => "data/test_policy.xml");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "linux|acpi_rsdp",
         Address     => "16#0010_0000#",
         Size        => "16#0000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_acpi_rsdp",
         File_Name   => "pattern",
         File_Format => "acpi_rsdp",
         File_Offset => "none");

      begin
         Pre_Checks.Files_Size (Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Pre_Checks.Check_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "File 'data/pattern' too large for physical memory"
                    & " region 'linux|acpi_rsdp': 16#001e# > 16#0000#",
                    Message   => "Exception mismatch");
      end;
   end File_Larger_Than_Memory;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Pre-check tests");
      T.Add_Test_Routine
        (Routine => File_Existence'Access,
         Name    => "File existence");
      T.Add_Test_Routine
        (Routine => File_Larger_Than_Memory'Access,
         Name    => "File larger than memory region");
      T.Add_Test_Routine
        (Routine => Offset_Larger_Than_File'Access,
         Name    => "Offset larger than file");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Offset_Larger_Than_File
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Command_Line.Test.Set_Input_Dir (Path => "data");
      Command_Line.Test.Set_Output_Dir (Path => "obj");
      Command_Line.Test.Set_Policy (Path => "data/test_policy.xml");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "linux|acpi_rsdp",
         Address     => "16#0010_0000#",
         Size        => "16#0000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_acpi_rsdp",
         File_Name   => "pattern",
         File_Format => "acpi_rsdp",
         File_Offset => "16#ffff#");

      begin
         Pre_Checks.Files_Size (Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Pre_Checks.Check_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Offset of file 'data/pattern' referenced by "
                    & "physical memory region 'linux|acpi_rsdp' larger than "
                    & "file size: 16#ffff# > 16#001e#",
                    Message   => "Exception mismatch");
      end;
   end Offset_Larger_Than_File;

end Pre_Check_Tests;
