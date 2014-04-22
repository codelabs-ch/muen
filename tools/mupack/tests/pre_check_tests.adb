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

with DOM.Core.Elements;

with Muxml.Utils;

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

      Pre_Checks.Files_Exist (Data => Policy);

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory[@name='linux|acpi_rsdp']/file");
      begin

         --  Set invalid filename.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "filename",
            Value => "nonexistent");

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

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory[@name='linux|acpi_rsdp']");
      begin

         --  Make memory region too small.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "size",
            Value => "16#0000#");

         Pre_Checks.Files_Size (Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Pre_Checks.Check_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "File 'data/sections.ref' too large for physical memory"
                    & " region 'linux|acpi_rsdp': 16#026b# > 16#0000#",
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

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory[@name='linux|acpi_rsdp']/file");
      begin

         --  Make offset larger than file.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "offset",
            Value => "16#ffff#");

         Pre_Checks.Files_Size (Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Pre_Checks.Check_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Offset of file 'data/sections.ref' referenced by "
                    & "physical memory region 'linux|acpi_rsdp' larger than "
                    & "file size: 16#ffff# > 16#026b#",
                    Message   => "Exception mismatch");
      end;
   end Offset_Larger_Than_File;

end Pre_Check_Tests;
