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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Muxml;

with Pack.Command_Line.Test;
with Pack.Checks;

package body Check_Tests
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

      Checks.Files_Exist (Data => Policy);

      declare
         Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Policy.Doc,
               XPath => "/system/memory/memory[@name='linux|acpi_rsdp']/file"),
            Index => 0);
      begin

         --  Set invalid filename.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "filename",
            Value => "nonexistent");

         Checks.Files_Exist (Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Checks.Check_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "File 'data/nonexistent' referenced by physical memory "
                    & "region 'linux|acpi_rsdp' not found",
                    Message   => "Exception mismatch");
      end;
   end File_Existence;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Checker tests");
      T.Add_Test_Routine
        (Routine => File_Existence'Access,
         Name    => "File existence");
   end Initialize;

end Check_Tests;
