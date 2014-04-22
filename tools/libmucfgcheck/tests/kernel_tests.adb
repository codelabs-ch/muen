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

with Mucfgcheck.Kernel;

package body Kernel_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Kernel validator tests");
      T.Add_Test_Routine
        (Routine => Validate_CPU_Store_Address_Equality'Access,
         Name    => "Validate CPU Store address equality");
      T.Add_Test_Routine
        (Routine => Validate_Stack_Address_Equality'Access,
         Name    => "Validate stack address equality");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Validate_CPU_Store_Address_Equality
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/kernel/memory/cpu/memory"
            & "[@physical='kernel_store_1']");
      begin
         DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                          Name  => "virtualAddress",
                                          Value => "16#0021_0000#");

         Mucfgcheck.Kernel.CPU_Store_Address_Equality (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'virtualAddress => 16#0021_0000#' of "
                    & "'kernel_store_1' CPU Store memory element differs",
                    Message   => "Exception mismatch");
      end;
   end Validate_CPU_Store_Address_Equality;

   -------------------------------------------------------------------------

   procedure Validate_Stack_Address_Equality
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Kernel.Stack_Address_Equality (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'virtualAddress => 16#0031_0000#' of "
                    & "'kernel_stack_1' kernel stack memory element differs",
                    Message   => "Exception mismatch");
      end;
   end Validate_Stack_Address_Equality;

end Kernel_Tests;
