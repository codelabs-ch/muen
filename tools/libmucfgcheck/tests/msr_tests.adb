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

with Mucfgcheck.MSR;

package body MSR_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "MSR validator tests");
      T.Add_Test_Routine
        (Routine => Validate_Start_Smaller_End'Access,
         Name    => "Validate MSR range start less or equal end");
      T.Add_Test_Routine
        (Routine => Validate_Low_High'Access,
         Name    => "Validate MSR range low/high");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Validate_Low_High
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/vcpu/registers/msrs/"
            & "msr[@end='16#0176#']");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "end",
            Value => "16#c000_0800#");

         Mucfgcheck.MSR.Low_Or_High (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "MSR start 16#0174# and end 16#c000_0800# in different"
                    & " low/high range (Subject 'linux')",
                    Message   => "Exception mismatch");
      end;
   end Validate_Low_High;

   -------------------------------------------------------------------------

   procedure Validate_Start_Smaller_End
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.MSR.Start_Smaller_End (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "MSR start 16#0902# larger than end 16#0800#"
                    & " (Subject 'linux')",
                    Message   => "Exception mismatch");
      end;
   end Validate_Start_Smaller_End;

end MSR_Tests;
