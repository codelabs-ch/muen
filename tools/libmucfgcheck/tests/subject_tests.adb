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

with Mucfgcheck.Subject;

package body Subject_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Subject validator tests");
      T.Add_Test_Routine
        (Routine => Validate_CPU_IDs'Access,
         Name    => "Validate CPU IDs");
      T.Add_Test_Routine
        (Routine => Validate_Name_Uniqueness'Access,
         Name    => "Validate name uniqueness");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Validate_CPU_IDs
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "cpu",
            Value => "7");

         Mucfgcheck.Subject.CPU_ID (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'cpu => 7' of 'linux' subject element not in "
                    & "valid range 0 .. 3",
                    Message   => "Exception mismatch");
      end;
   end Validate_CPU_IDs;

   -------------------------------------------------------------------------

   procedure Validate_Name_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");
      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='subject1']");
      begin

         --  Set duplicate subject name.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "name",
            Value => "linux");

         Mucfgcheck.Subject.Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subjects with id 0 and 1 have identical name 'linux'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Name_Uniqueness;

end Subject_Tests;
