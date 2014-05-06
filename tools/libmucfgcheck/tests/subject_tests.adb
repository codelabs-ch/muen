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
      T.Add_Test_Routine
        (Routine => Validate_Memory_Types'Access,
         Name    => "Validate memory types");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Validate_CPU_IDs
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']",
         Name  => "cpu",
         Value => "7");

      begin
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

   procedure Validate_Memory_Types
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vt|bin']",
         Name  => "type",
         Value => "system_pt");

      begin
         Mucfgcheck.Subject.Memory_Types (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical memory region 'binary' of subject 'vt' mapping "
                    & "physical region 'vt|bin' has invalid type system_pt",
                    Message   => "Exception mismatch");
      end;
   end Validate_Memory_Types;

   -------------------------------------------------------------------------

   procedure Validate_Name_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']",
         Name  => "name",
         Value => "linux");

      begin
         Mucfgcheck.Subject.Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subjects with id 1 and 4 have identical name 'linux'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Name_Uniqueness;

end Subject_Tests;
