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

with Validators.Scheduling;

package body Scheduling_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Scheduling validator tests");
      T.Add_Test_Routine
        (Routine => Validate_CPU_Element_Count'Access,
         Name    => "Validate CPU elements in major frame");
      T.Add_Test_Routine
        (Routine => Validate_Subj_References'Access,
         Name    => "Validate subject references");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Validate_CPU_Element_Count
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Scheduling.CPU_Element_Count (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "CPU element count of 2 in scheduling plan invalid, "
                    & "logical CPU count is 1",
                    Message   => "Exception mismatch");
      end;
   end Validate_CPU_Element_Count;

   -------------------------------------------------------------------------

   procedure Validate_Subj_References
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Scheduling.Subject_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'nonexistent' referenced in scheduling plan not"
                    & " found",
                    Message   => "Exception mismatch");
      end;
   end Validate_Subj_References;

end Scheduling_Tests;
