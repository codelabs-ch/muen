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
      T.Add_Test_Routine
        (Routine => Validate_Subj_CPU_Affinity'Access,
         Name    => "Validate subject CPU affinity");
      T.Add_Test_Routine
        (Routine => Validate_Major_Ticks'Access,
         Name    => "Validate major frame ticks");
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

   procedure Validate_Major_Ticks
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Scheduling.Major_Frame_Ticks (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid CPU elements in scheduling plan, tick counts "
                    & "differ",
                    Message   => "Exception mismatch");
      end;
   end Validate_Major_Ticks;

   -------------------------------------------------------------------------

   procedure Validate_Subj_CPU_Affinity
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Scheduling.Subject_CPU_Affinity (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'linux' scheduled on wrong CPU 1, should be 0",
                    Message   => "Exception mismatch");
      end;
   end Validate_Subj_CPU_Affinity;

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
