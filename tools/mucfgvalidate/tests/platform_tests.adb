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

with Validators.Platform;

package body Platform_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Platform validator tests");
      T.Add_Test_Routine
        (Routine => Validate_Memory_Space'Access,
         Name    => "Validate memory space");
      T.Add_Test_Routine
        (Routine => Validate_Memblock_Overlap'Access,
         Name    => "Validate memory block overlap");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Validate_Memblock_Overlap
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Validators.Platform.Memory_Block_Overlap (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of platform memory block 'ram_1' and 'ram_2'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Memblock_Overlap;

   -------------------------------------------------------------------------

   procedure Validate_Memory_Space
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Validators.Platform.Memory_Space (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Allocated 237605 bytes of physical memory but only 4162"
                    & " bytes available by the platform",
                    Message   => "Exception mismatch");
      end;
   end Validate_Memory_Space;

end Platform_Tests;
