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

with Mutools.Processors;

package body Processors_Tests
is

   use Ahven;
   use Mutools;

   Test_Counter : Natural := 0;

   --  Increment test counter variable.
   procedure Inc_Counter (Data : Natural);

   -------------------------------------------------------------------------

   procedure Inc_Counter (Data : Natural)
   is
      pragma Unreferenced (Data);
   begin
      Test_Counter := Test_Counter + 1;
   end Inc_Counter;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Processors  tests");
      T.Add_Test_Routine
        (Routine => Register_Processor'Access,
         Name    => "Register data processor");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Register_Processor
   is
      package Proc_Package is new Processors (Param_Type => Natural);

      Data : constant Natural := 0;
   begin
      Assert (Condition => Proc_Package.Get_Count = 0,
              Message   => "Processors not empty");

      Proc_Package.Register (Process => Inc_Counter'Access);
      Proc_Package.Register (Process => Inc_Counter'Access);

      Assert (Condition => Proc_Package.Get_Count = 2,
              Message   => "Processor count not 2");

      Proc_Package.Run (Data => Data);
      Assert (Condition => Test_Counter = 2,
              Message   => "Test counter not 2");

      Proc_Package.Clear;
      Assert (Condition => Proc_Package.Get_Count = 0,
              Message   => "Processor count not 0");

      Test_Counter := 0;

   exception
      when others =>
         Proc_Package.Clear;
         Test_Counter := 0;
         raise;
   end Register_Processor;

end Processors_Tests;
