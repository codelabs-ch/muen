--
--  Copyright (C) 2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with SK.Hypercall;

with Subject_Info;

with Isolation_Tests_Monitor_Component.Events;

with ITSM;

procedure Isolation_Tests_Monitor
is
   use type Interfaces.Unsigned_64;

   RIP             : Interfaces.Unsigned_64;
   Instruction_Len : Interfaces.Unsigned_32;
begin
   loop

      --  Save current state to isolation test runner result state.

      ITSM.Result_State := Subject_Info.State;

      --  Advance RIP in order to continue ITS runner.

      RIP := Subject_Info.State.RIP;
      Instruction_Len := Subject_Info.State.Instruction_Len;
      Subject_Info.State.RIP := RIP + Interfaces.Unsigned_64 (Instruction_Len);

      --  Resume isolation test runner

      SK.Hypercall.Trigger_Event
        (Number => Isolation_Tests_Monitor_Component.Events.Resume_Tests_ID);
   end loop;
end Isolation_Tests_Monitor;
