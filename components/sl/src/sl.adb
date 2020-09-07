--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.CPU;
with SK.Hypercall;

with Musinfo.Instance;
with Mucontrol.Status;

with Init.Run;
with Init.Status;

with Sl_Component.Events;

procedure Sl
is
   Success : Boolean;
begin
   if not Musinfo.Instance.Is_Valid then
      SK.CPU.Stop;
   end if;

   loop
      Init.Run.Initialize (Success => Success);
      if Success then
         Init.Status.Set (New_State => Mucontrol.Status.STATE_RUNNING);
      end if;
      SK.Hypercall.Trigger_Event (Number => Sl_Component.Events.Start_ID);
   end loop;
end Sl;
