--
--  Copyright (C) 2024  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2024  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Bitops;

with Controller_Component.Events;

with Ctrlr.Slot_Control;

package body Ctrlr.Requests
is
   package Cspecs renames Controller_Component.Events;

   -------------------------------------------------------------------------

   procedure Process
   is
      Pending : Interfaces.Unsigned_64;
   begin

      --  For now, we only explicitly check for pending interrupts that
      --  correspond to Slot 1 reset requests. This is identical to what is
      --  currently done with the slot control era handling.
      --  TODO: This should be reworked once event arrays are supported in
      --  component specs, see T537.

      Pending := Pending_Interrupts (Pending_Interrupts'First);

      if SK.Bitops.Bit_Test (Value => Pending,
                             Pos   => Cspecs.Request_Reset_Slot_1_Vector)
      then
         Slot_Control.Reset (Slot_ID => 1);
      end if;

      Pending_Interrupts (Pending_Interrupts'First) := 0;
   end Process;

end Ctrlr.Requests;
