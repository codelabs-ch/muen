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

private with System;
private with Muinterrupts;
private with Ctrlr.Config;

package Ctrlr.Requests
is

   -- Process pending requests, if any.
   procedure Process;

private

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Pending_Interrupts : Muinterrupts.Interrupt_Interface_Type
   with
      Volatile,
      Async_Writers,
      Async_Readers,
      Address => System'To_Address (Config.Pending_Interrupts_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

end Ctrlr.Requests;
