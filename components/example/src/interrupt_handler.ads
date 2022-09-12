--
--  Copyright (C) 2013-2022  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2022  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Exceptions;

with Musinfo.Instance;

package Interrupt_Handler
is

   --  Exception/Interrupt handler.
   procedure Dispatch_Exception (Context : SK.Exceptions.Isr_Context_Type)
   with
      Export,
      Convention => C,
      Link_Name  => "dispatch_interrupt",
      Pre        => Musinfo.Instance.Is_Valid;

   --  Interrupt handler.
   procedure Handle_Interrupt (Vector : SK.Byte)
   with
      Pre => Musinfo.Instance.Is_Valid;

end Interrupt_Handler;
