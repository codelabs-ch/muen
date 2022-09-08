--
--  Copyright (C) 2013-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with X86_64;

with SK.CPU_Info;
with SK.Crash_Audit;
with SK.Exceptions;
with SK.MCE;

--D @Interface
--D This package provides procedures to disable the legacy Programmable
--D Interrupt Controller (PIC) and the Programmable Interrupt Timer (PIT).
--D Moreover, an interrupt handler which is invoked if an exception occurs
--D during kernel execution is provided.
package SK.Interrupts
is

   --  Mask all interrupts in the legacy PIC.
   procedure Disable_Legacy_PIC
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

   --  Disable legacy PIT.
   procedure Disable_Legacy_PIT
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

   --  Write ISR context information to crash audit and trigger warm restart.
   --  Since an exception during kernel execution constitutes an unrecoverable
   --  error the procedure is marked as no return.
   procedure Dispatch_Exception (Context : Exceptions.Isr_Context_Type)
   with
      Global     => (Input  => (CPU_Info.APIC_ID, MCE.State),
                     In_Out => (Crash_Audit.State, X86_64.State)),
      Pre        => MCE.Valid_State,
      No_Return,
      Export,
      Convention => C,
      Link_Name  => "dispatch_interrupt";

end SK.Interrupts;
