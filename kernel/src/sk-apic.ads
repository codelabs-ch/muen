--
--  Copyright (C) 2013, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp;

--D @Interface
--D This package contains subprograms to interact with the local APIC, see Intel
--D SDM Vol. 3A, "Chapter 10 Advanced Programmable Interrupt Controller (APIC)"
--D \cite{intelsdm}. The APIC is a per-CPU interruptD controller which is
--D required for interrupt processing and sending of interprocessor Interrupts
--D (IPIs).
--D
--D Additionally, it provides the information if a CPU is the bootstrap
--D processor (BSP), which initially brings up the system. Muen programs the
--D APIC in x2APIC mode.
package SK.Apic
is

   --  Place local APIC in x2APIC mode and set bit 8 of the APIC spurious
   --  vector register (SVR).
   procedure Enable
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

   --  Startup AP processors by sending INIT-SIPI-SIPI IPI sequence, see Intel
   --  SDM Vol. 3A, "8.4.4 MP Initialization Example".
   procedure Start_AP_Processors
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

   --  Signal interrupt servicing completion.
   procedure EOI
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

   --  Send Interprocessor Interrupt (IPI) with given vector to the CPU core
   --  identified by CPU ID.
   procedure Send_IPI
     (Vector : Byte;
      CPU_ID : Skp.CPU_Range)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ (CPU_ID, Vector));

   --  Returns True if the executing CPU is the bootstrap processor (BSP).
   function Is_BSP return Boolean
   with
      Global => (Input => X86_64.State),
      Volatile_Function;

end SK.Apic;
