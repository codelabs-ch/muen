--
--  Copyright (C) 2026  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2026  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

--D @Interface
--D This package implements the IA32_ARCH_CAPABILITIES boot check; the MCU
--D revision floor lives in the SK.Mitigations.MCU child.
package SK.Mitigations
is

   --  Verify (IA32_ARCH_CAPABILITIES and Mask) = Expected; on failure record
   --  a crash audit entry and restart. Skipped (no MSR read) on targets
   --  without ARCH_CAP requires.
   procedure Check_Arch_Cap
   with
      Global  => (Input  => CPU_Info.APIC_ID,
                  In_Out => (Crash_Audit.State, X86_64.State)),
      Depends => (Crash_Audit.State => (Crash_Audit.State,
                                        CPU_Info.APIC_ID,
                                        X86_64.State),
                  X86_64.State      => (Crash_Audit.State,
                                        CPU_Info.APIC_ID,
                                        X86_64.State));

end SK.Mitigations;
