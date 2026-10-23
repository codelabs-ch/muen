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
--D This package implements the VM-entry mitigation primitives. It is
--D invoked by the kernel on every scheduling-partition crossing and once
--D before the first subject entry at boot; the primitives compiled in are
--D selected at build time (kernel.gpr), so no configuration-dependent
--D runtime branches exist.
package SK.Mitigations.VM_Entry
is

   --  Execute the enabled VM-entry mitigation primitives. Currently this is
   --  the IA32_PRED_CMD.IBPB branch predictor flush preceding a subject in
   --  a different scheduling partition (strategy doc, section 3.1).
   procedure Run
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ X86_64.State);

   --  Verify IA32_PRED_CMD.IBPB support (CPUID.07H.00H:EDX[26]); on failure
   --  record a crash audit entry and restart. Runs on every core at boot
   --  when the VM-entry mitigation policy is enabled.
   procedure Check_Ibpb
   with
      Global  => (Input  => CPU_Info.APIC_ID,
                  In_Out => (Crash_Audit.State, X86_64.State)),
      Depends => (Crash_Audit.State => (Crash_Audit.State,
                                        CPU_Info.APIC_ID,
                                        X86_64.State),
                  X86_64.State      => (Crash_Audit.State,
                                        CPU_Info.APIC_ID,
                                        X86_64.State));

end SK.Mitigations.VM_Entry;
