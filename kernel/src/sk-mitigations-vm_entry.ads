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
with SK.Crash_Audit_Types;

use type SK.Crash_Audit_Types.Reason_Type;

--D @Interface
--D This package implements the VM-entry mitigation primitives. It is
--D invoked by the kernel on every scheduling-partition crossing and once
--D before the first subject entry at boot; the primitives compiled in are
--D selected at build time (kernel.gpr), so no configuration-dependent
--D runtime branches exist. Each primitive lives in a child package with
--D its own enabled/disabled body selection.
package SK.Mitigations.VM_Entry
is

   --  Execute the enabled VM-entry mitigation primitives (crossing-gated at
   --  the call site): IBPB branch predictor flush and VERW buffer clear.
   procedure Run
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ X86_64.State);

   --  Verify that CPUID.07H.Subleaf:EDX[Bit] is set; on failure record a
   --  crash audit entry with the given reason and restart. The per-primitive
   --  children invoke this shared check for the enabled vmEntry primitives.
   procedure Check_Cpuid_Feature
     (Subleaf : Interfaces.Unsigned_32;
      Bit     : Natural;
      Reason  : SK.Crash_Audit_Types.Reason_Type)
   with
      Pre     => Bit <= 31
        and then Reason /= SK.Crash_Audit_Types.Reason_Undefined,
      Global  => (Input  => CPU_Info.APIC_ID,
                  In_Out => (Crash_Audit.State, X86_64.State)),
      Depends => (Crash_Audit.State => (Crash_Audit.State,
                                        Bit,
                                        CPU_Info.APIC_ID,
                                        Reason,
                                        Subleaf,
                                        X86_64.State),
                  X86_64.State      => (Crash_Audit.State,
                                        Bit,
                                        CPU_Info.APIC_ID,
                                        Reason,
                                        Subleaf,
                                        X86_64.State));

end SK.Mitigations.VM_Entry;
