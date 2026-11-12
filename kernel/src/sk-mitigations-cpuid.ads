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

with Interfaces;

with X86_64;

with SK.CPU_Info;
with SK.Crash_Audit;
with SK.Crash_Audit_Types;

use type SK.Crash_Audit_Types.Reason_Type;

--D @Interface
--D Shared boot-time CPUID feature check for the enabled mitigation
--D primitives. The check is compiled in unconditionally; the
--D per-mitigation enablement selects which callers exist.
package SK.Mitigations.Cpuid
is

   --  Verify that CPUID.07H.Subleaf:EDX[Bit] is set; on failure record a
   --  crash audit entry with the given reason and restart.
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

end SK.Mitigations.Cpuid;
