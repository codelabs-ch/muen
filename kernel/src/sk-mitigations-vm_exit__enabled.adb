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

with SK.Crash_Audit_Types;
with SK.Mitigations.Cpuid;

package body SK.Mitigations.VM_Exit
is

   procedure Check
   is
   begin
      --  CPUID.07H.00H:EDX[26] = IBRS/IBPB support, see SDM Vol. 2 CPUID
      --  leaf 7. The same bit also covers the IA32_SPEC_CTRL MSR used by
      --  the legacy IBRS re-assert.
      SK.Mitigations.Cpuid.Check_Cpuid_Feature
        (Subleaf => 0,
         Bit     => 26,
         Reason  => SK.Crash_Audit_Types.Mitigations_Cpuid_Ibrs_Ipbp);
   end Check;

end SK.Mitigations.VM_Exit;
