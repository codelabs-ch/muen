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

with SK.CPU;
with SK.Crash_Audit_Types;
with SK.Mitigations.Cpuid;

package body SK.Mitigations.VM_Entry.IBPB
is

   --  IA32_PRED_CMD (prediction command) MSR, see SDM Vol. 4, Table 2-2,
   --  MSR 0x49.
   IA32_PRED_CMD : constant := 16#49#;

   -------------------------------------------------------------------------

   procedure Flush
   is
   begin
      --  IBPB: flush the branch predictor before entering the next subject.
      --  Mirrors write_ibpb(), arch/x86/entry/entry.S, Linux master
      --  f47d223627f8 (2026-10-19). Linux additionally fills the RSB after
      --  IBPB when X86_BUG_IBPB_NO_RET is set (AMD in current Linux); not
      --  needed on the Intel target.
      SK.CPU.Write_MSR64
        (Register => IA32_PRED_CMD,
         Value    => 1);
   end Flush;

   -------------------------------------------------------------------------

   procedure Check
   is
   begin
      --  CPUID.07H.00H:EDX[26] = IBPB support, see SDM Vol. 2 CPUID leaf 7.
      SK.Mitigations.Cpuid.Check_Cpuid_Feature
        (Subleaf => 0,
         Bit     => 26,
         Reason  => SK.Crash_Audit_Types.Mitigations_Cpuid_Ibrs_Ipbp);
   end Check;

end SK.Mitigations.VM_Entry.IBPB;
