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

package body SK.Mitigations.VM_Entry.L1D_Flush
is

   --  IA32_FLUSH_CMD (flush command) MSR, see SDM Vol. 4, Table 2-2, MSR
   --  0x10B.
   IA32_FLUSH_CMD : constant := 16#10B#;

   -------------------------------------------------------------------------

   procedure Flush
   is
   begin
      --  Writeback and invalidate the L1 data cache (IA32_FLUSH_CMD bit 0,
      --  L1D_FLUSH). Mirrors l1d_flush(), arch/x86/include/asm/
      --  cacheflush.h, Linux master f47d223627f8 (2026-10-19). The flush
      --  clears L1D only: on FB_CLEAR processors (SDM Vol. 4, Table 2-2,
      --  MSR 0x10A) it does NOT overwrite the fill buffer values - the VERW
      --  buffer clear run after this in VM_Entry.Run is that fill-buffer
      --  clear there, so both primitives stay independent.
      SK.CPU.Write_MSR64
        (Register => IA32_FLUSH_CMD,
         Value    => 1);
   end Flush;

   -------------------------------------------------------------------------

   procedure Check
   is
   begin
      --  CPUID.07H.00H:EDX[28] = FLUSH_L1D, see SDM Vol. 2 CPUID leaf 7.
      SK.Mitigations.Cpuid.Check_Cpuid_Feature
        (Subleaf => 0,
         Bit     => 28,
         Reason  => SK.Crash_Audit_Types.Mitigations_Cpuid_L1d_Flush);
   end Check;

end SK.Mitigations.VM_Entry.L1D_Flush;
