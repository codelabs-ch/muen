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

with System.Machine_Code;

with SK.Constants;
with SK.Crash_Audit_Types;

package body SK.Mitigations.VM_Entry.Verw
with
   SPARK_Mode => Off
is

   procedure Clear
   is
      --  VERW operand: a data segment selector. Executing VERW against a
      --  valid data selector clears the fill buffers (MD_CLEAR). Mirrors
      --  x86_clear_cpu_buffers(), arch/x86/include/asm/nospec-branch.h,
      --  Linux master f47d223627f8 (2026-10-19).
      Slot : constant Word16 := SK.Constants.SEL_KERN_DATA;
   begin
      System.Machine_Code.Asm
        (Template => "verw %0",
         Inputs   => (Word16'Asm_Input ("m", Slot)),
         Volatile => True);
   end Clear;

   -------------------------------------------------------------------------

   procedure Check
   is
   begin
      --  CPUID.07H.00H:EDX[10] = MD_CLEAR, see SDM Vol. 2 CPUID leaf 7.
      Check_Cpuid_Feature
        (Subleaf => 0,
         Bit     => 10,
         Reason  => SK.Crash_Audit_Types.Mitigations_Cpuid_Md_Clear);
   end Check;
end SK.Mitigations.VM_Entry.Verw;
