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

pragma $Release_Warnings
  (Off, "unit * is not referenced", Reason => "Only used for debug output");
with SK.Apic;
with SK.Dump;
pragma $Release_Warnings (On, "unit * is not referenced");

package body SK.Mitigations.VM_Entry
is

   --  IA32_PRED_CMD (prediction command) MSR, see SDM Vol. 4, Table 2-2,
   --  MSR 0x49.
   IA32_PRED_CMD : constant := 16#49#;

   -------------------------------------------------------------------------

   procedure Run
   is
   begin
      --  IBPB: flush the branch predictor before entering the next subject.
      --  Mirrors indirect_branch_prediction_barrier(),
      --  arch/x86/include/asm/nospec-branch.h, kernel 6.12.94.
      SK.CPU.Write_MSR64
        (Register => IA32_PRED_CMD,
         Value    => 1);
   end Run;

   -------------------------------------------------------------------------

   procedure Check_Ibpb
   is
      Audit         : SK.Crash_Audit.Entry_Type;
      Unused_EAX    : Interfaces.Unsigned_32;
      Unused_EBX    : Interfaces.Unsigned_32;
      Unused_ECX    : Interfaces.Unsigned_32;
      EDX           : Interfaces.Unsigned_32;
      Ctx           : SK.Crash_Audit_Types.Mitigations_Context_Type
        := SK.Crash_Audit_Types.Null_Mitigations_Context;

      --  CPUID.07H.00H:EDX[26] = IBPB support, see SDM Vol. 2 CPUID leaf 7.
      IBPB_Support_Bit : constant := 26;
   begin
      Unused_EAX := 16#7#;
      Unused_ECX := 0;
      SK.CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => Unused_ECX,
         EDX => EDX);
      if (EDX and 2 ** IBPB_Support_Bit) = 0 then
         pragma Debug
           (Apic.Is_BSP,
            Dump.Print_Message
              (Msg => "IBPB: CPUID.07H.00H:EDX[26] unset, IA32_PRED_CMD "
               & "IBPB unsupported, system init failure"));
         Ctx.Cpuid_Value := EDX;
         SK.Crash_Audit.Allocate (Audit => Audit);
         SK.Crash_Audit.Set_Reason
           (Audit  => Audit,
            Reason => SK.Crash_Audit_Types.Mitigations_Cpuid_Ibpb);
         SK.Crash_Audit.Set_Mitigations_Context
           (Audit   => Audit,
            Context => Ctx);
         SK.Crash_Audit.Finalize (Audit => Audit);
      end if;
   end Check_Ibpb;

end SK.Mitigations.VM_Entry;
