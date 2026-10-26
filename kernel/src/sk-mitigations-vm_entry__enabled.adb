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
with SK.Mitigations.VM_Entry.IBPB;
with SK.Mitigations.VM_Entry.Verw;

pragma $Release_Warnings
  (Off, "unit * is not referenced", Reason => "Only used for debug output");
with SK.Apic;
with SK.Dump;
with SK.Strings;
pragma $Release_Warnings (On, "unit * is not referenced");

package body SK.Mitigations.VM_Entry
is

   -------------------------------------------------------------------------

   procedure Check_Cpuid_Feature
     (Subleaf : Interfaces.Unsigned_32;
      Bit     : Natural;
      Reason  : SK.Crash_Audit_Types.Reason_Type)
   is
      Audit      : SK.Crash_Audit.Entry_Type;
      Unused_EAX : Interfaces.Unsigned_32;
      Unused_EBX : Interfaces.Unsigned_32;
      Unused_ECX : Interfaces.Unsigned_32;
      EDX        : Interfaces.Unsigned_32;
      Ctx        : SK.Crash_Audit_Types.Mitigations_Context_Type
        := SK.Crash_Audit_Types.Null_Mitigations_Context;
   begin
      Unused_EAX := 16#7#;
      Unused_ECX := Subleaf;
      SK.CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => Unused_ECX,
         EDX => EDX);
      if (EDX and Interfaces.Shift_Left
        (Value  => Interfaces.Unsigned_32 (1),
         Amount => Bit)) = 0
      then
         pragma Debug
           (Apic.Is_BSP,
            Dump.Print_Message
              (Msg => "CPUID.07H.00H:EDX[bit "
               & SK.Strings.Img (Item => Byte (Bit))
               & "] not set, mitigation unsupported, system init failure"));
         Ctx.Cpuid_Value := EDX;
         SK.Crash_Audit.Allocate (Audit => Audit);
         SK.Crash_Audit.Set_Reason
           (Audit  => Audit,
            Reason => Reason);
         SK.Crash_Audit.Set_Mitigations_Context
           (Audit   => Audit,
            Context => Ctx);
         SK.Crash_Audit.Finalize (Audit => Audit);
      end if;
   end Check_Cpuid_Feature;

   -------------------------------------------------------------------------

   --  Execute the enabled VM-entry mitigation primitives (crossing-gated at
   --  the call site): IBPB branch predictor flush and VERW buffer clear.
   procedure Run
   is
   begin
      IBPB.Flush;
      Verw.Clear;
   end Run;

end SK.Mitigations.VM_Entry;
