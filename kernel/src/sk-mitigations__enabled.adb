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
with SK.Strings;
pragma $Release_Warnings (On, "unit * is not referenced");

with Skp.Mitigations;

package body SK.Mitigations
is

   -------------------------------------------------------------------------

   procedure Check_Arch_Cap
   is
      IA32_ARCH_CAPABILITIES : constant := 16#10a#;

      Audit   : SK.Crash_Audit.Entry_Type;
      High    : Interfaces.Unsigned_32;
      Low     : Interfaces.Unsigned_32;
      Value   : Interfaces.Unsigned_64;
      Mask    : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64 (Skp.Mitigations.Arch_Cap_Mask);
      Mit_Ctx : SK.Crash_Audit_Types.Mitigations_Context_Type
        := SK.Crash_Audit_Types.Null_Mitigations_Context;
   begin
      CPU.Get_MSR
        (Register => IA32_ARCH_CAPABILITIES,
         Low      => Low,
         High     => High);
      Value := Interfaces.Shift_Left
        (Value  => Interfaces.Unsigned_64 (High),
         Amount => 32)
        or Interfaces.Unsigned_64 (Low);
      if (Value and Mask) /= Mask then

         --  Common info is only logged on BSP (see SK.MCU).

         pragma Debug
           (Apic.Is_BSP,
            Dump.Print_Message
              (Msg => "ARCH_CAP: Value "
               & SK.Strings.Img (Item => Value)
               & " does not satisfy required mask "
               & SK.Strings.Img (Item => Mask)
               & ", system init failure"));
         Mit_Ctx.Arch_Cap_Value := Value;
         Mit_Ctx.Arch_Cap_Mask  := Mask;
         SK.Crash_Audit.Allocate (Audit => Audit);
         SK.Crash_Audit.Set_Reason
           (Audit  => Audit,
            Reason => SK.Crash_Audit_Types.Mitigations_Arch_Cap);
         SK.Crash_Audit.Set_Mitigations_Context
           (Audit   => Audit,
            Context => Mit_Ctx);
         SK.Crash_Audit.Finalize (Audit => Audit);
      end if;
   end Check_Arch_Cap;

end SK.Mitigations;
