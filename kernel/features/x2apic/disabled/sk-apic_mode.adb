--
--  Copyright (C) 2013-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with System;

with Skp.Features.XApic;

--  xAPIC mode implementation. Access to the registers is modeled using local,
--  volatile variables instead of a global record type. This saves us from
--  introducing a global SPARK state and is consistent with how x2APIC mode is
--  modeled (i.e. the APIC state is part of the X86_64.State).
package body SK.Apic_Mode
is

   ENABLE_APIC : constant := 16#0100#;

   OFFSET_XAPIC_ID       : constant := 16#0020#;
   OFFSET_XAPIC_EOI      : constant := 16#00b0#;
   OFFSET_XAPIC_SVR      : constant := 16#00f0#;
   OFFSET_XAPIC_ICR_LOW  : constant := 16#0300#;
   OFFSET_XAPIC_ICR_HIGH : constant := 16#0310#;

   -------------------------------------------------------------------------

   procedure EOI
   with
      SPARK_Mode => Off
   is
      Reg : SK.Word32
        with
          Volatile,
          Address => System'To_Address
            (Skp.Features.XApic.Local_Apic_Address + OFFSET_XAPIC_EOI);
   begin
      Reg := 0;
   end EOI;

   -------------------------------------------------------------------------

   procedure Enable
   with
      SPARK_Mode => Off
   is
      Reg : SK.Word32
        with
          Volatile,
          Address => System'To_Address
            (Skp.Features.XApic.Local_Apic_Address + OFFSET_XAPIC_SVR);
   begin

      --  Set bit 8 of the APIC spurious vector register (SVR).

      Reg := ENABLE_APIC;
   end Enable;

   -------------------------------------------------------------------------

   function Get_ID return SK.Byte
   with
      SPARK_Mode => Off
   is
      Reg : SK.Word32
        with
          Volatile,
          Address => System'To_Address
            (Skp.Features.XApic.Local_Apic_Address + OFFSET_XAPIC_ID);
   begin
      return SK.Byte'Mod (Reg / 2 ** 24);
   end Get_ID;

   -------------------------------------------------------------------------

   --  Write given low/high doubleword value to the ICR register of the local
   --  APIC.
   procedure Write_ICR (Low, High : SK.Word32)
   with
      SPARK_Mode => Off
   is
      Reg_Low  : SK.Word32
        with
          Volatile,
          Address => System'To_Address
            (Skp.Features.XApic.Local_Apic_Address + OFFSET_XAPIC_ICR_LOW);
      Reg_High : SK.Word32
        with
          Volatile,
          Address => System'To_Address
            (Skp.Features.XApic.Local_Apic_Address + OFFSET_XAPIC_ICR_HIGH);
   begin
      Reg_Low  := Low;
      Reg_High := High;
   end Write_ICR;

end SK.Apic_Mode;
