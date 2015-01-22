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

with SK.CPU;
with SK.Constants;

package body SK.Apic_Mode
is

   ENABLE_APIC         : constant := 8;
   ENABLE_X2_MODE_FLAG : constant := 10;

   MSR_X2APIC_ID  : constant := 16#802#;
   MSR_X2APIC_EOI : constant := 16#80b#;
   MSR_X2APIC_SVR : constant := 16#80f#;
   MSR_X2APIC_ICR : constant := 16#830#;

   -------------------------------------------------------------------------

   procedure EOI
   is
   begin
      CPU.Write_MSR64 (Register => MSR_X2APIC_EOI,
                       Value    => 0);
   end EOI;

   -------------------------------------------------------------------------

   procedure Enable
   is
      Base, Svr : SK.Word64;
   begin

      --  Enable x2APIC mode.

      Base := CPU.Get_MSR64 (Register => Constants.IA32_APIC_BASE);
      Base := SK.Bit_Set (Value => Base,
                          Pos   => ENABLE_X2_MODE_FLAG);
      CPU.Write_MSR64 (Register => Constants.IA32_APIC_BASE,
                       Value    => Base);

      --  Set bit 8 of the APIC spurious vector register (SVR).

      Svr := CPU.Get_MSR64 (Register => MSR_X2APIC_SVR);
      Svr := SK.Bit_Set (Value => Svr,
                         Pos   => ENABLE_APIC);
      CPU.Write_MSR64 (Register => MSR_X2APIC_SVR,
                       Value    => Svr);
   end Enable;

   -------------------------------------------------------------------------

   function Get_ID return SK.Byte
   is
      ID, Unused : SK.Word32;
   begin
      pragma $Prove_Warnings (Off, "unused assignment to ""Unused""",
         Reason => "Get_ID only needs the lower half of the MSR.");

      CPU.Get_MSR (Register => MSR_X2APIC_ID,
                   Low      => ID,
                   High     => Unused);

      pragma $Prove_Warnings (On, "unused assignment to ""Unused""");

      return SK.Byte'Mod (ID);
   end Get_ID;

   -------------------------------------------------------------------------

   --  Write given low/high doubleword value to the ICR register of the local
   --  APIC.
   procedure Write_ICR (Low, High : SK.Word32)
   is
   begin
      CPU.Write_MSR (Register => MSR_X2APIC_ICR,
                     Low      => Low,
                     High     => High);
   end Write_ICR;

end SK.Apic_Mode;
