--
--  Copyright (C) 2013, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp;

with SK.CPU;
with SK.Bitops;
with SK.Delays;
with SK.Constants;

package body SK.Apic
is

   ENABLE_APIC         : constant := 8;
   ENABLE_X2_MODE_FLAG : constant := 10;

   MSR_X2APIC_EOI : constant := 16#80b#;
   MSR_X2APIC_SVR : constant := 16#80f#;
   MSR_X2APIC_ICR : constant := 16#830#;

   --  See Intel SDM, Vol. 3A, section 10.4.4.
   APIC_BSP_FLAG : constant := 8;

   --  See Intel SDM, Vol. 3A, section 10.6.1.
   Ipi_Init  : constant := 16#0500#;
   Ipi_Start : constant := 16#4601#;

   -------------------------------------------------------------------------

   function Is_BSP return Boolean
   is
      Apic_Base_Value : constant Word64
        := CPU.Get_MSR64 (Register => Constants.IA32_APIC_BASE);
   begin
      return Bitops.Bit_Test
        (Value => Apic_Base_Value,
         Pos   => APIC_BSP_FLAG);
   end Is_BSP;

   -------------------------------------------------------------------------

   --  Write given value to the ICR register of the local APIC.
   procedure Write_ICR
     (Low  : Word32;
      High : Word32)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ (Low, High))
   is
   begin
      CPU.Write_MSR (Register => MSR_X2APIC_ICR,
                     Low      => Low,
                     High     => High);
   end Write_ICR;

   -------------------------------------------------------------------------

   procedure Enable
   is
      Base, Svr : Word64;
   begin

      --  Enable x2APIC mode.

      Base := CPU.Get_MSR64 (Register => Constants.IA32_APIC_BASE);
      Base := Bitops.Bit_Set (Value => Base,
                              Pos   => ENABLE_X2_MODE_FLAG);
      CPU.Write_MSR64 (Register => Constants.IA32_APIC_BASE,
                       Value    => Base);

      --  Set bit 8 of the APIC spurious vector register (SVR).

      Svr := CPU.Get_MSR64 (Register => MSR_X2APIC_SVR);
      Svr := Bitops.Bit_Set (Value => Svr,
                             Pos   => ENABLE_APIC);
      CPU.Write_MSR64 (Register => MSR_X2APIC_SVR,
                       Value    => Svr);
   end Enable;

   -------------------------------------------------------------------------

   procedure EOI
   is
   begin
      CPU.Write_MSR64 (Register => MSR_X2APIC_EOI,
                       Value    => 0);
   end EOI;

   -------------------------------------------------------------------------

   procedure Start_AP_Processors
   is
      use type Skp.CPU_Range;

      subtype Start_Range is Skp.CPU_Range range 1 .. Skp.CPU_Range'Last;

      function To_APIC_ID (AP_ID : Start_Range) return Word32 is
        (Word32 (AP_ID) * 2);
   begin
      for I in Start_Range loop
         declare
            Dest : constant Word32 := To_APIC_ID (AP_ID => I);
         begin
            Write_ICR (Low  => Ipi_Init,
                       High => Dest);
            Delays.U_Delay (US => 10 * 1000);

            Write_ICR (Low  => Ipi_Start,
                       High => Dest);
            Delays.U_Delay (US => 200);

            Write_ICR (Low  => Ipi_Start,
                       High => Dest);
         end;
      end loop;
   end Start_AP_Processors;

   -------------------------------------------------------------------------

   procedure Send_IPI
     (Vector  : Byte;
      Apic_ID : Byte)
   is
   begin
      Write_ICR (Low  => Word32 (Vector),
                 High => Word32 (Apic_ID));
   end Send_IPI;

end SK.Apic;
