--
--  Copyright (C) 2013-2026  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2026  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with SK.Bitops;
with SK.Delays;
with SK.Dump;
with SK.Constants;

pragma $Release_Warnings
  (Off, "unit * is not referenced", Reason => "Only used for debug output");
with SK.Strings;
pragma $Release_Warnings (On, "unit * is not referenced");

package body SK.Apic
with
   Refined_State => (State => IA32_APIC_BASE_Value)
is

   ENABLE_APIC         : constant := 8;
   ENABLE_X2_MODE_FLAG : constant := 10;

   MSR_X2APIC_EOI : constant := 16#80b#;
   MSR_X2APIC_SVR : constant := 16#80f#;
   MSR_X2APIC_ICR : constant := 16#830#;

   --  See Intel SDM Vol. 3A, "10.4.4 Local APIC Status and Location"
   APIC_BSP_FLAG : constant := 8;

   --  See Intel SDM Vol. 3A, "10.6.1 Interrupt Command Register (ICR)"
   Ipi_Init  : constant := 16#0500#;
   Ipi_Start : constant := 16#4601#;

   IA32_APIC_BASE_Value : constant Word64 := CPU.Get_MSR64
      (Register => Constants.IA32_APIC_BASE);

   -------------------------------------------------------------------------

   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.APIC_Init_Context_Type)
   is
      Expected_Is_BSP, Expected_APIC_ID : Boolean;
   begin
      Ctx := Crash_Audit_Types.Null_APIC_Init_Context;
      Ctx.IA32_APIC_BASE := IA32_APIC_BASE_Value;
      pragma Debug (Dump.Print_Message
         (Msg => "APIC: IA32_APIC_BASE "
          & SK.Strings.Img (Ctx.IA32_APIC_BASE)));

      declare
         Unused_EAX, Unused_EBX, Unused_ECX : Word32;
      begin
         Unused_EAX := 16#b#;
         Unused_ECX := 0;
         CPU.CPUID
           (EAX => Unused_EAX,
            EBX => Unused_EBX,
            ECX => Unused_ECX,
            EDX => Ctx.X2APIC_ID);
         pragma Debug (Dump.Print_Message
            (Msg => "APIC: x2APIC ID " & SK.Strings.Img (Ctx.X2APIC_ID)));
      end;
      Expected_APIC_ID := Ctx.X2APIC_ID = CPU_Info.APIC_ID;
      Expected_Is_BSP  := Is_BSP = (CPU_Info.APIC_ID = Skp.BSP_APIC_ID);

      Is_Valid := Expected_Is_BSP and Expected_APIC_ID;
   end Check_State;

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

      Base := Bitops.Bit_Set (Value => IA32_APIC_BASE_Value,
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
   begin
      for Dest_APIC_ID of Skp.CPU_To_APIC_ID loop
         if Dest_APIC_ID /= CPU_Info.APIC_ID then
            Write_ICR (Low  => Ipi_Init,
                       High => Dest_APIC_ID);
            Delays.U_Delay (US => 10 * 1000);

            Write_ICR (Low  => Ipi_Start,
                       High => Dest_APIC_ID);
            Delays.U_Delay (US => 200);

            Write_ICR (Low  => Ipi_Start,
                       High => Dest_APIC_ID);
         end if;
      end loop;
   end Start_AP_Processors;

   -------------------------------------------------------------------------

   procedure Send_IPI
     (Vector : Byte;
      CPU_ID : Skp.CPU_Range)
   is
   begin
      Write_ICR (Low  => Word32 (Vector),
                 High => Skp.CPU_To_APIC_ID (CPU_ID));
   end Send_IPI;

   -------------------------------------------------------------------------

begin
   Is_BSP := Bitops.Bit_Test (Value => IA32_APIC_BASE_Value,
                              Pos   => APIC_BSP_FLAG);
end SK.Apic;
