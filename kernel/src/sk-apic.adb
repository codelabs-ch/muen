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

with SK.CPU;
with SK.Constants;

package body SK.Apic
is

   ENABLE_APIC         : constant := 8;
   ENABLE_X2_MODE_FLAG : constant := 10;

   MSR_X2APIC_ID  : constant := 16#802#;
   MSR_X2APIC_EOI : constant := 16#80b#;
   MSR_X2APIC_SVR : constant := 16#80f#;
   MSR_X2APIC_ICR : constant := 16#830#;

   Ipi_Init_Broadcast  : constant := 16#000c4501#;
   Ipi_Start_Broadcast : constant := 16#000c4601#;

   -------------------------------------------------------------------------

   --  Write given value to the ICR register of the local APIC.
   procedure Write_ICR (Value : SK.Word64)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ Value)
   is
      Low_Dword, High_Dword : SK.Word32;
   begin
      Low_Dword  := SK.Word32'Mod (Value);
      High_Dword := SK.Word32'Mod (Value / 2 ** 32);

      CPU.Write_MSR (Register => MSR_X2APIC_ICR,
                     Low      => Low_Dword,
                     High     => High_Dword);
   end Write_ICR;

   -------------------------------------------------------------------------

   --  Busy-sleep for a given (scaled) period of time.
   procedure Busy_Wait (Count : Positive)
   with
      Depends => (null => Count),
      Pre     => Count < Integer'Last / 2 ** 8
   is
   begin
      for I in Integer range 1 .. Count * (2 ** 8) loop
         null;
      end loop;
   end Busy_Wait;

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

   procedure EOI
   is
   begin
      CPU.Write_MSR64 (Register => MSR_X2APIC_EOI,
                       Value    => 0);
   end EOI;

   -------------------------------------------------------------------------

   function Get_ID return SK.Byte
   is
      ID, Unused : SK.Word32;
   begin

      pragma Warnings (GNATprove, Off, "unused assignment to ""Unused""",
         Reason => "Get_ID only needs the lower half of the MSR.");

      CPU.Get_MSR (Register => MSR_X2APIC_ID,
                   Low      => ID,
                   High     => Unused);

      pragma Warnings (GNATprove, On, "unused assignment to ""Unused""");

      return SK.Byte'Mod (ID);
   end Get_ID;

   -------------------------------------------------------------------------

   procedure Start_AP_Processors
   is
   begin
      Write_ICR (Value => Ipi_Init_Broadcast);
      Busy_Wait (Count => 10);

      Write_ICR (Value => Ipi_Start_Broadcast);
      Busy_Wait (Count => 200);

      Write_ICR (Value => Ipi_Start_Broadcast);
      Busy_Wait (Count => 200);
   end Start_AP_Processors;

   -------------------------------------------------------------------------

   procedure Send_IPI
     (Vector  : SK.Byte;
      Apic_Id : SK.Byte)
   is
      ICR_Value : SK.Word64;
   begin
      ICR_Value := SK.Word64 (Apic_Id) * 2 ** 32;
      ICR_Value := ICR_Value + SK.Word64 (Vector);
      Write_ICR (Value => ICR_Value);
   end Send_IPI;

end SK.Apic;
