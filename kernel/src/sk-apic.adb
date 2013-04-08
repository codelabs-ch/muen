with SK.CPU;
with SK.Constants;

package body SK.Apic
is

   ENABLE_APIC         : constant := 8;
   ENABLE_X2_MODE_FLAG : constant := 10;

   APIC_BSP_FLAG : constant := 8;

   MSR_X2APIC_ID  : constant := 16#802#;
   MSR_X2APIC_SVR : constant := 16#80f#;
   MSR_X2APIC_ICR : constant := 16#830#;

   Ipi_Init_Broadcast  : constant := 16#000c4500#;
   Ipi_Start_Broadcast : constant := 16#000c4600#;

   -------------------------------------------------------------------------

   --  Busy-sleep for a given (scaled) period of time.
   procedure Sleep (Count : Positive)
   --# derives null from Count;
   is
      --# hide Sleep;
   begin
      for I in Integer range 1 .. Count * (2 ** 8) loop
         null;
      end loop;
   end Sleep;

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

      --# accept Flow, 10, Unused, "Result unused";

      CPU.Get_MSR (Register => MSR_X2APIC_ID,
                   Low      => ID,
                   High     => Unused);

      --# accept Flow, 33, Unused, "Result unused";

      return SK.Byte'Mod (ID);
   end Get_ID;

   -------------------------------------------------------------------------

   function Is_BSP return Boolean
   is
      Base : SK.Word64;
   begin
      Base := CPU.Get_MSR64 (Register => Constants.IA32_APIC_BASE);
      return SK.Bit_Test (Value => Base,
                          Pos   => APIC_BSP_FLAG);
   end Is_BSP;

   -------------------------------------------------------------------------

   procedure Start_AP_Processors
   is
   begin
      CPU.Write_MSR (Register => MSR_X2APIC_ICR,
                     Low      => Ipi_Init_Broadcast,
                     High     => 0);
      Sleep (Count => 10);

      CPU.Write_MSR (Register => MSR_X2APIC_ICR,
                     Low      => Ipi_Start_Broadcast,
                     High     => 0);
      Sleep (Count => 200);

      CPU.Write_MSR (Register => MSR_X2APIC_ICR,
                     Low      => Ipi_Start_Broadcast,
                     High     => 0);
      Sleep (Count => 200);
   end Start_AP_Processors;

end SK.Apic;
