with System;

package body SK.Hpet
is

   --  HPET at physical address 0xfed00000 is mapped at virtual address
   --  0x1fd000.
   Hpet_Address : constant := 16#1fd000#;

   --  Global Configuration Register, see IA-PC HPET Spec, section 2.3.5.

   GLOBAL_ENABLE_CNF : constant := 0;

   --  Timer N configuration and capability register, see IA-PC HPET Spec,
   --  section 2.3.8.

   Tn_INT_TYPE_CNF    : constant := 1;
   Tn_INT_ENB_CNF     : constant := 2;
   Tn_TYPE_CNF        : constant := 3;
   Tn_VAL_SET_CNF     : constant := 6;
   Tn_FSB_EN_CNF      : constant := 14;

   --  HPET memory map, see IA-PC HPET Specification Rev 1.0a, section 2.1.

   type Flag_Type is mod 2 ** 1;
   for Flag_Type'Size use 1;

   type Timer_Number_Type is mod 2 ** 5;
   for Timer_Number_Type'Size use 5;

   --  General Capability and ID register, see IA-PC HPET Specification Rev
   --  1.0a, section 2.3.4.
   type Capabilities_Register_Type is record
      Rev_Id             : SK.Byte;
      Num_Tim_Cap        : Timer_Number_Type;
      Count_Size_Cap     : Flag_Type;
      Leg_RT_Cap         : Flag_Type;
      Vendor_Id          : SK.Word16;
      Counter_Clk_Period : Counter_Period_Type;
   end record;

   for Capabilities_Register_Type use record
      Rev_Id             at 0 range  0 .. 7;
      Num_Tim_Cap        at 0 range  8 .. 12;
      Count_Size_Cap     at 0 range 13 .. 13;
      Leg_RT_Cap         at 0 range 15 .. 15;
      Vendor_Id          at 0 range 16 .. 31;
      Counter_Clk_Period at 0 range 32 .. 63;
   end record;
   for Capabilities_Register_Type'Size use 64;

   type Timer_Type is record
      Config     : SK.Word64;
      Comparator : SK.Word64;
      IRR        : SK.Word64;
      Reserved   : SK.Word64;
   end record;

   for Timer_Type use record
      Config     at 16#00# range 0 .. 63;
      Comparator at 16#08# range 0 .. 63;
      IRR        at 16#10# range 0 .. 63;
      Reserved   at 16#18# range 0 .. 63;
   end record;
   for Timer_Type'Size use 4 * 64;

   type Timers_Type is array (Timers_Range) of Timer_Type;
   for Timers_Type'Size use Num_Timers * 4 * 64;

   type HPET_Type is record
      Capabilities       : Capabilities_Register_Type;
      Config             : SK.Word64;
      Interrupt_Status   : SK.Word64;
      Main_Counter_Value : SK.Word64;
      Timers             : Timers_Type;
   end record;

   for HPET_Type use record
      Capabilities       at 16#000# range 0 .. 63;
      Config             at 16#010# range 0 .. 63;
      Interrupt_Status   at 16#020# range 0 .. 63;
      Main_Counter_Value at 16#0f0# range 0 .. 63;
      Timers             at 16#100# range 0 .. Num_Timers * 4 * 64 - 1;
   end record;
   for HPET_Type'Alignment use 64;

   Hpet_In : HPET_Type;
   for Hpet_In'Address use System'To_Address (Hpet_Address);
   pragma Volatile (Hpet_In);
   --# assert Hpet_In.Capabilities'Always_Valid;
   --# assert Hpet_In.Config'Always_Valid;
   --# assert Hpet_In.Main_Counter_Value'Always_Valid;
   --# assert Hpet_In.Timers'Always_Valid;

   Hpet_Out : HPET_Type;
   for Hpet_Out'Address use System'To_Address (Hpet_Address);
   pragma Volatile (Hpet_Out);

   -------------------------------------------------------------------------

   function Get_Counter_Period return Counter_Period_Type
   is
   begin

      --# assert
      --#    Hpet_In.Capabilities.Counter_Clk_Period in Counter_Period_Type;

      return Hpet_In.Capabilities.Counter_Clk_Period;
   end Get_Counter_Period;

   -------------------------------------------------------------------------

   function Get_Main_Counter return SK.Word64
   is
   begin
      return Hpet_In.Main_Counter_Value;
   end Get_Main_Counter;

   -------------------------------------------------------------------------

   procedure Set_Main_Counter (Value : SK.Word64)
   is
   begin
      Hpet_Out.Main_Counter_Value := Value;
   end Set_Main_Counter;

   -------------------------------------------------------------------------

   procedure Configure_Timer
     (Id             : Timers_Range;
      Periodic       : Boolean;
      FSB_Mode       : Boolean;
      Interrupt_Type : Interrupt_Kind)
   is
      Cfg : SK.Word64;
   begin
      Cfg := Hpet_In.Timers (Id).Config;

      if Periodic then
         Cfg := SK.Bit_Set (Value => Cfg,
                            Pos   => Tn_TYPE_CNF);
         Cfg := SK.Bit_Set (Value => Cfg,
                            Pos   => Tn_VAL_SET_CNF);
      end if;

      if FSB_Mode then
         Cfg := SK.Bit_Set (Value => Cfg,
                            Pos   => Tn_FSB_EN_CNF);
      end if;

      if Interrupt_Type = Level then
         Cfg := SK.Bit_Set (Value => Cfg,
                            Pos   => Tn_INT_TYPE_CNF);
      end if;

      Hpet_Out.Timers (Id).Config := Cfg;
   end Configure_Timer;

   -------------------------------------------------------------------------

   procedure Mask_Interrupt (Id : Timers_Range)
   is
      Cfg : SK.Word64;
   begin
      Cfg := Hpet_In.Timers (Id).Config;
      Cfg := SK.Bit_Clear (Value => Cfg,
                           Pos   => Tn_INT_ENB_CNF);
      Hpet_Out.Timers (Id).Config := Cfg;
   end Mask_Interrupt;

   -------------------------------------------------------------------------

   procedure Unmask_Interrupt (Id : Timers_Range)
   is
      Cfg : SK.Word64;
   begin
      Cfg := Hpet_In.Timers (Id).Config;
      Cfg := SK.Bit_Set (Value => Cfg,
                         Pos   => Tn_INT_ENB_CNF);
      Hpet_Out.Timers (Id).Config := Cfg;
   end Unmask_Interrupt;

   -------------------------------------------------------------------------

   function Is_Masked (Id : Timers_Range) return Boolean
   is
      Cfg : SK.Word64;
   begin
      Cfg := Hpet_In.Timers (Id).Config;
      return SK.Bit_Test (Value => Cfg,
                          Pos   => Tn_INT_ENB_CNF);
   end Is_Masked;

   -------------------------------------------------------------------------

   procedure Set_Timer
     (Id    : Timers_Range;
      Ticks : SK.Word64)
   is
   begin
      Hpet_Out.Timers (Id).Comparator := Ticks;
   end Set_Timer;

   -------------------------------------------------------------------------

   function Get_Timer (Id : Timers_Range) return SK.Word64
   is
      Value : SK.Word64;
   begin
      Value := Hpet_In.Timers (Id).Comparator;
      return Value;
   end Get_Timer;

   -------------------------------------------------------------------------

   procedure Set_FSB_Route
     (Id             : Timers_Range;
      Destination_Id : SK.Byte;
      Vector         : SK.Byte)
   is
      FSB_Val : SK.Word64;
      MSI_Prefix : constant SK.Word64 := 16#fee00000#;
   begin
      FSB_Val := (2 ** 32) * (MSI_Prefix + SK.Word64 (Destination_Id))
        + SK.Word64 (Vector);
      Hpet_Out.Timers (Id).IRR := FSB_Val;
   end Set_FSB_Route;

   -------------------------------------------------------------------------

   procedure Enable

   is
      Value : SK.Word64;
   begin
      Value := Hpet_In.Config;
      Value := SK.Bit_Set (Value => Value,
                           Pos   => GLOBAL_ENABLE_CNF);
      Hpet_Out.Config := Value;
   end Enable;

end SK.Hpet;
