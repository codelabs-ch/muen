--# inherit
--#    X86_64,
--#    SK.Apic;
package SK.Hpet
--# own in Hpet_In, out Hpet_Out;
is

   --  Number of available timer.
   Num_Timers : constant := 8;

   type Timers_Range is range 1 .. Num_Timers;

   --  Interrupt trigger type.
   type Interrupt_Kind is (Level, Edge);

   --  Initialize and enable HPET timer.
   procedure Enable;
   --# global
   --#    in     Hpet_In;
   --#       out Hpet_Out;
   --# derives
   --#    Hpet_Out from Hpet_In;

   --  Returns the main HPET counter period in at which the counter increments
   --  in femptoseconds (10^-15 [s]).
   function Get_Counter_Period return SK.Word32;
   --# global
   --#    in Hpet_In;

   --  Sets the main HPET counter to the specified value.
   procedure Set_Main_Counter (Value : SK.Word64);
   --# global
   --#    out Hpet_Out;
   --# derives
   --#    Hpet_Out from Value;

   --  Returns the current value of the main HPET counter.
   function Get_Main_Counter return SK.Word64;
   --# global
   --#    in Hpet_In;

   --  Configure timer with given id and specified parameters.
   procedure Configure_Timer
     (Id             : Timers_Range;
      Periodic       : Boolean;
      FSB_Mode       : Boolean;
      Interrupt_Type : Interrupt_Kind);
   --# global
   --#    in     Hpet_In;
   --#       out Hpet_Out;
   --# derives
   --#    Hpet_Out from
   --#       Hpet_In,
   --#       Id,
   --#       Periodic,
   --#       FSB_Mode,
   --#       Interrupt_Type;

   --  Set destination (APIC) id and vector number for timer specified by id.
   procedure Set_FSB_Route
     (Id             : Timers_Range;
      Destination_Id : SK.Byte;
      Vector         : SK.Byte);
   --# global
   --#    out Hpet_Out;
   --# derives
   --#    Hpet_Out from Id, Destination_Id, Vector;

   --  Set timer specified by id to fire in specified amout of counter ticks.
   --  To set a specific time (in femtoseconds) the ticks must be divided by
   --  the counter period and added to the current counter value.
   procedure Set_Timer
     (Id    : Timers_Range;
      Ticks : SK.Word64);
   --# global
   --#    out Hpet_Out;
   --# derives
   --#    Hpet_Out from Id, Ticks;

      --  Get comparator value of timer with given id.
   function Get_Timer (Id : Timers_Range) return SK.Word64;
   --# global
   --#    in Hpet_In;

   --  Mask/disable interrupt delivery for timer with given id.
   procedure Mask_Interrupt (Id : Timers_Range);
   --# global
   --#    in     Hpet_In;
   --#       out Hpet_Out;
   --# derives
   --#    Hpet_Out from Hpet_In, Id;

   --  Unmask/enable interrupt delivery for timer with given id.
   procedure Unmask_Interrupt (Id : Timers_Range);
   --# global
   --#    in     Hpet_In;
   --#       out Hpet_Out;
   --# derives
   --#    Hpet_Out from Hpet_In, Id;

   --  Returns true if the interrupt for timer specified by id is masked.
   function Is_Masked (Id : Timers_Range) return Boolean;
   --# global
   --#    in Hpet_In;

end SK.Hpet;
