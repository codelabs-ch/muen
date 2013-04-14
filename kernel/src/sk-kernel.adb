with SK.KC;
with SK.Version;
with SK.Interrupts;
with SK.System_State;
with SK.VMX;
with SK.Scheduler;
with SK.Apic;
with SK.CPU;
with SK.MP;
with SK.Locks;
with SK.IO;
with SK.Hpet;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   --  Arm the Hpet timer to fire every second.
   procedure Arm_Hpet_Timer
   --# global
   --#    in     X86_64.State;
   --#    in     Hpet.Hpet_In;
   --#       out Hpet.Hpet_Out;
   --# derives
   --#    Hpet.Hpet_Out from Hpet.Hpet_In, X86_64.State;
   is
      Sec_To_Femto : constant SK.Word64 := 10 ** 15;
      Period       : SK.Word32;
      Timer_Ticks  : SK.Word64;
   begin

      --  Setup timer 0 in FSB mode.

      Hpet.Configure_Timer (Id             => 1,
                            Periodic       => True,
                            FSB_Mode       => True,
                            Interrupt_Type => Hpet.Edge);

      --  Deliver timer interrupt to local APIC as vector 32.

      Hpet.Set_FSB_Route (Id             => 1,
                          Destination_Id => Apic.Get_ID,
                          Vector         => 32);

      --  Calculate ticks: convert second to femto and divide by period.

      Period := Hpet.Get_Counter_Period;
      Timer_Ticks := (1 * Sec_To_Femto) /  SK.Word64 (Period);

      Hpet.Set_Timer (Id    => 1,
                      Ticks => Timer_Ticks);

      Hpet.Unmask_Interrupt (Id => 1);
      Hpet.Set_Main_Counter (Value => 0);
   end Arm_Hpet_Timer;

   -------------------------------------------------------------------------

   --  Mask all interrupts in the legacy PIC.
   procedure Disable_Legacy_PIC
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;
   is
   begin

      --  Disable slave.

      IO.Outb (Port  => 16#a1#,
               Value => 16#ff#);

      --  Disable master.

      IO.Outb (Port  => 16#21#,
               Value => 16#ff#);
   end Disable_Legacy_PIC;

   -------------------------------------------------------------------------

   procedure Main
   is
      Success, Is_Bsp : Boolean;
   begin
      MP.Increment_CPU_Count;

      Is_Bsp := Apic.Is_BSP;
      if Is_Bsp then
         pragma Debug (KC.Init);
         pragma Debug (KC.Put_Line
           (Item => "Booting Separation Kernel ("
            & SK.Version.Version_String & ") ..."));
         Disable_Legacy_PIC;
      else
         Apic.Enable;

         Locks.Spin_Lock;
         pragma Debug (KC.Put_String (Item => "CPU"));
         pragma Debug (KC.Put_Byte   (Item => Apic.Get_ID));
         pragma Debug (KC.Put_Line
                       (Item => ": AP online -> halting"));
         Locks.Unlock;
         CPU.Hlt;
      end if;

      --  Setup IDT.

      Interrupts.Init;
      Interrupts.Load;

      Success := System_State.Is_Valid;
      if Success then

         pragma Debug (KC.Put_Line (Item => "Starting AP processors"));
         Apic.Enable;
         Apic.Start_AP_Processors;
         MP.Wait_For_AP_Processors;

         pragma Debug (KC.Put_String (Item => "Processors online: "));
         pragma Debug (KC.Put_Byte   (Item => MP.Get_CPU_Count));
         pragma Debug (KC.New_Line);

         VMX.Enable;
         Hpet.Enable;
         Arm_Hpet_Timer;
         Scheduler.Schedule;
      else
         pragma Debug (KC.Put_Line (Item => "System initialisation error"));
         null;
      end if;

      pragma Debug (KC.Put_Line (Item => "Terminating"));
   end Main;

end SK.Kernel;
