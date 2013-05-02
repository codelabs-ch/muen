with System;

with Skp.Interrupts;

with SK.VMX;
with SK.Constants;
with SK.KC;
with SK.CPU;
with SK.CPU_Global;
with SK.Subjects;
with SK.Apic;
with SK.MP;

package body SK.Scheduler
--# own
--#    State is in New_Major, Current_Major, Scheduling_Plan;
is

   --  Dumper subject id.
   Dumper_Id : constant := 1;

   --  Configured scheduling plan.
   Scheduling_Plan : Skp.Scheduling.Scheduling_Plan_Type;

   Tau0_Kernel_Iface_Address : SK.Word64;
   pragma Import (C, Tau0_Kernel_Iface_Address, "tau0kernel_iface_ptr");

   New_Major : Skp.Scheduling.Major_Frame_Range;
   for New_Major'Address use System'To_Address (Tau0_Kernel_Iface_Address);
   --# assert New_Major'Always_Valid;

   --  Current major.
   Current_Major : Skp.Scheduling.Major_Frame_Range
     := Skp.Scheduling.Major_Frame_Range'First;

   subtype Ext_Int_Type is SK.Byte range 32 .. 255;

   -------------------------------------------------------------------------

   --  Return CPU scheduling ID.
   procedure Get_ID (ID : out Skp.CPU_Range)
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from * &
   --#    ID           from X86_64.State;
   is
      Apic_ID : SK.Byte;
   begin
      ID      := 0;
      Apic_ID := Apic.Get_ID;

      if Apic_ID > SK.Byte (Skp.CPU_Range'Last) then
         pragma Debug (KC.Put_String (Item => "CPU ID not in range: "));
         pragma Debug (KC.Put_Byte   (Item => Apic_ID));
         pragma Debug (KC.New_Line);
         CPU.Panic;
      else
         ID := Skp.CPU_Range (Apic_ID);
      end if;
   end Get_ID;

   -------------------------------------------------------------------------

   --  Remove subject specified by Old_Id from the scheduling plan and replace
   --  it with the subject given by New_Id.
   procedure Swap_Subject (Old_Id, New_Id : Skp.Subject_Id_Type)
   --# global
   --#    in out X86_64.State;
   --#    in out Scheduling_Plan;
   --# derives
   --#    X86_64.State    from *, Old_Id, New_Id &
   --#    Scheduling_Plan from *, Old_Id, New_Id, X86_64.State;
   is
      CPU_ID : Skp.CPU_Range;
   begin
      Get_ID (ID => CPU_ID);

      if Old_Id = New_Id then
         pragma Debug (KC.Put_String (Item => "Scheduling error: subject "));
         pragma Debug (KC.Put_Byte   (Item => Byte (Old_Id)));
         pragma Debug (KC.Put_Line   (Item => " swap to self"));
         CPU.Panic;
      end if;

      for I in Skp.Scheduling.Major_Frame_Range loop
         for J in Skp.Scheduling.Minor_Frame_Range loop
            if Scheduling_Plan (I).CPUs (CPU_ID).Minor_Frames
              (J).Subject_Id = Old_Id
            then
               Scheduling_Plan (I).CPUs (CPU_ID).Minor_Frames
                 (J).Subject_Id := New_Id;
            end if;
         end loop;
      end loop;
   end Swap_Subject;

   -------------------------------------------------------------------------

   --  Read VMCS fields and store them in the given subject state.
   procedure Store_Subject_Info (State : in out SK.Subject_State_Type)
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from * &
   --#    State        from *, X86_64.State;
   is
   begin
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_QUALIFICATION,
                     Value => State.Exit_Qualification);
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INTR_INFO,
                     Value => State.Interrupt_Info);

      VMX.VMCS_Read (Field => Constants.GUEST_RIP,
                     Value => State.RIP);
      VMX.VMCS_Read (Field => Constants.GUEST_SEL_CS,
                     Value => State.CS);
      VMX.VMCS_Read (Field => Constants.GUEST_RSP,
                     Value => State.RSP);
      VMX.VMCS_Read (Field => Constants.GUEST_SEL_SS,
                     Value => State.SS);
      VMX.VMCS_Read (Field => Constants.GUEST_CR0,
                     Value => State.CR0);
      VMX.VMCS_Read (Field => Constants.GUEST_CR3,
                     Value => State.CR3);
      VMX.VMCS_Read (Field => Constants.GUEST_CR4,
                     Value => State.CR4);
      VMX.VMCS_Read (Field => Constants.GUEST_RFLAGS,
                     Value => State.RFLAGS);
   end Store_Subject_Info;

   -------------------------------------------------------------------------

   --  Update scheduling information. If the end of the current major frame is
   --  reached, the minor frame index is reset and the major frame is switched
   --  to the one set by Tau0. Otherwise the minor frame index is incremented
   --  by 1.
   procedure Update_Scheduling_Info
   --# global
   --#    in     New_Major;
   --#    in     Scheduling_Plan;
   --#    in out Current_Major;
   --#    in out CPU_Global.Storage;
   --#    in out X86_64.State;
   --#    in out MP.Barrier;
   --# derives
   --#    X86_64.State from * &
   --#    MP.Barrier   from
   --#       *,
   --#       Current_Major,
   --#       Scheduling_Plan,
   --#       CPU_Global.Storage,
   --#       X86_64.State &
   --#    Current_Major, CPU_Global.Storage from
   --#       Current_Major,
   --#       CPU_Global.Storage,
   --#       New_Major,
   --#       Scheduling_Plan,
   --#       X86_64.State;
   is
      CPU_ID      : Skp.CPU_Range;
      Minor_Frame : CPU_Global.Active_Minor_Frame_Type;
   begin
      Get_ID (ID => CPU_ID);
      Minor_Frame := CPU_Global.Get_Current_Minor_Frame;

      if Minor_Frame.Ticks = 0 then

         --  Minor frame ticks consumed, advance to next minor frame.

         if Minor_Frame.Id < Scheduling_Plan
           (Current_Major).CPUs (CPU_ID).Length
         then

            --  Switch to next minor frame in current major frame.

            Minor_Frame.Id := Minor_Frame.Id + 1;
         else

            --  Switch to first minor frame in next major frame.

            Minor_Frame.Id := Skp.Scheduling.Minor_Frame_Range'First;

            MP.Wait_For_All;
            if Apic.Is_BSP then
               Current_Major := New_Major;
            end if;
            MP.Wait_For_All;
         end if;

         Minor_Frame.Ticks := Scheduling_Plan (Current_Major).CPUs
           (CPU_ID).Minor_Frames (Minor_Frame.Id).Ticks;
         CPU_Global.Set_Current_Minor (Frame => Minor_Frame);
      end if;
   end Update_Scheduling_Info;

   -------------------------------------------------------------------------

   procedure Handle_Hypercall
     (Current_Subject :        Skp.Subject_Id_Type;
      Subject_State   : in out SK.Subject_State_Type)
   --# global
   --#    in out X86_64.State;
   --#    in out Subjects.Descriptors;
   --#    in out Scheduling_Plan;
   --# derives
   --#    X86_64.State from
   --#       *,
   --#       Current_Subject,
   --#       Subject_State &
   --#    Scheduling_Plan from
   --#       *,
   --#       Current_Subject,
   --#       Subject_State,
   --#       X86_64.State &
   --#    Subject_State        from * &
   --#    Subjects.Descriptors from *, Subject_State;
   is
      New_Subject : Skp.Subject_Id_Type;
   begin
      if Subject_State.Regs.RAX <= SK.Word64 (Skp.Subject_Id_Type'Last) then
         New_Subject := Skp.Subject_Id_Type (Subject_State.Regs.RAX);
         Subjects.Set_State (Id    => New_Subject,
                             State => SK.Null_Subject_State);

         Swap_Subject
           (Old_Id => Current_Subject,
            New_Id => New_Subject);
         Subject_State := SK.Null_Subject_State;
      else
         pragma Debug (KC.Put_String ("Invalid hypercall parameter"));
         CPU.Panic;
      end if;
   end Handle_Hypercall;

   -------------------------------------------------------------------------

   --  Handle external interrupt request with given vector.
   procedure Handle_Irq (Vector : SK.Byte)
   --# global
   --#    in out X86_64.State;
   --#    in out Subjects.Descriptors;
   --# derives
   --#    Subjects.Descriptors from *, Vector &
   --#    X86_64.State         from *;
   is
   begin
      if Vector >= Ext_Int_Type'First then
         if Skp.Interrupts.Vector_Routing
           (Vector) not in Skp.Subject_Id_Type
         then
            pragma Debug (KC.Put_String (Item => "Spurious IRQ vector "));
            pragma Debug (KC.Put_Byte (Item => Vector));
            pragma Debug (KC.New_Line);
            null;
         else
            Subjects.Set_Pending_Event
              (Id     => Skp.Interrupts.Vector_Routing (Vector),
               Vector => Vector);
         end if;
      else
         pragma Debug (KC.Put_String (Item => "IRQ with invalid vector "));
         pragma Debug (KC.Put_Byte (Item => Vector));
         pragma Debug (KC.New_Line);
         null;
      end if;

      Apic.EOI;
   end Handle_Irq;

   -------------------------------------------------------------------------

   procedure Schedule
   --# global
   --#    in     VMX.State;
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     Current_Major;
   --#    in     Scheduling_Plan;
   --#    in out CPU_Global.Storage;
   --#    in out X86_64.State;
   --#    in out Subjects.Descriptors;
   --# derives
   --#    CPU_Global.Storage from
   --#       *,
   --#       Current_Major,
   --#       Scheduling_Plan,
   --#       Subjects.Descriptors,
   --#       X86_64.State &
   --#    Subjects.Descriptors from
   --#       *,
   --#       Current_Major,
   --#       Scheduling_Plan,
   --#       CPU_Global.Storage,
   --#       X86_64.State &
   --#    X86_64.State from
   --#       *,
   --#       VMX.State,
   --#       GDT.GDT_Pointer,
   --#       Interrupts.IDT_Pointer,
   --#       Subjects.Descriptors,
   --#       CPU_Global.Storage,
   --#       Current_Major,
   --#       Scheduling_Plan;
   is
      CPU_ID        : Skp.CPU_Range;
      Plan_Frame    : Skp.Scheduling.Minor_Frame_Type;
      Current_Frame : CPU_Global.Active_Minor_Frame_Type;
   begin
      Get_ID (ID => CPU_ID);

      Current_Frame := CPU_Global.Get_Current_Minor_Frame;
      Plan_Frame    := Scheduling_Plan (Current_Major).CPUs
        (CPU_ID).Minor_Frames (Current_Frame.Id);

      if Subjects.Get_State (Id => Plan_Frame.Subject_Id).Launched then
         VMX.Resume (Subject_Id => Plan_Frame.Subject_Id,
                     Time_Slice => Current_Frame.Ticks);
      else
         VMX.Launch (Subject_Id => Plan_Frame.Subject_Id,
                     Time_Slice => Plan_Frame.Ticks);
      end if;
   end Schedule;

   -------------------------------------------------------------------------

   procedure Handle_Vmx_Exit (Subject_Registers : SK.CPU_Registers_Type)
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     VMX.State;
   --#    in     New_Major;
   --#    in out CPU_Global.Storage;
   --#    in out Scheduling_Plan;
   --#    in out Current_Major;
   --#    in out MP.Barrier;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    CPU_Global.Storage from
   --#       *,
   --#       New_Major,
   --#       Current_Major,
   --#       Scheduling_Plan,
   --#       Subject_Registers,
   --#       Subjects.Descriptors,
   --#       X86_64.State &
   --#    Current_Major from
   --#       *,
   --#       New_Major,
   --#       Scheduling_Plan,
   --#       Subject_Registers,
   --#       Subjects.Descriptors,
   --#       CPU_Global.Storage,
   --#       X86_64.State &
   --#    Scheduling_Plan, MP.Barrier from
   --#       *,
   --#       Scheduling_Plan,
   --#       Current_Major,
   --#       Subject_Registers,
   --#       Subjects.Descriptors,
   --#       CPU_Global.Storage,
   --#       X86_64.State &
   --#    X86_64.State from
   --#       *,
   --#       Subject_Registers,
   --#       New_Major,
   --#       Current_Major,
   --#       Scheduling_Plan,
   --#       VMX.State,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       CPU_Global.Storage,
   --#       Subjects.Descriptors &
   --#    Subjects.Descriptors from
   --#       *,
   --#       CPU_Global.Storage,
   --#       X86_64.State,
   --#       Subject_Registers,
   --#       New_Major,
   --#       Current_Major,
   --#       Scheduling_Plan;
   is
      CPU_ID          : Skp.CPU_Range;
      State           : SK.Subject_State_Type;
      Current_Subject : Skp.Subject_Id_Type;
      Current_Minor   : CPU_Global.Active_Minor_Frame_Type;
      Timer_Value     : SK.Word64;
   begin
      Get_ID (ID => CPU_ID);
      Current_Minor := CPU_Global.Get_Current_Minor_Frame;

      Current_Subject := Scheduling_Plan (Current_Major).CPUs
        (CPU_ID).Minor_Frames (Current_Minor.Id).Subject_Id;
      State           := Subjects.Get_State (Id => Current_Subject);
      State.Regs      := Subject_Registers;

      VMX.VMCS_Read (Field => Constants.VMX_EXIT_REASON,
                     Value => State.Exit_Reason);
      VMX.VMCS_Read (Field => Constants.GUEST_VMX_PREEMPT_TIMER,
                     Value => Timer_Value);
      Current_Minor.Ticks := SK.Word32'Mod (Timer_Value);
      CPU_Global.Set_Current_Minor (Frame => Current_Minor);

      Store_Subject_Info (State => State);

      if SK.Bit_Test (Value => State.Exit_Reason,
                      Pos   => Constants.VM_EXIT_ENTRY_FAILURE)
      then
         pragma Debug (KC.Put_String (Item => "Subject "));
         pragma Debug (KC.Put_Byte   (Item =>  Byte (Current_Subject)));
         pragma Debug (KC.Put_String (Item => " VM-entry failure ("));
         pragma Debug (KC.Put_Word16 (Item => Word16 (State.Exit_Reason)));
         pragma Debug (KC.Put_String (Item => ":"));
         pragma Debug (KC.Put_Word32
                       (Item => Word32 (State.Exit_Qualification)));
         pragma Debug (KC.New_Line);
         CPU.Panic;
      end if;

      if State.Exit_Reason = Constants.VM_EXIT_EXTERNAL_INT then
         Handle_Irq (Vector => SK.Byte'Mod (State.Interrupt_Info));
      elsif State.Exit_Reason = Constants.VM_EXIT_HYPERCALL then
         Handle_Hypercall (Current_Subject => Current_Subject,
                           Subject_State   => State);
      elsif State.Exit_Reason /= Constants.VM_EXIT_TIMER_EXPIRY then

         --  Abnormal subject exit, schedule dumper.

         Swap_Subject (Old_Id => Current_Subject,
                       New_Id => Dumper_Id);
      end if;

      Subjects.Set_State (Id    => Current_Subject,
                          State => State);

      Update_Scheduling_Info;
      Schedule;
   end Handle_Vmx_Exit;

begin
   Scheduling_Plan := Skp.Scheduling.Scheduling_Plans;
end SK.Scheduler;
