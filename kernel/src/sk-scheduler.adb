with System;

with SK.VMX;
with SK.Constants;
with SK.KC;
with SK.CPU;
with SK.Subjects;
with SK.Apic;

package body SK.Scheduler
--# own
--#    State is in New_Major, Current_Major, Current_Minors, Scheduling_Plan;
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

   --  Current per-CPU minor frames.
   type Current_Minor_Array is array (Skp.Scheduling.CPU_Range)
     of Skp.Scheduling.Minor_Frame_Range;

   Current_Minors : Current_Minor_Array := Current_Minor_Array'
     (others => Skp.Scheduling.Minor_Frame_Range'First);

   -------------------------------------------------------------------------

   --  Return CPU scheduling ID.
   procedure Get_ID (ID : out Skp.Scheduling.CPU_Range)
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

      if Apic_ID > SK.Byte (Skp.Scheduling.CPU_Range'Last) then
         pragma Debug (KC.Put_String (Item => "CPU ID not in range: "));
         pragma Debug (KC.Put_Byte   (Item => Apic_ID));
         pragma Debug (KC.New_Line);
         CPU.Panic;
      else
         ID := Skp.Scheduling.CPU_Range (Apic_ID);
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
      CPU_ID : Skp.Scheduling.CPU_Range;
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
   --#    in out Current_Minors;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from * &
   --#    Current_Minors from
   --#       * ,
   --#       Current_Major,
   --#       Scheduling_Plan,
   --#       X86_64.State &
   --#    Current_Major from
   --#       *,
   --#       Current_Minors,
   --#       New_Major,
   --#       Scheduling_Plan,
   --#       X86_64.State;
   is
      CPU_ID : Skp.Scheduling.CPU_Range;
   begin
      Get_ID (ID => CPU_ID);

      if Current_Minors (CPU_ID) < Scheduling_Plan
        (Current_Major).CPUs (CPU_ID).Length
      then

         --# assert
         --#    CPU_ID in Skp.Scheduling.CPU_Range      and
         --#    Current_Minors (CPU_ID) < Scheduling_Plan
         --#       (Current_Major).CPUs (CPU_ID).Length and
         --#    Scheduling_Plan (Current_Major).CPUs (CPU_ID).Length
         --#       <= Skp.Scheduling.Minor_Frame_Range'Last;

         Current_Minors (CPU_ID) := Current_Minors (CPU_ID) + 1;
      else
         Current_Minors (CPU_ID) := Skp.Scheduling.Minor_Frame_Range'First;
         Current_Major := New_Major;
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

   procedure Schedule
   --# global
   --#    in     VMX.State;
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     Current_Major;
   --#    in     Current_Minors;
   --#    in     Scheduling_Plan;
   --#    in out X86_64.State;
   --#    in out Subjects.Descriptors;
   --# derives
   --#    Subjects.Descriptors from
   --#       *,
   --#       Current_Major,
   --#       Current_Minors,
   --#       Scheduling_Plan,
   --#       X86_64.State &
   --#    X86_64.State from
   --#       *,
   --#       VMX.State,
   --#       GDT.GDT_Pointer,
   --#       Interrupts.IDT_Pointer,
   --#       Subjects.Descriptors,
   --#       Current_Major,
   --#       Current_Minors,
   --#       Scheduling_Plan;
   is
      CPU_ID        : Skp.Scheduling.CPU_Range;
      Current_Frame : Skp.Scheduling.Minor_Frame_Type;
   begin
      Get_ID (ID => CPU_ID);

      Current_Frame := Scheduling_Plan (Current_Major).CPUs
        (CPU_ID).Minor_Frames (Current_Minors (CPU_ID));

      if Subjects.Get_State (Id => Current_Frame.Subject_Id).Launched then
         VMX.Resume (Subject_Id => Current_Frame.Subject_Id,
                     Time_Slice => Current_Frame.Ticks);
      else
         VMX.Launch (Subject_Id => Current_Frame.Subject_Id,
                     Time_Slice => Current_Frame.Ticks);
      end if;
   end Schedule;

   -------------------------------------------------------------------------

   procedure Handle_Vmx_Exit (Subject_Registers : SK.CPU_Registers_Type)
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     VMX.State;
   --#    in     New_Major;
   --#    in out Scheduling_Plan;
   --#    in out Current_Major;
   --#    in out Current_Minors;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    Current_Major from
   --#       *,
   --#       Current_Minors,
   --#       New_Major,
   --#       Scheduling_Plan,
   --#       Subject_Registers,
   --#       Subjects.Descriptors,
   --#       X86_64.State  &
   --#    Current_Minors from
   --#       *,
   --#       Current_Major,
   --#       Scheduling_Plan,
   --#       Subject_Registers,
   --#       Subjects.Descriptors,
   --#       X86_64.State &
   --#    Scheduling_Plan from
   --#       *,
   --#       Current_Major,
   --#       Current_Minors,
   --#       Subject_Registers,
   --#       Subjects.Descriptors,
   --#       X86_64.State &
   --#    X86_64.State from
   --#       *,
   --#       Subject_Registers,
   --#       New_Major,
   --#       Current_Major,
   --#       Current_Minors,
   --#       Scheduling_Plan,
   --#       VMX.State,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       Subjects.Descriptors &
   --#    Subjects.Descriptors from
   --#       *,
   --#       X86_64.State,
   --#       Subject_Registers,
   --#       New_Major,
   --#       Current_Major,
   --#       Current_Minors,
   --#       Scheduling_Plan;
   is
      CPU_ID          : Skp.Scheduling.CPU_Range;
      State           : SK.Subject_State_Type;
      Current_Subject : Skp.Subject_Id_Type;
   begin
      Get_ID (ID => CPU_ID);

      Current_Subject := Scheduling_Plan (Current_Major).CPUs
        (CPU_ID).Minor_Frames (Current_Minors (CPU_ID)).Subject_Id;
      State           := Subjects.Get_State (Id => Current_Subject);
      State.Regs      := Subject_Registers;

      VMX.VMCS_Read (Field => Constants.VMX_EXIT_REASON,
                     Value => State.Exit_Reason);

      if State.Exit_Reason = Constants.VM_EXIT_HYPERCALL then
         Handle_Hypercall (Current_Subject => Current_Subject,
                           Subject_State   => State);
      elsif State.Exit_Reason /= Constants.VM_EXIT_TIMER_EXPIRY then

         --  Abnormal subject exit, schedule dumper.

         Store_Subject_Info (State => State);
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
