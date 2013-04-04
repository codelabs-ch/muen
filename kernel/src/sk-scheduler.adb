with System;

with Skp;

with SK.VMX;
with SK.Constants;
with SK.KC;
with SK.CPU;
with SK.Dump;
with SK.Subjects;

package body SK.Scheduler
--# own
--#    State is in New_Major, Current_Major, Current_Minor, Scheduling_Plan;
is

   --  The minor frame range specifies the number of minor frames that
   --  constitute a major frame.
   type Minor_Frame_Range is mod 2 ** 2;

   --  A major frame specifies which subject to schedule in which minor frame.
   type Major_Frame_Type is array (Minor_Frame_Range) of Skp.Subject_Id_Type;

   --  A scheduling plan consists of multiple major frames.
   type Scheduling_Plan_Type is
     array (SK.Major_Frame_Range) of Major_Frame_Type;

   --  Dumper subject id.
   Dumper_Id : constant := 1;

   --  Configured scheduling plan.
   Scheduling_Plan : Scheduling_Plan_Type;

   Tau0_Kernel_Iface_Address : SK.Word64;
   pragma Import (C, Tau0_Kernel_Iface_Address, "tau0kernel_iface_ptr");

   New_Major : SK.Major_Frame_Range;
   for New_Major'Address use System'To_Address (Tau0_Kernel_Iface_Address);
   --# assert New_Major'Always_Valid;

   --  Current major, minor frame.
   Current_Major : SK.Major_Frame_Range := SK.Major_Frame_Range'First;
   Current_Minor : Minor_Frame_Range    := Minor_Frame_Range'First;

   -------------------------------------------------------------------------

   --  Remove subject specified by Old_Id from the scheduling plan and replace
   --  it with the subject given by New_Id.
   procedure Swap_Subject (Old_Id, New_Id : Skp.Subject_Id_Type)
   --# global
   --#    in out X86_64.State;
   --#    in out Scheduling_Plan;
   --# derives
   --#    X86_64.State, Scheduling_Plan from *, Old_Id, New_Id;
   is
   begin
      if Old_Id = New_Id then
         pragma Debug (KC.Put_String (Item => "Scheduling error: subject "));
         pragma Debug (KC.Put_Byte   (Item => Byte (Old_Id)));
         pragma Debug (KC.Put_Line   (Item => " swap to self"));
         CPU.Panic;
      end if;

      for I in SK.Major_Frame_Range loop
         for J in Minor_Frame_Range loop
            if Scheduling_Plan (I)(J) = Old_Id then
               Scheduling_Plan (I)(J) := New_Id;
            end if;
         end loop;
      end loop;
   end Swap_Subject;

   -------------------------------------------------------------------------

   procedure Schedule
   --# global
   --#    in     VMX.State;
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     Current_Major;
   --#    in     Current_Minor;
   --#    in     Scheduling_Plan;
   --#    in out X86_64.State;
   --#    in out Subjects.Descriptors;
   --# derives
   --#    Subjects.Descriptors from
   --#       *,
   --#       Current_Major,
   --#       Current_Minor,
   --#       Scheduling_Plan &
   --#    X86_64.State from
   --#       *,
   --#       VMX.State,
   --#       GDT.GDT_Pointer,
   --#       Interrupts.IDT_Pointer,
   --#       Subjects.Descriptors,
   --#       Current_Major,
   --#       Current_Minor,
   --#       Scheduling_Plan;
   is
      Current_Subject : Skp.Subject_Id_Type;
   begin
      Current_Subject := Scheduling_Plan (Current_Major) (Current_Minor);

      if Subjects.Get_State (Id => Current_Subject).Launched then
         VMX.Resume (Subject_Id => Current_Subject);
      else
         VMX.Launch (Subject_Id => Current_Subject);
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
   --#    in out Current_Minor;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    Current_Major   from New_Major                                     &
   --#    Current_Minor   from *                                             &
   --#    Scheduling_Plan from *, Current_Major, Current_Minor, X86_64.State &
   --#    X86_64.State    from
   --#       *,
   --#       Subject_Registers,
   --#       New_Major,
   --#       Current_Major,
   --#       Current_Minor,
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
   --#       Current_Minor,
   --#       Scheduling_Plan;
   is
      State           : SK.Subject_State_Type;
      Current_Subject : Skp.Subject_Id_Type;
   begin
      Current_Subject := Scheduling_Plan (Current_Major) (Current_Minor);
      State           := Subjects.Get_State (Id => Current_Subject);
      State.Regs      := Subject_Registers;

      VMX.VMCS_Read (Field => Constants.VMX_EXIT_REASON,
                     Value => State.Exit_Reason);
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_QUALIFICATION,
                     Value => State.Exit_Qualification);

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

      if State.Exit_Reason /= Constants.VM_EXIT_TIMER_EXPIRY then
         if State.Exit_Reason = Constants.VM_EXIT_EXCEPTION_NMI then
            VMX.VMCS_Read (Field => Constants.VMX_EXIT_INTR_INFO,
                           Value => State.Interrupt_Info);
            Swap_Subject (Old_Id => Current_Subject,
                          New_Id => Dumper_Id);
         else
            pragma Debug (Dump.Print_State (Subject => Current_Subject));
            CPU.Hlt;
         end if;
      end if;

      Subjects.Set_State (Id    => Current_Subject,
                          State => State);

      Current_Major := New_Major;
      Current_Minor := Current_Minor + 1;
      Schedule;
   end Handle_Vmx_Exit;

begin
   Scheduling_Plan := Scheduling_Plan_Type'
     (0 => Major_Frame_Type'(0, 0, 0, 0),
      1 => Major_Frame_Type'(0, 2, 3, 0));
end SK.Scheduler;
