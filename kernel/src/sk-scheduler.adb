with System;

with SK.VMX;
with SK.Constants;
with SK.KC;
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
   type Major_Frame_Type is array (Minor_Frame_Range)
     of Skp.Subjects.Subject_Id_Type;

   --  A scheduling plan consists of multiple major frames.
   type Scheduling_Plan_Type is
     array (SK.Major_Frame_Range) of Major_Frame_Type;

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
      Current_Subject : Skp.Subjects.Subject_Id_Type;
   begin
      Current_Subject := Scheduling_Plan (Current_Major) (Current_Minor);

      if Subjects.Get_State (Id => Current_Subject).Launched then
         VMX.Resume (Subject_Id => Current_Subject);
      else
         VMX.Launch (Subject_Id => Current_Subject);
      end if;
   end Schedule;

   -------------------------------------------------------------------------

   procedure Handle_Vmx_Exit (Subject_Registers : CPU.Registers_Type)
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     VMX.State;
   --#    in     New_Major;
   --#    in     Scheduling_Plan;
   --#    in out Current_Major;
   --#    in out Current_Minor;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    Current_Major from New_Major &
   --#    Current_Minor from *         &
   --#    X86_64.State  from
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
   --#       Subject_Registers,
   --#       New_Major,
   --#       Current_Major,
   --#       Current_Minor,
   --#       Scheduling_Plan;
   is
      Reason, Qualification, Intr_Info : SK.Word64;
      State                            : Subjects.State_Type;
      Current_Subject                  : Skp.Subjects.Subject_Id_Type;
   begin
      Current_Subject := Scheduling_Plan (Current_Major) (Current_Minor);
      State           := Subjects.Get_State (Id => Current_Subject);
      State.Regs      := Subject_Registers;
      Subjects.Set_State (Id    => Current_Subject,
                          State => State);

      VMX.VMCS_Read (Field => Constants.VMX_EXIT_REASON,
                     Value => Reason);

      if Reason /= Constants.VM_EXIT_TIMER_EXPIRY then
         pragma Debug (VMX.VMCS_Read
           (Field => Constants.VMX_EXIT_QUALIFICATION,
            Value => Qualification));

         pragma Debug (KC.Put_String (Item => "Subject "));
         pragma Debug (KC.Put_Byte   (Item => Byte (Current_Subject)));
         pragma Debug (KC.Put_String (Item => " EXIT ("));
         pragma Debug (KC.Put_Word16 (Item => SK.Word16 (Reason)));
         pragma Debug (KC.Put_String (Item => ":"));
         pragma Debug (KC.Put_Word32 (Item => SK.Word32 (Qualification)));

         pragma Debug (Reason = Constants.VM_EXIT_EXCEPTION_NMI,
           VMX.VMCS_Read (Field => Constants.VMX_EXIT_INTR_INFO,
                          Value => Intr_Info));
         pragma Debug (Reason = Constants.VM_EXIT_EXCEPTION_NMI,
                       KC.Put_String (Item => ":"));
         pragma Debug (Reason = Constants.VM_EXIT_EXCEPTION_NMI,
                       KC.Put_Word32 (Item => SK.Word32 (Intr_Info)));
         pragma Debug (KC.Put_Line (Item => ")"));
         pragma Debug (Dump.Print_State (Subject => Current_Subject));

         CPU.Panic;
      end if;

      Current_Major := New_Major;
      Current_Minor := Current_Minor + 1;
      Schedule;

      --# accept Warning, 400, Qualification, "Only used for debug output";
      --# accept Warning, 400, Intr_Info, "Only used for debug output";
   end Handle_Vmx_Exit;

begin
   Scheduling_Plan := Scheduling_Plan_Type'
     (0 => Major_Frame_Type'(0, 0, 0, 0),
      1 => Major_Frame_Type'(0, 1, 2, 0));
end SK.Scheduler;
