with SK.CPU;
with SK.VMX;
with SK.Constants;
with SK.KC;
with SK.Dump;

package body SK.Scheduler
--# own
--#    Current_Subject is Current_Slot, Current_Plan;
is

   --  Scheduling slot.
   type Slot_Type is mod 2 ** 2;

   --  Scheduling plan.
   type Plan_Type is array (Slot_Type) of Subjects.Id_Type;

   --  Current scheduling slot and plan.
   Current_Slot : Slot_Type := 0;
   Current_Plan : Plan_Type;

   -------------------------------------------------------------------------

   procedure Schedule
   --# global
   --#    in     VMX.VMX_Exit_Address;
   --#    in     VMX.Kernel_Stack_Address;
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     Current_Slot;
   --#    in     Current_Plan;
   --#    in out X86_64.State;
   --#    in out Subjects.Descriptors;
   --# derives
   --#    Subjects.Descriptors from *, Current_Slot, Current_Plan &
   --#    X86_64.State from
   --#       *,
   --#       VMX.VMX_Exit_Address,
   --#       VMX.Kernel_Stack_Address,
   --#       GDT.GDT_Pointer,
   --#       Interrupts.IDT_Pointer,
   --#       Subjects.Descriptors,
   --#       Current_Slot,
   --#       Current_Plan;
   is
      Current_Subject : Subjects.Id_Type;
   begin
      Current_Subject := Current_Plan (Current_Slot);

      if Subjects.Get_State (Id => Current_Subject).Launched then
         VMX.Resume (Subject_Id => Current_Subject);
      else
         VMX.Launch (Subject_Id => Current_Subject);
      end if;
   end Schedule;

   -------------------------------------------------------------------------

   procedure Handle_Vmx_Exit
     (RDI : SK.Word64; RSI : SK.Word64; RDX : SK.Word64; RCX : SK.Word64;
      R08 : SK.Word64; R09 : SK.Word64; RAX : SK.Word64; RBX : SK.Word64;
      RBP : SK.Word64; R10 : SK.Word64; R11 : SK.Word64; R12 : SK.Word64;
      R13 : SK.Word64; R14 : SK.Word64; R15 : SK.Word64)
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     VMX.VMX_Exit_Address;
   --#    in     VMX.Kernel_Stack_Address;
   --#    in     Current_Plan;
   --#    in out Current_Slot;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    Current_Slot from * &
   --#    X86_64.State from
   --#       *,
   --#       RAX,
   --#       RBX,
   --#       RCX,
   --#       RDX,
   --#       RDI,
   --#       RSI,
   --#       RBP,
   --#       R08,
   --#       R09,
   --#       R10,
   --#       R11,
   --#       R12,
   --#       R13,
   --#       R14,
   --#       R15,
   --#       VMX.VMX_Exit_Address,
   --#       VMX.Kernel_Stack_Address,
   --#       Current_Slot,
   --#       Current_Plan,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       Subjects.Descriptors &
   --#    Subjects.Descriptors from
   --#       *,
   --#       RAX,
   --#       RBX,
   --#       RCX,
   --#       RDX,
   --#       RDI,
   --#       RSI,
   --#       RBP,
   --#       R08,
   --#       R09,
   --#       R10,
   --#       R11,
   --#       R12,
   --#       R13,
   --#       R14,
   --#       R15,
   --#       Current_Slot,
   --#       Current_Plan;
   is
      Reason, Qualification, Intr_Info : SK.Word64;
      Registers                        : CPU.Registers_Type;
      State                            : Subjects.State_Type;
      Current_Subject                  : Subjects.Id_Type;
   begin
      Current_Subject := Current_Plan (Current_Slot);

      Registers := CPU.Registers_Type'
        (RAX => RAX,
         RBX => RBX,
         RCX => RCX,
         RDX => RDX,
         RDI => RDI,
         RSI => RSI,
         RBP => RBP,
         R08 => R08,
         R09 => R09,
         R10 => R10,
         R11 => R11,
         R12 => R12,
         R13 => R13,
         R14 => R14,
         R15 => R15);

      State := Subjects.Get_State (Id => Current_Subject);
      State.Regs := Registers;
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

      Current_Slot := Current_Slot + 1;
      Schedule;

      --# accept Warning, 400, Qualification, "Only used for debug output";
      --# accept Warning, 400, Intr_Info, "Only used for debug output";
   end Handle_Vmx_Exit;

begin
   Current_Plan := Plan_Type'(0, 1, 0, 1);
end SK.Scheduler;
