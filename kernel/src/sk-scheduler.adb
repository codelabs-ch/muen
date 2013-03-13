with SK.CPU;
with SK.VMX;
with SK.Constants;
with SK.KC;
with SK.Dump;

package body SK.Scheduler
is

   --  Current active subject, initialized to root subject.
   Current_Subject : Subjects.Index_Type := 0;

   -------------------------------------------------------------------------

   procedure Handle_Vmx_Exit
     (RDI : SK.Word64; RSI : SK.Word64; RDX : SK.Word64; RCX : SK.Word64;
      R08 : SK.Word64; R09 : SK.Word64; RAX : SK.Word64; RBX : SK.Word64;
      RBP : SK.Word64; R10 : SK.Word64; R11 : SK.Word64; R12 : SK.Word64;
      R13 : SK.Word64; R14 : SK.Word64; R15 : SK.Word64)
   is
      Reason, Qualification, Intr_Info : SK.Word64;
      Registers                        : CPU.Registers_Type;
      State                            : Subjects.State_Type;
   begin
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

      State := Subjects.Get_State (Idx => Current_Subject);
      State.Regs := Registers;
      Subjects.Set_State (Idx   => Current_Subject,
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

      Current_Subject := Current_Subject + 1;
      if Subjects.Get_State (Idx => Current_Subject).Launched then
         VMX.Resume (Subject_Id => Current_Subject);
      else
         VMX.Launch (Subject_Id => Current_Subject);
      end if;
      --# accept Warning, 400, Qualification, "Only used for debug output";
      --# accept Warning, 400, Intr_Info, "Only used for debug output";
   end Handle_Vmx_Exit;

end SK.Scheduler;
