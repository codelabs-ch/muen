with System;

with SK.CPU;
with SK.Interrupts;
with SK.Descriptors;
with SK.KC;
with SK.GDT;
with SK.Constants;
with SK.Debug;

package body SK.VMX
is

   --  Segment selectors

   SEL_KERN_CODE : constant := 16#08#;
   SEL_KERN_DATA : constant := 16#10#;
   SEL_TSS       : constant := 16#18#;

   --  Subject preemption time in ticks. Used to set the VMX preemption timer.

   Subject_Time_Slice : constant := 500;

   subtype Alignment_Type is SK.Word16 range 1 .. SK.Word16'Last;

   --  Current active subject, initialized to root subject.

   Current_Subject : Subjects.Index_Type := 0;

   --# accept Warning, 350, VMXON_Address, "Imported from Linker";
   VMXON_Address : SK.Word64;
   pragma Import (C, VMXON_Address, "vmxon_pointer");
   --# end accept;

   --# accept Warning, 350, VMX_Exit_Address, "Imported from Linker";
   VMX_Exit_Address : SK.Word64;
   pragma Import (C, VMX_Exit_Address, "vmx_exit_handler_pointer");
   --# end accept;

   --# accept Warning, 350, Kernel_Stack_Address, "Imported from Linker";
   Kernel_Stack_Address : SK.Word64;
   pragma Import (C, Kernel_Stack_Address, "kernel_stack_pointer");
   --# end accept;

   ---------------------------------------------------------------------------

   --  Check alignment of given address.
   function Is_Aligned
     (Address   : SK.Word64;
      Alignment : Alignment_Type)
      return Boolean
   is
   begin
      return (Address mod SK.Word64 (Alignment)) = 0;
   end Is_Aligned;

   -------------------------------------------------------------------------

   --  Write given value to the specified field of the current, active VMCS. If
   --  the operation fails, CPU.Panic is called.
   procedure VMCS_Write
     (Field : SK.Word16;
      Value : SK.Word64)
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Field, Value;
   is
      Success : Boolean;
   begin
      CPU.VMWRITE (Field   => SK.Word64 (Field),
                   Value   => Value,
                   Success => Success);
      if not Success then
         pragma Debug (KC.Put_String (Item => "Error setting VMCS field "));
         pragma Debug (KC.Put_Word16 (Item => Field));
         pragma Debug (KC.Put_String (Item => " to value "));
         pragma Debug (KC.Put_Word64 (Item => Value));
         pragma Debug (KC.New_Line);
         CPU.Panic;
      end if;
   end VMCS_Write;

   -------------------------------------------------------------------------

   procedure VMCS_Read
     (Field :     SK.Word16;
      Value : out SK.Word64)
   is
      Success : Boolean;
   begin
      CPU.VMREAD (Field   => SK.Word64 (Field),
                  Value   => Value,
                  Success => Success);
      if not Success then
         pragma Debug (KC.Put_String (Item => "Error reading VMCS field "));
         pragma Debug (KC.Put_Word16 (Item => Field));
         pragma Debug (KC.New_Line);
         CPU.Panic;
      end if;
   end VMCS_Read;

   -------------------------------------------------------------------------

   procedure Resume
   --# global
   --#    in     Current_Subject;
   --#    in     Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Current_Subject, Subjects.Descriptors;
   is
      Error   : SK.Word64;
      Success : Boolean;
      State   : Subjects.State_Type;
   begin
      State := Subjects.Get_State (Idx => Current_Subject);

      CPU.VMPTRLD (Region  => State.VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug (KC.Put_Line (Item => "Error loading VMCS pointer"));
         CPU.Panic;
      end if;

      VMCS_Write (Field => Constants.GUEST_VMX_PREEMPT_TIMER,
                  Value => Subject_Time_Slice);

      CPU.Restore_Registers (Regs => State.Regs);
      CPU.VMRESUME (Success => Success);
      if not Success then
         pragma Debug (CPU.VMREAD (Field   => Constants.VMX_INST_ERROR,
                                   Value   => Error,
                                   Success => Success));
         pragma Debug (KC.Put_String (Item => "Error resuming VM ("));
         pragma Debug (KC.Put_Byte (Item => Byte (Error)));
         pragma Debug (KC.Put_Line (Item => ")"));
         CPU.Panic;
      end if;
      --# accept Warning, 400, Error, "Only used for debug output";
   end Resume;

   -------------------------------------------------------------------------

   procedure VMCS_Setup_Control_Fields
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;
   is
      Default0, Default1 : SK.Word32;
      Value              : SK.Word64;
   begin
      CPU.Get_MSR (Register => Constants.IA32_VMX_PINBASED_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Constants.VM_CONTROL_PREEMPT_TIMER;
      Value := Value and SK.Word64 (Default1);
      Value := Value or  SK.Word64 (Default0);
      VMCS_Write (Field => Constants.PIN_BASED_EXEC_CONTROL,
                  Value => Value);

      CPU.Get_MSR (Register => Constants.IA32_VMX_PROCBASED_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Constants.VM_CONTROL_EXIT_HLT;
      Value := Value and SK.Word64 (Default1);
      Value := Value or  SK.Word64 (Default0);
      VMCS_Write (Field => Constants.CPU_BASED_EXEC_CONTROL,
                  Value => Value);

      VMCS_Write (Field => Constants.EXCEPTION_BITMAP,
                  Value => 16#ffffffff#);

      CPU.Get_MSR (Register => Constants.IA32_VMX_EXIT_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Constants.VM_CONTROL_IA32E_MODE;
      Value := Value and SK.Word64 (Default1);
      Value := Value or  SK.Word64 (Default0);
      VMCS_Write (Field => Constants.VM_EXIT_CONTROLS,
                  Value => Value);

      CPU.Get_MSR (Register => Constants.IA32_VMX_ENTRY_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Constants.VM_CONTROL_IA32E_MODE;
      Value := Value and SK.Word64 (Default1);
      Value := Value or  SK.Word64 (Default0);
      VMCS_Write (Field => Constants.VM_ENTRY_CONTROLS,
                  Value => Value);
   end VMCS_Setup_Control_Fields;

   -------------------------------------------------------------------------

   procedure VMCS_Setup_Host_Fields
   --# global
   --#    in     Interrupts.IDT_Pointer;
   --#    in     GDT.GDT_Pointer;
   --#    in     VMX_Exit_Address;
   --#    in     Kernel_Stack_Address;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from
   --#       *,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       VMX_Exit_Address,
   --#       Kernel_Stack_Address;
   is
      PD : Descriptors.Pseudo_Descriptor_Type;
   begin
      VMCS_Write (Field => Constants.HOST_SEL_CS,
                  Value => SEL_KERN_CODE);
      VMCS_Write (Field => Constants.HOST_SEL_DS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.HOST_SEL_ES,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.HOST_SEL_SS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.HOST_SEL_TR,
                  Value => SEL_TSS);

      VMCS_Write (Field => Constants.HOST_CR0,
                  Value => CPU.Get_CR0);
      VMCS_Write (Field => Constants.HOST_CR3,
                  Value => CPU.Get_CR3);
      VMCS_Write (Field => Constants.HOST_CR4,
                  Value => CPU.Get_CR4);

      PD := Interrupts.Get_IDT_Pointer;
      VMCS_Write (Field => Constants.HOST_BASE_IDTR,
                  Value => PD.Base);
      PD := GDT.Get_GDT_Pointer;
      VMCS_Write (Field => Constants.HOST_BASE_GDTR,
                  Value => PD.Base);

      VMCS_Write (Field => Constants.HOST_RSP,
                  Value => Kernel_Stack_Address);
      VMCS_Write (Field => Constants.HOST_RIP,
                  Value => VMX_Exit_Address);
   end VMCS_Setup_Host_Fields;

   -------------------------------------------------------------------------

   procedure VMCS_Setup_Guest_Fields
     (Stack_Address : SK.Word64;
      PML4_Address  : SK.Word64;
      Entry_Point   : SK.Word64)
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from
   --#       *,
   --#       Stack_Address,
   --#       PML4_Address,
   --#       Entry_Point;
   is
   begin
      VMCS_Write (Field => Constants.VMCS_LINK_POINTER,
                  Value => SK.Word64'Last);

      VMCS_Write (Field => Constants.GUEST_SEL_CS,
                  Value => SEL_KERN_CODE);
      VMCS_Write (Field => Constants.GUEST_SEL_DS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.GUEST_SEL_ES,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.GUEST_SEL_SS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.GUEST_SEL_TR,
                  Value => SEL_TSS);

      VMCS_Write (Field => Constants.GUEST_LIMIT_CS,
                  Value => SK.Word64 (SK.Word32'Last));
      VMCS_Write (Field => Constants.GUEST_LIMIT_DS,
                  Value => SK.Word64 (SK.Word32'Last));
      VMCS_Write (Field => Constants.GUEST_LIMIT_ES,
                  Value => SK.Word64 (SK.Word32'Last));
      VMCS_Write (Field => Constants.GUEST_LIMIT_SS,
                  Value => SK.Word64 (SK.Word32'Last));
      VMCS_Write (Field => Constants.GUEST_LIMIT_TR,
                  Value => SK.Word64 (SK.Byte'Last));

      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_CS,
                  Value => 16#a09b#);
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_DS,
                  Value => 16#c093#);
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_ES,
                  Value => 16#c093#);
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_SS,
                  Value => 16#c093#);
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_TR,
                  Value => 16#8b#);

      --  Disable fs, gs and ldt segments for now.

      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_FS,
                  Value => 16#10000#);
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_GS,
                  Value => 16#10000#);
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_LDTR,
                  Value => 16#10000#);

      VMCS_Write (Field => Constants.GUEST_CR0,
                  Value => CPU.Get_CR0);
      VMCS_Write (Field => Constants.GUEST_CR3,
                  Value => PML4_Address);
      VMCS_Write (Field => Constants.GUEST_CR4,
                  Value => CPU.Get_CR4);

      VMCS_Write (Field => Constants.GUEST_RFLAGS,
                  Value => CPU.Get_RFLAGS);
      VMCS_Write (Field => Constants.GUEST_RSP,
                  Value => Stack_Address);
      VMCS_Write (Field => Constants.GUEST_RIP,
                  Value => Entry_Point);

      VMCS_Write (Field => Constants.GUEST_VMX_PREEMPT_TIMER,
                  Value => Subject_Time_Slice);
   end VMCS_Setup_Guest_Fields;

   -------------------------------------------------------------------------

   procedure Launch
   is
      Success : Boolean;
      Error   : SK.Word64;
      State   : Subjects.State_Type;
   begin
      pragma Debug (KC.Put_String (Item => "Launching subject "));
      pragma Debug (KC.Put_Byte (Item => Byte (Current_Subject)));
      pragma Debug (KC.New_Line);

      State := Subjects.Get_State (Idx => Current_Subject);

      Success := Is_Aligned
        (Address   => State.VMCS_Address,
         Alignment => 4096);
      if not Success then
         pragma Debug (KC.Put_Line (Item => "VMCS region alignment invalid"));
         CPU.Panic;
      end if;

      CPU.VMCLEAR (Region  => State.VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug (KC.Put_Line (Item => "Error clearing VMCS"));
         CPU.Panic;
      end if;

      CPU.VMPTRLD (Region  => State.VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug (KC.Put_Line (Item => "Error loading VMCS pointer"));
         CPU.Panic;
      end if;

      VMCS_Setup_Control_Fields;
      VMCS_Setup_Host_Fields;
      VMCS_Setup_Guest_Fields
        (Stack_Address => State.Stack_Address,
         PML4_Address  => State.PML4_Address,
         Entry_Point   => State.Entry_Point);

      State.Launched := True;
      Subjects.Set_State (Idx   => Current_Subject,
                          State => State);

      CPU.Restore_Registers
        (Regs => Subjects.Get_State (Idx => Current_Subject).Regs);
      CPU.VMLAUNCH (Success => Success);
      if not Success then
         pragma Debug (CPU.VMREAD (Field   => Constants.VMX_INST_ERROR,
                                   Value   => Error,
                                   Success => Success));
         pragma Debug (KC.Put_String (Item => "Error launching VM ("));
         pragma Debug (KC.Put_Byte (Item => Byte (Error)));
         pragma Debug (KC.Put_Line (Item => ")"));
         CPU.Panic;
      end if;
      --# accept Warning, 400, Error, "Only used for debug output";
   end Launch;

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
        (RAX  => RAX,
         RBX  => RBX,
         RCX  => RCX,
         RDX  => RDX,
         RDI  => RDI,
         RSI  => RSI,
         RBP  => RBP,
         R08  => R08,
         R09  => R09,
         R10  => R10,
         R11  => R11,
         R12  => R12,
         R13  => R13,
         R14  => R14,
         R15  => R15);

      State := Subjects.Get_State (Idx => Current_Subject);
      State.Regs := Registers;
      Subjects.Set_State (Idx   => Current_Subject,
                          State => State);

      VMCS_Read (Field => Constants.VMX_EXIT_REASON,
                 Value => Reason);

      if Reason /= Constants.VMEXIT_TIMER_EXPIRY then
         pragma Debug (VMCS_Read (Field => Constants.VMX_EXIT_QUALIFICATION,
                                  Value => Qualification));

         pragma Debug (KC.Put_String (Item => "Subject "));
         pragma Debug (KC.Put_Byte   (Item => Byte (Current_Subject)));
         pragma Debug (KC.Put_String (Item => " EXIT ("));
         pragma Debug (KC.Put_Word16 (Item => SK.Word16 (Reason)));
         pragma Debug (KC.Put_String (Item => ":"));
         pragma Debug (KC.Put_Word32 (Item => SK.Word32 (Qualification)));

         pragma Debug (Reason = Constants.VMEXIT_EXCEPTION_NMI,
           VMCS_Read (Field => Constants.VMX_EXIT_INTR_INFO,
                      Value => Intr_Info));
         pragma Debug (Reason = Constants.VMEXIT_EXCEPTION_NMI,
                       KC.Put_String (Item => ":"));
         pragma Debug (Reason = Constants.VMEXIT_EXCEPTION_NMI,
                       KC.Put_Word32 (Item => SK.Word32 (Intr_Info)));
         pragma Debug (KC.Put_Line (Item => ")"));
         pragma Debug (Debug.Print_State (Subject => Current_Subject));

         CPU.Panic;
      end if;

      Current_Subject := Current_Subject + 1;
      if Subjects.Get_State (Idx => Current_Subject).Launched then
         Resume;
      else
         Launch;
      end if;
      --# accept Warning, 400, Qualification, "Only used for debug output";
      --# accept Warning, 400, Intr_Info, "Only used for debug output";
   end Handle_Vmx_Exit;

   -------------------------------------------------------------------------

   procedure Enable
   is
      Success : Boolean;
   begin
      Success := Is_Aligned
        (Address   => VMXON_Address,
         Alignment => 4096);
      if not Success then
         pragma Debug (KC.Put_Line ("VMXON region alignment invalid"));
         CPU.Panic;
      end if;

      CPU.Set_CR4 (Value => SK.Bit_Set
                   (Value => CPU.Get_CR4,
                    Pos   => Constants.CR4_VMXE_FLAG));

      CPU.VMXON (Region  => VMXON_Address,
                 Success => Success);
      if not Success then
         pragma Debug (KC.Put_Line (Item => "Error enabling VMX"));
         CPU.Panic;
      end if;
   end Enable;

begin

   --# hide SK.VMX;

   declare
      Unused_High, VMXON_Region : SK.Word32;
      for VMXON_Region'Address use System'To_Address (VMXON_Address);
   begin
      CPU.Get_MSR
        (Register => Constants.IA32_VMX_BASIC,
         Low      => VMXON_Region,
         High     => Unused_High);
   end;
end SK.VMX;
