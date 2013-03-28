with Skp.Kernel;

with SK.CPU;
with SK.Interrupts;
with SK.Descriptors;
with SK.KC;
with SK.GDT;
with SK.Constants;
with SK.Subjects;

package body SK.VMX
--# own
--#    State is VMX_Exit_Address;
is

   --  Segment selectors

   SEL_KERN_CODE : constant := 16#08#;
   SEL_KERN_DATA : constant := 16#10#;
   SEL_TSS       : constant := 16#18#;

   --  Subject preemption time in ticks. Used to set the VMX preemption timer.

   Subject_Time_Slice : constant := 500;

   subtype Alignment_Type is SK.Word16 range 1 .. SK.Word16'Last;

   --# accept Warning, 350, VMX_Exit_Address, "Imported from Linker";
   VMX_Exit_Address : SK.Word64;
   pragma Import (C, VMX_Exit_Address, "vmx_exit_handler_ptr");
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

   procedure VMX_Error
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;
   is
      Error   : SK.Word64;
      Success : Boolean;
   begin
      pragma Debug (CPU.VMREAD (Field   => Constants.VMX_INST_ERROR,
                                Value   => Error,
                                Success => Success));
      pragma Debug (Success, KC.Put_String (Item => "VM instruction error: "));
      pragma Debug (Success, KC.Put_Byte (Item => Byte (Error)));
      pragma Debug (Success, KC.New_Line);
      pragma Debug (not Success, KC.Put_Line
        (Item => "Unable to read VMX instruction error"));

      CPU.Panic;

      --# accept Warning, 400, Error,   "Only used for debug output";
      --# accept Warning, 400, Success, "Only used for debug output";
   end VMX_Error;

   -------------------------------------------------------------------------

   procedure Resume (Subject_Id : Skp.Subjects.Subject_Id_Type)
   is
      Success : Boolean;
      Spec    : Skp.Subjects.Subject_Spec_Type;
      State   : Subjects.State_Type;
   begin
      Spec  := Skp.Subjects.Subject_Specs (Subject_Id);
      State := Subjects.Get_State (Id => Subject_Id);

      CPU.VMPTRLD (Region  => Spec.VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug (KC.Put_Line (Item => "Error loading VMCS pointer"));
         CPU.Panic;
      end if;

      VMCS_Write (Field => Constants.GUEST_VMX_PREEMPT_TIMER,
                  Value => Subject_Time_Slice);

      CPU.Restore_Registers (Regs => State.Regs);
      CPU.VMRESUME;

      --  VM resume failed.

      CPU.Set_Stack (Address => Skp.Kernel.Stack_Address);

      pragma Debug (KC.Put_String (Item => "Error resuming subject "));
      pragma Debug (KC.Put_Byte (Item => Byte (Subject_Id)));
      pragma Debug (KC.New_Line);

      VMX_Error;
   end Resume;

   -------------------------------------------------------------------------

   procedure VMCS_Setup_Control_Fields
     (IO_Bitmap_Address : SK.Word64;
      Ctls_Exec_Pin     : SK.Word32;
      Ctls_Exec_Proc    : SK.Word32;
      Ctls_Exec_Proc2   : SK.Word32)
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from
   --#       *,
   --#       Ctls_Exec_Pin,
   --#       Ctls_Exec_Proc,
   --#       Ctls_Exec_Proc2,
   --#       IO_Bitmap_Address;
   is
      Default0, Default1, Value : SK.Word32;
   begin

      --  Pin-based controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_PINBASED_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exec_Pin;
      Value := Value and Default1;
      Value := Value or  Default0;

      pragma Debug (KC.Put_String (Item => "PIN_BASED_EXEC_CONTROL  "));
      pragma Debug (KC.Put_Word32 (Item => Value));
      pragma Debug (KC.New_Line);
      VMCS_Write (Field => Constants.PIN_BASED_EXEC_CONTROL,
                  Value => SK.Word64 (Value));

      --  Primary processor-based controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_PROCBASED_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exec_Proc;
      Value := Value and Default1;
      Value := Value or  Default0;

      pragma Debug (KC.Put_String (Item => "CPU_BASED_EXEC_CONTROL  "));
      pragma Debug (KC.Put_Word32 (Item => Value));
      pragma Debug (KC.New_Line);
      VMCS_Write (Field => Constants.CPU_BASED_EXEC_CONTROL,
                  Value => SK.Word64 (Value));

      --  Secondary processor-based controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_PROCBASED_CTLS2,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exec_Proc2;
      Value := Value and Default1;
      Value := Value or  Default0;

      pragma Debug (KC.Put_String (Item => "CPU_BASED_EXEC_CONTROL2 "));
      pragma Debug (KC.Put_Word32 (Item => Value));
      pragma Debug (KC.New_Line);
      VMCS_Write (Field => Constants.CPU_BASED_EXEC_CONTROL2,
                  Value => SK.Word64 (Value));

      --  Exception bitmap.

      VMCS_Write (Field => Constants.EXCEPTION_BITMAP,
                  Value => 16#ffff_ffff#);

      --  Disallow write access to CR0/CR4.

      VMCS_Write (Field => Constants.CR0_MASK,
                  Value => 16#ffff_ffff#);
      VMCS_Write (Field => Constants.CR4_MASK,
                  Value => 16#ffff_ffff#);

      --  Shadow read access to CR0/CR4.

      VMCS_Write (Field => Constants.CR0_READ_SHADOW,
                  Value => CPU.Get_CR0);
      VMCS_Write (Field => Constants.CR4_READ_SHADOW,
                  Value => CPU.Get_CR4);

      --  I/O bitmaps.

      VMCS_Write (Field => Constants.IO_BITMAP_A,
                  Value => IO_Bitmap_Address);
      VMCS_Write (Field => Constants.IO_BITMAP_B,
                  Value => IO_Bitmap_Address + SK.Page_Size);

      --  VM-exit controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_EXIT_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Constants.VM_CTRL_IA32E_MODE;
      Value := Value and Default1;
      Value := Value or  Default0;

      pragma Debug (KC.Put_String (Item => "VM_EXIT_CONTROLS        "));
      pragma Debug (KC.Put_Word32 (Item => Value));
      pragma Debug (KC.New_Line);
      VMCS_Write (Field => Constants.VM_EXIT_CONTROLS,
                  Value => SK.Word64 (Value));

      --  VM-entry controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_ENTRY_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Constants.VM_CTRL_IA32E_MODE;
      Value := Value and Default1;
      Value := Value or  Default0;

      pragma Debug (KC.Put_String (Item => "VM_ENTRY_CONTROLS       "));
      pragma Debug (KC.Put_Word32 (Item => Value));
      pragma Debug (KC.New_Line);
      VMCS_Write (Field => Constants.VM_ENTRY_CONTROLS,
                  Value => SK.Word64 (Value));
   end VMCS_Setup_Control_Fields;

   -------------------------------------------------------------------------

   procedure VMCS_Setup_Host_Fields
   --# global
   --#    in     Interrupts.IDT_Pointer;
   --#    in     GDT.GDT_Pointer;
   --#    in     VMX_Exit_Address;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from
   --#       *,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       VMX_Exit_Address;
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
                  Value => Skp.Kernel.Stack_Address);
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

   procedure Launch (Subject_Id : Skp.Subjects.Subject_Id_Type)
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     VMX_Exit_Address;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    Subjects.Descriptors from *, Subject_Id &
   --#    X86_64.State from
   --#       *,
   --#       GDT.GDT_Pointer,
   --#       Interrupts.IDT_Pointer,
   --#       VMX_Exit_Address,
   --#       Subjects.Descriptors,
   --#       Subject_Id;
   is
      Success : Boolean;
      Spec    : Skp.Subjects.Subject_Spec_Type;
      State   : Subjects.State_Type;
   begin
      pragma Debug (KC.Put_String (Item => "Launching subject "));
      pragma Debug (KC.Put_Byte (Item => Byte (Subject_Id)));
      pragma Debug (KC.New_Line);

      Spec  := Skp.Subjects.Subject_Specs (Subject_Id);
      State := Subjects.Get_State (Id => Subject_Id);

      Success := Is_Aligned
        (Address   => Spec.VMCS_Address,
         Alignment => SK.Page_Size);
      if not Success then
         pragma Debug (KC.Put_Line (Item => "VMCS region alignment invalid"));
         CPU.Panic;
      end if;

      CPU.VMCLEAR (Region  => Spec.VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug (KC.Put_Line (Item => "Error clearing VMCS"));
         CPU.Panic;
      end if;

      CPU.VMPTRLD (Region  => Spec.VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug (KC.Put_String (Item => "Error loading VMCS pointer: "));
         pragma Debug (KC.Put_Word64 (Item => Spec.VMCS_Address));
         pragma Debug (KC.New_Line);
         CPU.Panic;
      end if;

      VMCS_Setup_Control_Fields
        (IO_Bitmap_Address => Spec.IO_Bitmap_Address,
         Ctls_Exec_Pin     => State.Ctls_Exec_Pin,
         Ctls_Exec_Proc    => State.Ctls_Exec_Proc,
         Ctls_Exec_Proc2   => State.Ctls_Exec_Proc2);
      VMCS_Setup_Host_Fields;
      VMCS_Setup_Guest_Fields
        (Stack_Address => State.Stack_Address,
         PML4_Address  => Spec.PML4_Address,
         Entry_Point   => State.Entry_Point);

      State.Launched := True;
      Subjects.Set_State (Id    => Subject_Id,
                          State => State);

      CPU.Restore_Registers
        (Regs => Subjects.Get_State (Id => Subject_Id).Regs);
      CPU.VMLAUNCH;

      --  VM launch failed.

      CPU.Set_Stack (Address => Skp.Kernel.Stack_Address);

      pragma Debug (KC.Put_String (Item => "Error launching subject "));
      pragma Debug (KC.Put_Byte (Item => Byte (Subject_Id)));
      pragma Debug (KC.New_Line);

      VMX_Error;
   end Launch;

   -------------------------------------------------------------------------

   procedure Enable
   is
      Success : Boolean;
   begin
      CPU.Set_CR4 (Value => SK.Bit_Set
                   (Value => CPU.Get_CR4,
                    Pos   => Constants.CR4_VMXE_FLAG));

      CPU.VMXON (Region  => Skp.Vmxon_Address,
                 Success => Success);
      if not Success then
         pragma Debug (KC.Put_Line (Item => "Error enabling VMX"));
         CPU.Panic;
      end if;
   end Enable;

end SK.VMX;
