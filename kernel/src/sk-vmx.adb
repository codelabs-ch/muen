with Skp.Kernel;
with Skp.Subjects;

with SK.CPU;
with SK.CPU_Global;
with SK.Interrupts;
with SK.Descriptors;
with SK.KC;
with SK.GDT;
with SK.Constants;
with SK.Subjects;
with SK.Apic;

package body SK.VMX
--# own
--#    State is VMX_Exit_Address;
is

   --  Segment selectors

   SEL_KERN_CODE : constant := 16#08#;
   SEL_KERN_DATA : constant := 16#10#;
   SEL_TSS       : constant := 16#18#;

   Exec_Pin_Defaults   : constant SK.Word32 := Constants.VM_CTRL_EXIT_EXT_INT
     or Constants.VM_CTRL_PREEMPT_TIMER;
   Exec_Proc_Defaults  : constant SK.Word32 := Constants.VM_CTRL_IO_BITMAPS
     or Constants.VM_CTRL_SECONDARY_PROC
     or Constants.VM_CTRL_EXIT_INVLPG
     or Constants.VM_CTRL_EXIT_MWAIT
     or Constants.VM_CTRL_EXIT_RDPMC
     or Constants.VM_CTRL_EXIT_RDTSC
     or Constants.VM_CTRL_EXIT_CR3_LOAD
     or Constants.VM_CTRL_EXIT_CR3_STORE
     or Constants.VM_CTRL_EXIT_CR8_LOAD
     or Constants.VM_CTRL_EXIT_CR8_STORE
     or Constants.VM_CTRL_EXIT_MOV_DR
     or Constants.VM_CTRL_MSR_BITMAPS
     or Constants.VM_CTRL_EXIT_MONITOR;
   Exec_Proc2_Defaults : constant SK.Word32 := Constants.VM_CTRL_EXIT_WBINVD;

   Exit_Ctrl_Defaults  : constant SK.Word32 := Constants.VM_CTRL_IA32E_MODE
     or Constants.VM_CTRL_EXIT_ACK_INT
     or Constants.VM_CTRL_EXIT_SAVE_TIMER;

   --# accept Warning, 350, VMX_Exit_Address, "Imported from Linker";
   VMX_Exit_Address : SK.Word64;
   pragma Import (C, VMX_Exit_Address, "vmx_exit_handler_ptr");
   --# end accept;

   ---------------------------------------------------------------------------

   --  Return per-CPU memory offset.
   function Get_CPU_Offset return SK.Word64
   --# global
   --#    X86_64.State;
   is
   begin
      return SK.Word64 (Apic.Get_ID) * SK.Page_Size;
   end Get_CPU_Offset;

   -------------------------------------------------------------------------

   procedure VMCS_Write
     (Field : SK.Word16;
      Value : SK.Word64)
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

   procedure VMCS_Setup_Control_Fields
     (IO_Bitmap_Address  : SK.Word64;
      MSR_Bitmap_Address : SK.Word64;
      Ctls_Exec_Pin      : SK.Word32;
      Ctls_Exec_Proc     : SK.Word32;
      Ctls_Exec_Proc2    : SK.Word32)
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from
   --#       *,
   --#       Ctls_Exec_Pin,
   --#       Ctls_Exec_Proc,
   --#       Ctls_Exec_Proc2,
   --#       IO_Bitmap_Address,
   --#       MSR_Bitmap_Address;
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
      VMCS_Write (Field => Constants.PIN_BASED_EXEC_CONTROL,
                  Value => SK.Word64 (Value));

      --  Primary processor-based controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_PROCBASED_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exec_Proc;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.CPU_BASED_EXEC_CONTROL,
                  Value => SK.Word64 (Value));

      --  Secondary processor-based controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_PROCBASED_CTLS2,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exec_Proc2;
      Value := Value and Default1;
      Value := Value or  Default0;
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

      --  MSR bitmaps.

      VMCS_Write (Field => Constants.MSR_BITMAP,
                  Value => MSR_Bitmap_Address);

      --  VM-exit controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_EXIT_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Exit_Ctrl_Defaults;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.VM_EXIT_CONTROLS,
                  Value => SK.Word64 (Value));

      --  VM-entry controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_ENTRY_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Constants.VM_CTRL_IA32E_MODE;
      Value := Value and Default1;
      Value := Value or  Default0;
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
   end VMCS_Setup_Guest_Fields;

   -------------------------------------------------------------------------

   procedure Clear (VMCS_Address : SK.Word64)
   is
      Success : Boolean;
   begin
      CPU.VMCLEAR (Region  => VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug (KC.Put_String (Item => "Error clearing VMCS: "));
         pragma Debug (KC.Put_Word64 (Item => VMCS_Address));
         pragma Debug (KC.New_Line);
         CPU.Panic;
      end if;
   end Clear;

   -------------------------------------------------------------------------

   procedure Load (VMCS_Address : SK.Word64)
   is
      Success : Boolean;
   begin
      CPU.VMPTRLD (Region  => VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug (KC.Put_String (Item => "Error loading VMCS pointer: "));
         pragma Debug (KC.Put_Word64 (Item => VMCS_Address));
         pragma Debug (KC.New_Line);
         CPU.Panic;
      end if;
   end Load;

   -------------------------------------------------------------------------

   procedure Launch (Subject_Id : Skp.Subject_Id_Type)
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
      Spec  : Skp.Subjects.Subject_Spec_Type;
      State : SK.Subject_State_Type;
   begin
      Spec  := Skp.Subjects.Subject_Specs (Subject_Id);
      State := Subjects.Get_State (Id => Subject_Id);

      Clear (VMCS_Address => Spec.VMCS_Address);
      Load  (VMCS_Address => Spec.VMCS_Address);

      VMCS_Setup_Control_Fields
        (IO_Bitmap_Address  => Spec.IO_Bitmap_Address,
         MSR_Bitmap_Address => Spec.MSR_Bitmap_Address,
         Ctls_Exec_Pin      => Exec_Pin_Defaults,
         Ctls_Exec_Proc     => Exec_Proc_Defaults,
         Ctls_Exec_Proc2    => Exec_Proc2_Defaults);
      VMCS_Setup_Host_Fields;
      VMCS_Setup_Guest_Fields
        (Stack_Address => Spec.Stack_Address,
         PML4_Address  => Spec.PML4_Address,
         Entry_Point   => Spec.Entry_Point);

      State.Launched := True;
      Subjects.Set_State (Id    => Subject_Id,
                          State => State);

      CPU.Restore_Registers
        (Regs => Subjects.Get_State (Id => Subject_Id).Regs);
      CPU.VMLAUNCH;

      --  VM launch failed.

      CPU.Set_Stack (Address => Skp.Kernel.Stack_Address);

      pragma Debug (KC.Put_String (Item => "Error launching subject "));
      pragma Debug (KC.Put_Byte
                    (Item => SK.Byte
                     (CPU_Global.Get_Current_Minor_Frame.Subject_Id)));
      pragma Debug (KC.New_Line);
      VMX_Error;
   end Launch;

   -------------------------------------------------------------------------

   procedure Resume (Subject_Id : Skp.Subject_Id_Type)
   is
      Spec       : Skp.Subjects.Subject_Spec_Type;
      State      : SK.Subject_State_Type;
      Intr_State : SK.Word64;
   begin
      Spec  := Skp.Subjects.Subject_Specs (Subject_Id);
      State := Subjects.Get_State (Id => Subject_Id);

      Load (VMCS_Address => Spec.VMCS_Address);

      if State.Pending_Event > 0
        and then SK.Bit_Test
          (Value => State.RFLAGS,
           Pos   => Constants.RFLAGS_IF_FLAG)
      then

         --  Check guest interruptibility state (see Intel SDM Vol. 3C, chapter
         --  24.4.2).

         VMCS_Read (Field => Constants.GUEST_INTERRUPTIBILITY,
                    Value => Intr_State);

         if Intr_State = 0 then
            VMCS_Write
              (Field => Constants.VM_ENTRY_INTERRUPT_INFO,
               Value => Constants.VM_INTERRUPT_INFO_VALID +
                 SK.Word64 (State.Pending_Event));

            --  Clear pending event.

            Subjects.Set_Pending_Event (Id     => Subject_Id,
                                        Vector => 0);
         end if;
      end if;

      VMCS_Write (Field => Constants.GUEST_RIP,
                  Value => State.RIP);
      VMCS_Write (Field => Constants.GUEST_RSP,
                  Value => State.RSP);

      CPU.Restore_Registers (Regs => State.Regs);
      CPU.VMRESUME;

      --  VM resume failed.

      CPU.Set_Stack (Address => Skp.Kernel.Stack_Address);

      pragma Debug (KC.Put_String (Item => "Error resuming subject "));
      pragma Debug (KC.Put_Byte
                    (Item => SK.Byte
                     (CPU_Global.Get_Current_Minor_Frame.Subject_Id)));
      pragma Debug (KC.New_Line);

      VMX_Error;
   end Resume;

   -------------------------------------------------------------------------

   procedure Enable
   is
      Success : Boolean;
   begin
      CPU.Set_CR4 (Value => SK.Bit_Set
                   (Value => CPU.Get_CR4,
                    Pos   => Constants.CR4_VMXE_FLAG));

      CPU.VMXON (Region  => Skp.Vmxon_Address + Get_CPU_Offset,
                 Success => Success);
      if not Success then
         pragma Debug (KC.Put_Line (Item => "Error enabling VMX"));
         CPU.Panic;
      end if;
   end Enable;

end SK.VMX;
