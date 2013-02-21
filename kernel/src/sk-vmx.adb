with System.Storage_Elements;

with SK.CPU;
with SK.Interrupts;
with SK.Descriptors;
with SK.KC;
with SK.GDT;
with SK.Constants;
with SK.Subject;

package body SK.VMX
is

   --  Segment selectors

   SEL_KERN_CODE : constant := 16#08#;
   SEL_KERN_DATA : constant := 16#10#;
   SEL_TSS       : constant := 16#18#;

   subtype Alignment_Type is SK.Word16 range 1 .. SK.Word16'Last;

   --# accept Warning, 350, VMXON_Address, "Imported from Linker";
   VMXON_Address : SK.Word64;
   pragma Import (C, VMXON_Address, "vmxon_pointer");
   --# end accept;

   --# accept Warning, 350, VMCS_Address, "Imported from Linker";
   VMCS_Address : SK.Word64;
   pragma Import (C, VMCS_Address, "vmcs_pointer");
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

   --  Read value from specified field of the current, active VMCS. If the
   --  operation fails, CPU.Panic is called.
   procedure VMCS_Read
     (Field :     SK.Word16;
      Value : out SK.Word64)
   --# global
   --#    X86_64.State;
   --# derives
   --#    Value, X86_64.State from X86_64.State, Field;
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

   --  Handle VM exit.
   procedure Handle_Vmx_Exit
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;
   is
      Reason, Qualification : SK.Word64;
   begin
      pragma Debug (VMCS_Read (Field => Constants.VMX_EXIT_REASON,
                               Value => Reason));
      pragma Debug (VMCS_Read (Field => Constants.VMX_EXIT_QUALIFICATION,
                               Value => Qualification));
      pragma Debug (KC.Put_String (Item => "VM EXIT ("));
      pragma Debug (KC.Put_Word16 (Item => SK.Word16 (Reason)));
      pragma Debug (KC.Put_String (Item => ":"));
      pragma Debug (KC.Put_Word32 (Item => SK.Word32 (Qualification)));
      pragma Debug (KC.Put_Line (Item => ")"));
      CPU.Panic;
      --# accept Warning, 400, Reason, "Only used for debug output";
      --# accept Warning, 400, Qualification, "Only used for debug output";
   end Handle_Vmx_Exit;

   -------------------------------------------------------------------------

   --  Return address of VM exit handler.
   function Get_Vmx_Exit_Address return SK.Word64
   is
      --# hide Get_Vmx_Exit_Address;
   begin
      return SK.Word64
        (System.Storage_Elements.To_Integer
           (Value => Handle_Vmx_Exit'Address));
   end Get_Vmx_Exit_Address;

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
      Value := 0;
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
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Interrupts.IDT_Pointer, GDT.GDT_Pointer;
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
                  Value => CPU.Get_RSP);
      VMCS_Write (Field => Constants.HOST_RIP,
                  Value => Get_Vmx_Exit_Address);
   end VMCS_Setup_Host_Fields;

   -------------------------------------------------------------------------

   procedure VMCS_Setup_Guest_Fields
   --# global
   --#    in     Interrupts.IDT_Pointer;
   --#    in     GDT.GDT_Pointer;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from
   --#       *,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer;
   is
      PD : Descriptors.Pseudo_Descriptor_Type;
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
                  Value => 16#c09b#);
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
                  Value => CPU.Get_CR3);
      VMCS_Write (Field => Constants.GUEST_CR4,
                  Value => CPU.Get_CR4);

      PD := Interrupts.Get_IDT_Pointer;
      VMCS_Write (Field => Constants.GUEST_BASE_IDTR,
                  Value => PD.Base);
      VMCS_Write (Field => Constants.GUEST_LIMIT_IDTR,
                  Value => SK.Word64 (PD.Limit));
      PD := GDT.Get_GDT_Pointer;
      VMCS_Write (Field => Constants.GUEST_BASE_GDTR,
                  Value => PD.Base);
      VMCS_Write (Field => Constants.GUEST_LIMIT_GDTR,
                  Value => SK.Word64 (PD.Limit));

      VMCS_Write (Field => Constants.GUEST_RFLAGS,
                  Value => CPU.Get_RFLAGS);
      VMCS_Write (Field => Constants.GUEST_RSP,
                  Value => CPU.Get_RSP);
      VMCS_Write (Field => Constants.GUEST_RIP,
                  Value => Subject.Get_Main_Address);
   end VMCS_Setup_Guest_Fields;

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

   -------------------------------------------------------------------------

   procedure Launch
   is
      Success : Boolean;
      Error   : SK.Word64;
   begin
      Success := Is_Aligned
        (Address   => VMCS_Address,
         Alignment => 4096);
      if not Success then
         pragma Debug (KC.Put_Line ("VMCS region alignment invalid"));
         CPU.Panic;
      end if;

      CPU.VMCLEAR (Region  => VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug (KC.Put_Line (Item => "Error clearing VMCS"));
         CPU.Panic;
      end if;

      CPU.VMPTRLD (Region  => VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug
           (KC.Put_Line (Item => "Error loading VMCS pointer"));
         CPU.Panic;
      end if;

      VMCS_Setup_Control_Fields;
      VMCS_Setup_Host_Fields;
      VMCS_Setup_Guest_Fields;

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

begin

   --# hide SK.VMX;

   declare
      Revision, Unused_High, VMXON_Region, VMCS_Region : SK.Word32;
      for VMXON_Region'Address use System'To_Address (VMXON_Address);
      for VMCS_Region'Address use System'To_Address (VMCS_Address);
   begin
      CPU.Get_MSR
        (Register => Constants.IA32_VMX_BASIC,
         Low      => Revision,
         High     => Unused_High);

      VMXON_Region := Revision;
      VMCS_Region  := Revision;
   end;
end SK.VMX;
