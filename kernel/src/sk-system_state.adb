with SK.CPU;
with SK.Console;

package body SK.System_State
is

   CPUID_FEATURE_VMX_FLAG : constant := 5;
   CR0_PE_FLAG            : constant := 0;
   CR0_PG_FLAG            : constant := 0;
   CR4_VMXE_FLAG          : constant := 13;
   IA32_EFER_LMA_FLAG     : constant := 10;
   IA32_FCTRL_SMX_FLAG    : constant := 2;
   RFLAGS_VM_FLAG         : constant := 17;
   IA32_FEATURE_CONTROL   : constant := 16#3a#;
   IA32_VMX_CR0_FIXED0    : constant := 16#486#;
   IA32_VMX_CR0_FIXED1    : constant := 16#487#;
   IA32_VMX_CR4_FIXED0    : constant := 16#488#;
   IA32_VMX_CR4_FIXED1    : constant := 16#489#;
   IA32_EFER              : constant := 16#C000_0080#;

   -------------------------------------------------------------------------

   --  Check required VMX-fixed bits of given register, see Intel SDM 3C,
   --  appendix A.7 & A.8.
   function Fixed_Valid
     (Register : SK.Word64;
      Fixed0   : SK.Word64;
      Fixed1   : SK.Word64)
      return Boolean
   is
   begin
      return
        ((Fixed0 or  Register) = Register) and
        ((Fixed1 and Register) = Register);
   end Fixed_Valid;

   -------------------------------------------------------------------------

   --  Returns true if VMX is supported by the CPU.
   function Has_VMX_Support return Boolean
   is
      Unused_EAX, Unused_EBX, ECX, Unused_EDX : SK.Word32;
   begin
      Unused_EAX := 1;
      ECX        := 0;

      --# accept Flow, 10, Unused_EAX, "Result unused" &
      --#        Flow, 10, Unused_EBX, "Result unused" &
      --#        Flow, 10, Unused_EDX, "Result unused";
      CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => ECX,
         EDX => Unused_EDX);

      --# accept Flow, 33, Unused_EBX, "Result unused" &
      --#        Flow, 33, Unused_EDX, "Result unused";
      return SK.Bit_Test
        (Value => SK.Word64 (ECX),
         Pos   => CPUID_FEATURE_VMX_FLAG);
   end Has_VMX_Support;

   -------------------------------------------------------------------------

   function Is_Valid return Boolean
   is
      VMX_Support, VMX_Locked, Protected_Mode, Paging, IA_32e_Mode : Boolean;
      CR0_Valid, CR4_Valid, Not_Virtual_8086, Not_In_SMX           : Boolean;
   begin
      VMX_Support := Has_VMX_Support;
      pragma Debug
        (not VMX_Support,
         SK.Console.Put_Line (Item => "VMX not supported"));

      VMX_Locked := SK.Bit_Test
        (Value => CPU.Get_MSR64 (Register => IA32_FEATURE_CONTROL),
         Pos   => 0);
      pragma Debug
        (not VMX_Locked,
         SK.Console.Put_Line (Item => "IA32_FEATURE_CONTROL not locked"));

      Protected_Mode := SK.Bit_Test
        (Value => CPU.Get_CR0,
         Pos   => CR0_PE_FLAG);
      pragma Debug
        (not Protected_Mode,
         SK.Console.Put_Line (Item => "Protected mode not enabled"));

      Paging := SK.Bit_Test
        (Value => CPU.Get_CR0,
         Pos   => CR0_PG_FLAG);
      pragma Debug
        (not Paging,
         SK.Console.Put_Line (Item => "Paging not enabled"));

      IA_32e_Mode := SK.Bit_Test
        (Value => CPU.Get_MSR64 (Register => IA32_EFER),
         Pos   => IA32_EFER_LMA_FLAG);
      pragma Debug
        (not IA_32e_Mode,
         SK.Console.Put_Line (Item => "IA-32e mode not enabled"));

      Not_Virtual_8086 := not SK.Bit_Test
        (Value => CPU.Get_RFLAGS,
         Pos   => RFLAGS_VM_FLAG);
      pragma Debug
        (not Not_Virtual_8086,
         SK.Console.Put_Line (Item => "Virtual-8086 mode enabled"));

      Not_In_SMX := SK.Bit_Test
        (Value => CPU.Get_MSR64 (Register => IA32_FEATURE_CONTROL),
         Pos   => IA32_FCTRL_SMX_FLAG);
      pragma Debug
        (not Not_In_SMX,
         SK.Console.Put_Line (Item => "SMX mode enabled"));

      CR0_Valid := Fixed_Valid
        (Register => CPU.Get_CR0,
         Fixed0   => CPU.Get_MSR64 (Register => IA32_VMX_CR0_FIXED0),
         Fixed1   => CPU.Get_MSR64 (Register => IA32_VMX_CR0_FIXED1));
      pragma Debug
        (not CR0_Valid,
         SK.Console.Put_Line (Item => "CR0 is invalid"));

      CR4_Valid := Fixed_Valid
        (Register => SK.Bit_Set
           (Value => CPU.Get_CR4,
            Pos   => CR4_VMXE_FLAG),
         Fixed0   => CPU.Get_MSR64 (Register => IA32_VMX_CR4_FIXED0),
         Fixed1   => CPU.Get_MSR64 (Register => IA32_VMX_CR4_FIXED1));
      pragma Debug
        (not CR4_Valid,
         SK.Console.Put_Line (Item => "CR4 is invalid"));

      return VMX_Support and
        VMX_Locked       and
        Protected_Mode   and
        Paging           and
        IA_32e_Mode      and
        Not_Virtual_8086 and
        Not_In_SMX       and
        CR0_Valid        and
        CR4_Valid;
   end Is_Valid;

end SK.System_State;
