with SK.CPU;
with SK.Console;

package body SK.System_State
is

   CPUID_FEATURE_VMX_FLAG : constant := 5;
   CR0_PE_FLAG            : constant := 0;
   CR0_PG_FLAG            : constant := 0;
   IA32_VMX_CR0_FIXED0    : constant := 16#486#;
   IA32_VMX_CR0_FIXED1    : constant := 16#487#;

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
      VMX_Support, Protected_Mode, Paging, CR0_Valid : Boolean;
   begin
      VMX_Support := Has_VMX_Support;
      pragma Debug (not VMX_Support,
                    SK.Console.Put_Line (Item => "VMX not supported"));

      Protected_Mode := SK.Bit_Test (Value => CPU.Get_CR0,
                                     Pos   => CR0_PE_FLAG);
      pragma Debug
        (not Protected_Mode,
         SK.Console.Put_Line (Item => "Protected mode not enabled"));

      Paging := SK.Bit_Test (Value => CPU.Get_CR0,
                             Pos   => CR0_PG_FLAG);
      pragma Debug
        (not Paging,
         SK.Console.Put_Line (Item => "Paging not enabled"));

      CR0_Valid := Fixed_Valid
        (Register => CPU.Get_CR0,
         Fixed0   => CPU.Get_MSR (Register => IA32_VMX_CR0_FIXED0),
         Fixed1   => CPU.Get_MSR (Register => IA32_VMX_CR0_FIXED1));
      pragma Debug
        (not CR0_Valid,
         SK.Console.Put_Line (Item => "CR0 is invalid"));

      return VMX_Support and Protected_Mode and Paging and CR0_Valid;
   end Is_Valid;

end SK.System_State;
