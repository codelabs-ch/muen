--# inherit
--#    SK;
package SK.CPU
is

   CR4_VMXE_FLAG : constant := 13;

   --  Execute CPUID instruction.
   procedure CPUID
      (EAX : in out SK.Word32;
       EBX :    out SK.Word32;
       ECX : in out SK.Word32;
       EDX :    out SK.Word32);
   --# derives
   --#    EAX, EBX, ECX, EDX from EAX, ECX;

   --  Halt the CPU.
   procedure Hlt;

   --  Return current value of CR0 register.
   function Get_CR0 return SK.Word64;

   --  Return current value of CR4 register.
   function Get_CR4 return SK.Word64;

   --  Set value of CR4.
   procedure Set_CR4 (Value : SK.Word64);

   --  Return current value of given model specific register.
   function Get_MSR64 (Register : SK.Word32) return SK.Word64;

   --  Return value of given MSR as low/high doublewords.
   procedure Get_MSR
     (Register :     SK.Word32;
      Low      : out SK.Word32;
      High     : out SK.Word32);
   --# derives Low, High from Register;

   --  Return current RFLAGS.
   function Get_RFLAGS return SK.Word64;

   --  Enter VMX operation.
   procedure VMXON
     (Region  :     SK.Word64;
      Success : out Boolean);

end SK.CPU;
