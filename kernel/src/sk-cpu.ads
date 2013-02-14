--# inherit
--#    SK,
--#    X86_64;
package SK.CPU
is

   RFLAGS_CF_FLAG : constant := 0;
   RFLAGS_ZF_FLAG : constant := 6;
   CR4_VMXE_FLAG  : constant := 13;

   --  Execute CPUID instruction.
   procedure CPUID
     (EAX : in out SK.Word32;
      EBX :    out SK.Word32;
      ECX : in out SK.Word32;
      EDX :    out SK.Word32);
   --# global
   --#    X86_64.State;
   --# derives
   --#    EAX, EBX, ECX, EDX from X86_64.State, EAX, ECX;

   --  Halt the CPU.
   procedure Hlt;

   --  Panic.
   procedure Panic;
   --# global
   --#    X86_64.State;
   --# derives
   --#    X86_64.State from *;

   --  Return current value of CR0 register.
   function Get_CR0 return SK.Word64;
   --# global
   --#    X86_64.State;

   --  Return current value of CR4 register.
   function Get_CR4 return SK.Word64;
   --# global
   --#    X86_64.State;

   --  Set value of CR4.
   procedure Set_CR4 (Value : SK.Word64);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Value;

   --  Return current value of given model specific register.
   function Get_MSR64 (Register : SK.Word32) return SK.Word64;
   --# global
   --#    X86_64.State;

   --  Return value of given MSR as low/high doublewords.
   procedure Get_MSR
     (Register :     SK.Word32;
      Low      : out SK.Word32;
      High     : out SK.Word32);
   --# global
   --#    X86_64.State;
   --# derives
   --#    Low, High from X86_64.State, Register;

   --  Return current RFLAGS.
   function Get_RFLAGS return SK.Word64;
   --# global
   --#    X86_64.State;

   --  Enter VMX operation.
   procedure VMXON
     (Region  :     SK.Word64;
      Success : out Boolean);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Region &
   --#    Success      from X86_64.State, Region;

   --  Launch VM designated by current VMCS.
   procedure VMLAUNCH;
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;

   procedure VMCLEAR
     (Region  :     SK.Word64;
      Success : out Boolean);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Region &
   --#    Success      from X86_64.State, Region;

end SK.CPU;
