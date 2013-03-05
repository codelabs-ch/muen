--# inherit
--#    SK,
--#    X86_64;
package SK.CPU
is

   --  General purpose registers.
   type Registers_Type is private;

   --  Save CPU register values.
   procedure Save_Registers
     (Regs : out Registers_Type;
      RAX  :     SK.Word64;
      RBX  :     SK.Word64;
      RCX  :     SK.Word64;
      RDX  :     SK.Word64;
      RDI  :     SK.Word64;
      RSI  :     SK.Word64;
      RBP  :     SK.Word64;
      R08  :     SK.Word64;
      R09  :     SK.Word64;
      R10  :     SK.Word64;
      R11  :     SK.Word64;
      R12  :     SK.Word64;
      R13  :     SK.Word64;
      R14  :     SK.Word64;
      R15  :     SK.Word64);
   --# derives
   --#    Regs from
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
   --#       R15;

   --  Restore CPU register values.
   procedure Restore_Registers (Regs : Registers_Type);
   --# global
   --#    X86_64.State;
   --# derives
   --#    X86_64.State from *, Regs;
   pragma Inline_Always (Restore_Registers);

   Null_Regs : constant Registers_Type;

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

   --  Return current value of CR3 register.
   function Get_CR3 return SK.Word64;
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
   procedure VMLAUNCH (Success : out Boolean);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from * &
   --#    Success      from X86_64.State;
   pragma Inline_Always (VMLAUNCH);

   --  Resume VM designated by current VMCS.
   procedure VMRESUME (Success : out Boolean);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from * &
   --#    Success      from X86_64.State;
   pragma Inline_Always (VMRESUME);

   procedure VMCLEAR
     (Region  :     SK.Word64;
      Success : out Boolean);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Region &
   --#    Success      from X86_64.State, Region;

   procedure VMPTRLD
     (Region  :     SK.Word64;
      Success : out Boolean);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Region &
   --#    Success      from X86_64.State, Region;

   procedure VMREAD
     (Field   :     SK.Word64;
      Value   : out SK.Word64;
      Success : out Boolean);
   --# global
   --#    X86_64.State;
   --# derives
   --#    Value, Success from X86_64.State, Field;

   procedure VMWRITE
     (Field   :     SK.Word64;
      Value   :     SK.Word64;
      Success : out Boolean);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State, Success from X86_64.State, Field, Value;

private

   type Registers_Type is record
      RAX : SK.Word64;
      RBX : SK.Word64;
      RCX : SK.Word64;
      RDX : SK.Word64;
      RDI : SK.Word64;
      RSI : SK.Word64;
      RBP : SK.Word64;
      R08 : SK.Word64;
      R09 : SK.Word64;
      R10 : SK.Word64;
      R11 : SK.Word64;
      R12 : SK.Word64;
      R13 : SK.Word64;
      R14 : SK.Word64;
      R15 : SK.Word64;
   end record;

   Null_Regs : constant Registers_Type := Registers_Type'
     (RAX => 0,
      RBX => 0,
      RCX => 0,
      RDX => 0,
      RDI => 0,
      RSI => 0,
      RBP => 0,
      R08 => 0,
      R09 => 0,
      R10 => 0,
      R11 => 0,
      R12 => 0,
      R13 => 0,
      R14 => 0,
      R15 => 0);

end SK.CPU;
