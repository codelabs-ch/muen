with System.Machine_Code;

package body SK.CPU
is

   -------------------------------------------------------------------------

   procedure CPUID
     (EAX : in out SK.Word32;
      EBX :    out SK.Word32;
      ECX : in out SK.Word32;
      EDX :    out SK.Word32)
   is
      --# hide CPUID;
   begin
      System.Machine_Code.Asm
        (Template => "cpuid",
         Inputs   => (SK.Word32'Asm_Input ("a", EAX),
                      SK.Word32'Asm_Input ("c", ECX)),
         Outputs  => (SK.Word32'Asm_Output ("=a", EAX),
                      SK.Word32'Asm_Output ("=b", EBX),
                      SK.Word32'Asm_Output ("=c", ECX),
                      SK.Word32'Asm_Output ("=d", EDX)),
         Volatile => True);
   end CPUID;

   -------------------------------------------------------------------------

   function Get_CR0 return SK.Word64
   is
      --# hide Get_CR0;

      Result : SK.Word64;
   begin
      System.Machine_Code.Asm
        (Template => "movq %%cr0, %0",
         Outputs  => (SK.Word64'Asm_Output ("=r", Result)),
         Volatile => True);
      return Result;
   end Get_CR0;

   -------------------------------------------------------------------------

   function Get_CR3 return SK.Word64
   is
      --# hide Get_CR3;

      Result : SK.Word64;
   begin
      System.Machine_Code.Asm
        (Template => "movq %%cr3, %0",
         Outputs  => (SK.Word64'Asm_Output ("=r", Result)),
         Volatile => True);
      return Result;
   end Get_CR3;

   -------------------------------------------------------------------------

   function Get_CR4 return SK.Word64
   is
      --# hide Get_CR4;

      Result : SK.Word64;
   begin
      System.Machine_Code.Asm
        (Template => "movq %%cr4, %0",
         Outputs  => (SK.Word64'Asm_Output ("=r", Result)),
         Volatile => True);
      return Result;
   end Get_CR4;

   -------------------------------------------------------------------------

   procedure Get_MSR
     (Register :     SK.Word32;
      Low      : out SK.Word32;
      High     : out SK.Word32)
   is
      --# hide Get_MSR;
   begin
      System.Machine_Code.Asm
        (Template => "rdmsr",
         Inputs   => (SK.Word32'Asm_Input ("c", Register)),
         Outputs  => (SK.Word32'Asm_Output ("=d", High),
                      SK.Word32'Asm_Output ("=a", Low)),
         Volatile => True);
   end Get_MSR;

   -------------------------------------------------------------------------

   function Get_MSR64 (Register : SK.Word32) return SK.Word64
   is
      Low_Dword, High_Dword : SK.Word32;
   begin
      Get_MSR (Register => Register,
               Low      => Low_Dword,
               High     => High_Dword);
      return 2 ** 31 * SK.Word64 (High_Dword) + SK.Word64 (Low_Dword);
   end Get_MSR64;

   -------------------------------------------------------------------------

   function Get_RFLAGS return SK.Word64
   is
      --# hide Get_RFLAGS;

      Result : SK.Word64;
   begin
      System.Machine_Code.Asm
        (Template => "pushf; pop %0",
         Outputs  => (SK.Word64'Asm_Output ("=m", Result)),
         Volatile => True,
         Clobber  => "memory");
      return Result;
   end Get_RFLAGS;

   -------------------------------------------------------------------------

   procedure Hlt
   is
      --# hide Hlt;
   begin
      System.Machine_Code.Asm
        (Template => "hlt",
         Volatile => True);
   end Hlt;

   -------------------------------------------------------------------------

   procedure Lidt (Address : SK.Word64)
   is
      --# hide Lidt;
   begin
      System.Machine_Code.Asm
        (Template => "lidt (%0)",
         Inputs   => (SK.Word64'Asm_Input ("r", Address)),
         Volatile => True);
   end Lidt;

   -------------------------------------------------------------------------

   procedure Panic
   is
      --# hide Panic;
   begin
      System.Machine_Code.Asm
        (Template => "ud2",
         Volatile => True);
   end Panic;

   -------------------------------------------------------------------------

   procedure Restore_Registers (Regs : SK.CPU_Registers_Type)
   is
      --# hide Restore_Registers;
   begin
      System.Machine_Code.Asm
        (Template => "movq %0, %%rbx;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.RBX)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%rcx;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.RCX)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%rdx;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.RDX)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%rdi;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.RDI)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%rsi;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.RSI)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%r8;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.R08)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%r9;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.R09)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%r10;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.R10)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%r11;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.R11)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%r12;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.R12)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%r13;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.R13)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%r14;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.R14)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%r15;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.R15)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%rax; push %%rax",
         Inputs   => (SK.Word64'Asm_Input ("m", Regs.RBP)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %0, %%rax;",
         Inputs   => (SK.Word64'Asm_Input ("a", Regs.RAX)),
         Volatile => True);

      --  The RBP register must be written as the final value since it will
      --  break any RBP-relative adressing.

      System.Machine_Code.Asm
        (Template => "popq %%rbp;",
         Volatile => True);
   end Restore_Registers;

   -------------------------------------------------------------------------

   procedure Set_CR4 (Value : SK.Word64)
   is
      --# hide Set_CR4;
   begin
      System.Machine_Code.Asm
        (Template => "movq %0, %%cr4",
         Inputs   => (SK.Word64'Asm_Input ("r", Value)),
         Volatile => True);
   end Set_CR4;

   -------------------------------------------------------------------------

   procedure Set_Stack (Address : SK.Word64)
   is
      --# hide Set_Stack;
   begin
      System.Machine_Code.Asm
        (Template => "mov %0, %%rsp; mov %%rsp, %%rbp",
         Inputs   => (SK.Word64'Asm_Input ("g", Address)),
         Volatile => True);
   end Set_Stack;

   -------------------------------------------------------------------------

   procedure VMCLEAR
     (Region  :     SK.Word64;
      Success : out Boolean)
   is
      --# hide VMCLEAR;
   begin
      System.Machine_Code.Asm
        (Template => "vmclear %1; seta %0",
         Inputs   => (Word64'Asm_Input ("m", Region)),
         Outputs  => (Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc",
         Volatile => True);
   end VMCLEAR;

   -------------------------------------------------------------------------

   procedure VMLAUNCH
   is
      --# hide VMLAUNCH;
   begin
      System.Machine_Code.Asm
        (Template => "vmlaunch",
         Volatile => True);
   end VMLAUNCH;

   -------------------------------------------------------------------------

   procedure VMPTRLD
     (Region  :     SK.Word64;
      Success : out Boolean)
   is
      --# hide VMPTRLD;
   begin
      System.Machine_Code.Asm
        (Template => "vmptrld %1; seta %0",
         Inputs   => (Word64'Asm_Input ("m", Region)),
         Outputs  => (Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc",
         Volatile => True);
   end VMPTRLD;

   -------------------------------------------------------------------------

   procedure VMREAD
     (Field   :     SK.Word64;
      Value   : out SK.Word64;
      Success : out Boolean)
   is
      --# hide VMREAD;
   begin
      System.Machine_Code.Asm
        (Template => "vmread %2, %0; seta %1",
         Inputs   => (Word64'Asm_Input ("r", Field)),
         Outputs  => (Word64'Asm_Output ("=rm", Value),
                      Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc",
         Volatile => True);
   end VMREAD;

   -------------------------------------------------------------------------

   procedure VMRESUME
   is
   --# hide VMRESUME;
   begin
      System.Machine_Code.Asm
        (Template => "vmresume",
         Volatile => True);
   end VMRESUME;

   -------------------------------------------------------------------------

   procedure VMWRITE
     (Field   :     SK.Word64;
      Value   :     SK.Word64;
      Success : out Boolean)
   is
      --# hide VMWRITE;
   begin
      System.Machine_Code.Asm
        (Template => "vmwrite %1, %2; seta %0",
         Inputs   => (Word64'Asm_Input ("rm", Value),
                      Word64'Asm_Input ("r", Field)),
         Outputs  => (Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc",
         Volatile => True);
   end VMWRITE;

   -------------------------------------------------------------------------

   procedure VMXON
     (Region  :     SK.Word64;
      Success : out Boolean)
   is
      --# hide VMXON;
   begin
      System.Machine_Code.Asm
        (Template => "vmxon %1; seta %0",
         Inputs   => (Word64'Asm_Input ("m", Region)),
         Outputs  => (Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc",
         Volatile => True);
   end VMXON;

   -------------------------------------------------------------------------

   procedure Write_MSR
     (Register : SK.Word32;
      Low      : SK.Word32;
      High     : SK.Word32)
   is
      --# hide Write_MSR;
   begin
      System.Machine_Code.Asm
        (Template => "wrmsr",
         Inputs   => (Word32'Asm_Input ("a", Low),
                      Word32'Asm_Input ("d", High),
                      Word32'Asm_Input ("c", Register)),
         Volatile => True);
   end Write_MSR;

   -------------------------------------------------------------------------

   procedure Write_MSR64
     (Register : SK.Word32;
      Value    : SK.Word64)
   is
      Low_Dword, High_Dword : SK.Word32;
   begin
      Low_Dword  := SK.Word32'Mod (Value);
      High_Dword := SK.Word32'Mod (Value / 2 ** 31);

      Write_MSR (Register => Register,
                 Low      => Low_Dword,
                 High     => High_Dword);
   end Write_MSR64;

end SK.CPU;
