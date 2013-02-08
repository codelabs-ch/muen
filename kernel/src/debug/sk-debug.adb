with Interfaces;

with System.Machine_Code;

with SK.Console;

package body SK.Debug
is

   -------------------------------------------------------------------------

   procedure Isr_Dump
     (RIP    : Word64;
      CS     : Word64;
      RFLAGS : Word64;
      RSP    : Word64;
      SS     : Word64)
   is
      RAX, RBX, RCX, RDX, RSI, RDI, RBP      : Word64;
      R08, R09, R10, R11, R12, R13, R14, R15 : Word64;
      CR0, CR2, CR3, CR4                     : Word64;
   begin
      System.Machine_Code.Asm
        (Template => "movq %%rax, %0",
         Outputs  => (Word64'Asm_Output ("=m", RAX)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rbx, %0",
         Outputs  => (Word64'Asm_Output ("=m", RBX)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rcx, %0",
         Outputs  => (Word64'Asm_Output ("=m", RCX)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rdx, %0",
         Outputs  => (Word64'Asm_Output ("=m", RDX)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rsi, %0",
         Outputs  => (Word64'Asm_Output ("=m", RSI)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rdi, %0",
         Outputs  => (Word64'Asm_Output ("=m", RDI)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rbp, %0",
         Outputs  => (Word64'Asm_Output ("=m", RBP)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r8, %0",
         Outputs  => (Word64'Asm_Output ("=m", R08)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r9, %0",
         Outputs  => (Word64'Asm_Output ("=m", R09)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r10, %0",
         Outputs  => (Word64'Asm_Output ("=m", R10)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r11, %0",
         Outputs  => (Word64'Asm_Output ("=m", R11)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r12, %0",
         Outputs  => (Word64'Asm_Output ("=m", R12)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r13, %0",
         Outputs  => (Word64'Asm_Output ("=m", R13)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r14, %0",
         Outputs  => (Word64'Asm_Output ("=m", R14)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r15, %0",
         Outputs  => (Word64'Asm_Output ("=m", R15)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%cr0, %0",
         Outputs  => (Word64'Asm_Output ("=r", CR0)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%cr2, %0",
         Outputs  => (Word64'Asm_Output ("=r", CR2)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%cr3, %0",
         Outputs  => (Word64'Asm_Output ("=r", CR3)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%cr4, %0",
         Outputs  => (Word64'Asm_Output ("=r", CR4)),
         Volatile => True);

      Console.Put_String ("RIP: ");
      Console.Put_Word64 (Item => RIP);
      Console.Put_String (" CS : ");
      Console.Put_Word16 (Item => Word16 (CS));
      Console.New_Line;
      Console.Put_String ("RSP: ");
      Console.Put_Word64 (Item => RSP);
      Console.Put_String (" SS : ");
      Console.Put_Word16 (Item => Word16 (SS));
      Console.New_Line;

      Console.Put_String (Item => "RAX: ");
      Console.Put_Word64 (Item => RAX);
      Console.Put_String (Item => " RBX: ");
      Console.Put_Word64 (Item => RBX);
      Console.Put_String (Item => " RCX: ");
      Console.Put_Word64 (Item => RCX);
      Console.New_Line;

      Console.Put_String (Item => "RDX: ");
      Console.Put_Word64 (Item => RDX);
      Console.Put_String (Item => " RSI: ");
      Console.Put_Word64 (Item => RSI);
      Console.Put_String (Item => " RDI: ");
      Console.Put_Word64 (Item => RDI);
      Console.New_Line;

      Console.Put_String (Item => "RBP: ");
      Console.Put_Word64 (Item => RBP);
      Console.Put_String (Item => " R08: ");
      Console.Put_Word64 (Item => R08);
      Console.Put_String (Item => " R09: ");
      Console.Put_Word64 (Item => R09);
      Console.New_Line;

      Console.Put_String (Item => "R10: ");
      Console.Put_Word64 (Item => R10);
      Console.Put_String (Item => " R11: ");
      Console.Put_Word64 (Item => R11);
      Console.Put_String (Item => " R12: ");
      Console.Put_Word64 (Item => R12);
      Console.New_Line;

      Console.Put_String (Item => "R13: ");
      Console.Put_Word64 (Item => R13);
      Console.Put_String (Item => " R14: ");
      Console.Put_Word64 (Item => R14);
      Console.Put_String (Item => " R15: ");
      Console.Put_Word64 (Item => R15);
      Console.New_Line;

      Console.Put_String (Item => "CR0: ");
      Console.Put_Word64 (Item => CR0);
      Console.Put_String (Item => " CR2: ");
      Console.Put_Word64 (Item => CR2);
      Console.Put_String (Item => " CR3: ");
      Console.Put_Word64 (Item => CR3);
      Console.New_Line;

      Console.Put_String (Item => "CR4: ");
      Console.Put_Word64 (Item => CR4);
      Console.Put_String (" EFL: ");
      Console.Put_Word32 (Item => Word32 (RFLAGS));
      Console.New_Line;
   end Isr_Dump;

end SK.Debug;
