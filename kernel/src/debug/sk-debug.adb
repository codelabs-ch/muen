with Interfaces;

with System.Machine_Code;

with SK.CPU;
with SK.Console;

package body SK.Debug
is

   -------------------------------------------------------------------------

   procedure Isr_Dump
     (RDI : Word64; RSI : Word64; RDX : Word64; RCX : Word64; R08 : Word64;
      R09 : Word64; RAX : Word64; RBX : Word64; RBP : Word64; R10 : Word64;
      R11 : Word64; R12 : Word64; R13 : Word64; R14 : Word64; R15 : Word64;
      Vec : Word64; Err : Word64; RIP : Word64; CS  : Word64; RFL : Word64;
      RSP : Word64; SS  : Word64)
   is
      CR0, CR2, CR3, CR4 : Word64;
   begin
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

      Console.Put_Line ("[KERNEL PANIC]");

      Console.Put_String ("Vector: ");
      Console.Put_Byte (Item => Byte (Vec));
      Console.Put_String (", Error: ");
      Console.Put_Word64 (Item => Err);
      Console.New_Line;
      Console.New_Line;

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
      Console.Put_Word32 (Item => Word32 (RFL));
      Console.New_Line;

      CPU.Hlt;
   end Isr_Dump;

end SK.Debug;
