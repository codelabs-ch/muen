with System.Machine_Code;

with SK.CPU;
with SK.KC;

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

      KC.Put_Line ("[KERNEL PANIC]");

      KC.Put_String ("Vector: ");
      KC.Put_Byte (Item => Byte (Vec));
      KC.Put_String (", Error: ");
      KC.Put_Word64 (Item => Err);
      KC.New_Line;
      KC.New_Line;

      KC.Put_String ("RIP: ");
      KC.Put_Word64 (Item => RIP);
      KC.Put_String (" CS : ");
      KC.Put_Word16 (Item => Word16 (CS));
      KC.New_Line;
      KC.Put_String ("RSP: ");
      KC.Put_Word64 (Item => RSP);
      KC.Put_String (" SS : ");
      KC.Put_Word16 (Item => Word16 (SS));
      KC.New_Line;

      KC.Put_String (Item => "RAX: ");
      KC.Put_Word64 (Item => RAX);
      KC.Put_String (Item => " RBX: ");
      KC.Put_Word64 (Item => RBX);
      KC.Put_String (Item => " RCX: ");
      KC.Put_Word64 (Item => RCX);
      KC.New_Line;

      KC.Put_String (Item => "RDX: ");
      KC.Put_Word64 (Item => RDX);
      KC.Put_String (Item => " RSI: ");
      KC.Put_Word64 (Item => RSI);
      KC.Put_String (Item => " RDI: ");
      KC.Put_Word64 (Item => RDI);
      KC.New_Line;

      KC.Put_String (Item => "RBP: ");
      KC.Put_Word64 (Item => RBP);
      KC.Put_String (Item => " R08: ");
      KC.Put_Word64 (Item => R08);
      KC.Put_String (Item => " R09: ");
      KC.Put_Word64 (Item => R09);
      KC.New_Line;

      KC.Put_String (Item => "R10: ");
      KC.Put_Word64 (Item => R10);
      KC.Put_String (Item => " R11: ");
      KC.Put_Word64 (Item => R11);
      KC.Put_String (Item => " R12: ");
      KC.Put_Word64 (Item => R12);
      KC.New_Line;

      KC.Put_String (Item => "R13: ");
      KC.Put_Word64 (Item => R13);
      KC.Put_String (Item => " R14: ");
      KC.Put_Word64 (Item => R14);
      KC.Put_String (Item => " R15: ");
      KC.Put_Word64 (Item => R15);
      KC.New_Line;

      KC.Put_String (Item => "CR0: ");
      KC.Put_Word64 (Item => CR0);
      KC.Put_String (Item => " CR2: ");
      KC.Put_Word64 (Item => CR2);
      KC.Put_String (Item => " CR3: ");
      KC.Put_Word64 (Item => CR3);
      KC.New_Line;

      KC.Put_String (Item => "CR4: ");
      KC.Put_Word64 (Item => CR4);
      KC.Put_String (" EFL: ");
      KC.Put_Word32 (Item => Word32 (RFL));
      KC.New_Line;

      CPU.Hlt;
   end Isr_Dump;

end SK.Debug;
