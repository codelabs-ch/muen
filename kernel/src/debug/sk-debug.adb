with Interfaces;

with System.Machine_Code;

with SK.Console;

package body SK.Debug
is

   -------------------------------------------------------------------------

   procedure Dump_Info
   is
      subtype Word64_Range is Positive range 1 .. 16;
      subtype Word64_String is String (Word64_Range);

      RSP, RAX, RBX, RCX, RDX, RSI, RDI, RBP : Word64;
      R08, R09, R10, R11, R12, R13, R14, R15 : Word64;
      CR0, CR2, CR3, CR4                     : Word64;
      Reg_Str                                : Word64_String
        := Word64_String'(others => '0');
   begin
      System.Machine_Code.Asm
        (Template => "movq %%rsp, %0",
         Outputs  => (Word64'Asm_Output ("=r", RSP)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rax, %0",
         Outputs  => (Word64'Asm_Output ("=r", RAX)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rbx, %0",
         Outputs  => (Word64'Asm_Output ("=r", RBX)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rcx, %0",
         Outputs  => (Word64'Asm_Output ("=r", RCX)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rdx, %0",
         Outputs  => (Word64'Asm_Output ("=r", RDX)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rsi, %0",
         Outputs => (Word64'Asm_Output ("=r", RSI)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rdi, %0",
         Outputs  => (Word64'Asm_Output ("=r", RDI)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%rbp, %0",
         Outputs  => (Word64'Asm_Output ("=r", RBP)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r8, %0",
         Outputs  => (Word64'Asm_Output ("=r", R08)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r9, %0",
         Outputs  => (Word64'Asm_Output ("=r", R09)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r10, %0",
         Outputs  => (Word64'Asm_Output ("=r", R10)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r11, %0",
         Outputs  => (Word64'Asm_Output ("=r", R11)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r12, %0",
         Outputs  => (Word64'Asm_Output ("=r", R12)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r13, %0",
         Outputs  => (Word64'Asm_Output ("=r", R13)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r14, %0",
         Outputs  => (Word64'Asm_Output ("=r", R14)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "movq %%r15, %0",
         Outputs  => (Word64'Asm_Output ("=r", R15)),
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

      To_Hex (Item   => RSP,
              Buffer => Reg_Str);
      Console.Put_String (Item => "RSP: ");
      Console.Put_String (Item => Reg_Str);
      Console.New_Line;

      To_Hex (Item   => RAX,
              Buffer => Reg_Str);
      Console.Put_String (Item => "RAX: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => RBX,
              Buffer => Reg_Str);
      Console.Put_String (Item => " RBX: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => RCX,
              Buffer => Reg_Str);
      Console.Put_String (Item => " RCX: ");
      Console.Put_String (Item => Reg_Str);
      Console.New_Line;

      To_Hex (Item   => RDX,
              Buffer => Reg_Str);
      Console.Put_String (Item => "RDX: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => RSI,
              Buffer => Reg_Str);
      Console.Put_String (Item => " RSI: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => RDI,
              Buffer => Reg_Str);
      Console.Put_String (Item => " RDI: ");
      Console.Put_String (Item => Reg_Str);
      Console.New_Line;

      To_Hex (Item   => RBP,
              Buffer => Reg_Str);
      Console.Put_String (Item => "RBP: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => R08,
              Buffer => Reg_Str);
      Console.Put_String (Item => " R08: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => R09,
              Buffer => Reg_Str);
      Console.Put_String (Item => " R09: ");
      Console.Put_String (Item => Reg_Str);
      Console.New_Line;

      To_Hex (Item   => R10,
              Buffer => Reg_Str);
      Console.Put_String (Item => "R10: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => R11,
              Buffer => Reg_Str);
      Console.Put_String (Item => " R11: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => R12,
              Buffer => Reg_Str);
      Console.Put_String (Item => " R12: ");
      Console.Put_String (Item => Reg_Str);
      Console.New_Line;

      To_Hex (Item   => R13,
              Buffer => Reg_Str);
      Console.Put_String (Item => "R13: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => R14,
              Buffer => Reg_Str);
      Console.Put_String (Item => " R14: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => R15,
              Buffer => Reg_Str);
      Console.Put_String (Item => " R15: ");
      Console.Put_String (Item => Reg_Str);
      Console.New_Line;

      To_Hex (Item   => CR0,
              Buffer => Reg_Str);
      Console.Put_String (Item => "CR0: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => CR2,
              Buffer => Reg_Str);
      Console.Put_String (Item => " CR2: ");
      Console.Put_String (Item => Reg_Str);
      To_Hex (Item   => CR3,
              Buffer => Reg_Str);
      Console.Put_String (Item => " CR3: ");
      Console.Put_String (Item => Reg_Str);
      Console.New_Line;

      To_Hex (Item   => CR4,
              Buffer => Reg_Str);
      Console.Put_String (Item => "CR4: ");
      Console.Put_String (Item => Reg_Str);
   end Dump_Info;

   ---------------------------------------------------------------------------

   function To_Character (Value : Word64) return Character
   is
      Result : Character;
   begin
      case Value is
         when 16#0#  => Result := '0';
         when 16#1#  => Result := '1';
         when 16#2#  => Result := '2';
         when 16#3#  => Result := '3';
         when 16#4#  => Result := '4';
         when 16#5#  => Result := '5';
         when 16#6#  => Result := '6';
         when 16#7#  => Result := '7';
         when 16#8#  => Result := '8';
         when 16#9#  => Result := '9';
         when 16#a#  => Result := 'a';
         when 16#b#  => Result := 'b';
         when 16#c#  => Result := 'c';
         when 16#d#  => Result := 'd';
         when 16#e#  => Result := 'e';
         when 16#f#  => Result := 'f';
         when others => Result := '?';
      end case;

      return Result;
   end To_Character;

   -------------------------------------------------------------------------

   procedure To_Hex
     (Item   :        Word64;
      Buffer : in out String)
   is
      Temp : Word64;
   begin
      Temp := Item;
      for Pos in reverse Buffer'Range loop
         Buffer (Pos) := To_Character (Temp mod 16);
         Temp         := Temp / 16;
         exit when Temp = 0;
      end loop;
   end To_Hex;

end SK.Debug;
