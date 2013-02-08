with Ada.Unchecked_Conversion;

with System;
with System.Machine_Code;

with SK.Debug;

package body SK.Interrupts
is

   --  Interrupt vector number.
   subtype Vector_Type is SK.Byte range 0 .. 255;

   -- Interrupt gate descriptor.
   type Gate_Type is record
      Offset_15_00     : SK.Word16;
      Segment_Selector : SK.Word16;
      Flags            : SK.Word16;
      Offset_31_16     : SK.Word16;
      Offset_63_32     : SK.Word32;
      Reserved         : SK.Word32;
   end record;

   for Gate_Type use record
      Offset_15_00     at  0 range 0 .. 15;
      Segment_Selector at  2 range 0 .. 15;
      Flags            at  4 range 0 .. 15;
      Offset_31_16     at  6 range 0 .. 15;
      Offset_63_32     at  8 range 0 .. 31;
      Reserved         at 12 range 0 .. 31;
   end record;
   for Gate_Type'Size use 16 * 8;

   type IDT_Type is array (Vector_Type) of Gate_Type;

   type IDT_Pointer_Type is record
      Limit : SK.Word16;
      Base  : System.Address;
   end record;

   for IDT_Pointer_Type use record
      Limit at 0 range 0 .. 15;
      Base  at 2 range 0 .. 63;
   end record;
   for IDT_Pointer_Type'Size use 80;

   --  IDT
   IDT : IDT_Type;
   for IDT'Alignment use 8;

   --  Interrupt table pointer, loaded into IDTR
   IDT_Pointer : IDT_Pointer_Type;

   function To_Word64 is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => SK.Word64);

   -------------------------------------------------------------------------

   procedure Isr_UD
   is
      --# hide Isr_UD;

      RIP, CS, RFLAGS, RSP, SS : Word64;
   begin

      --  Get RIP, CS, RFLAGS, RSP and SS from the stack.
      --  See Intel SDM, Volume 3A, chapter 6.14.2 "64-Bit Mode Stack Frame".

      System.Machine_Code.Asm
        (Template => "pushq 0x8(%%rbp); popq %0",
         Outputs  => (Word64'Asm_Output ("=m", RIP)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "pushq 0x10(%%rbp); popq %0",
         Outputs  => (Word64'Asm_Output ("=m", CS)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "pushq 0x18(%%rbp); popq %0",
         Outputs  => (Word64'Asm_Output ("=m", RFLAGS)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "pushq 0x20(%%rbp); popq %0",
         Outputs  => (Word64'Asm_Output ("=m", RSP)),
         Volatile => True);
      System.Machine_Code.Asm
        (Template => "pushq 0x28(%%rbp); popq %0",
         Outputs  => (Word64'Asm_Output ("=m", SS)),
         Volatile => True);

      pragma Debug
        (SK.Debug.Isr_Dump
           (RIP    => RIP,
            CS     => CS,
            RFLAGS => RFLAGS,
            RSP    => RSP,
            SS     => SS));
      System.Machine_Code.Asm
        (Template => "hlt",
         Volatile => True);
   end Isr_UD;

   -------------------------------------------------------------------------

   procedure Init
   is
      --# hide Init;

      Temp : SK.Word64;
   begin
      IDT := IDT_Type'
        (others => Gate_Type'
           (Offset_15_00     => 0,
            Segment_Selector => 0,
            Flags            => 0,
            Offset_31_16     => 0,
            Offset_63_32     => 0,
            Reserved         => 0));

      --  Install #UD interrupt service routine.

      Temp := To_Word64 (Isr_Ud'Address);
      IDT (6) := Gate_Type'
        (Offset_15_00     => SK.Word16
           (Temp and 16#0000_0000_0000_ffff#),
         Segment_Selector => 16#0008#,
         Flags            => 16#8e00#,
         Offset_31_16     => SK.Word16
           ((Temp and 16#0000_0000_ffff_0000#) / 2**16),
         Offset_63_32     => SK.Word32
           ((Temp and 16#ffff_ffff_0000_0000#) / 2**32),
         Reserved         => 0);
   end Init;

   -------------------------------------------------------------------------

   procedure Load
   is
      --# hide Load;
   begin
      IDT_Pointer := IDT_Pointer_Type'
        (Limit => SK.Word16 (16 * IDT'Last) - 1,
         Base  => IDT'Address);
      System.Machine_Code.Asm
        (Template => "lidt (%0)",
         Inputs   => (System.Address'Asm_Input ("r", IDT_Pointer'Address)),
         Volatile => True);
   end Load;

end SK.Interrupts;
