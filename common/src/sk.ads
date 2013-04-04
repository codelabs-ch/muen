package SK
is

   type Byte is mod 2**8;
   for Byte'Size use 8;

   type Word16 is mod 2**16;
   for Word16'Size use 16;

   type Word32 is mod 2**32;
   for Word32'Size use 32;

   type Word64 is mod 2**64;
   for Word64'Size use 64;

   type Word64_Pos is range 0 .. 63;

   --  Number of major frames in a scheduling plan.
   type Major_Frame_Range is range 0 .. 1;

   --  CPU registers.
   type CPU_Registers_Type is record
      RAX : Word64;
      RBX : Word64;
      RCX : Word64;
      RDX : Word64;
      RDI : Word64;
      RSI : Word64;
      RBP : Word64;
      R08 : Word64;
      R09 : Word64;
      R10 : Word64;
      R11 : Word64;
      R12 : Word64;
      R13 : Word64;
      R14 : Word64;
      R15 : Word64;
   end record;

   CPU_Null_Regs : constant CPU_Registers_Type;

   --  Subject state.
   type Subject_State_Type is record
      Launched       : Boolean;
      Regs           : CPU_Registers_Type;
      Interrupt_Info : Word64;
      RIP            : Word64;
      CS             : Word64;
      RSP            : Word64;
   end record;

   type Subject_State_Array is array (Natural range <>) of Subject_State_Type;

   --  Size of one page (4k).
   Page_Size : constant := 4096;

   --  Test if bit at given position is set.
   function Bit_Test
     (Value : Word64;
      Pos   : Word64_Pos)
      return Boolean;

   --  Set bit at given position.
   function Bit_Set
     (Value : Word64;
      Pos   : Word64_Pos)
      return Word64;

private

   CPU_Null_Regs : constant CPU_Registers_Type := CPU_Registers_Type'
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

end SK;
