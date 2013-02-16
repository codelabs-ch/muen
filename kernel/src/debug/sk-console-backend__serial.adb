with System.Machine_Code;

package body SK.Console.Backend
is

   --  Intel AMT SoL port address.
   Port    : constant := 16#50b0#;
   --  Baud rate: 115200
   Divisor : constant := 1;

   -------------------------------------------------------------------------

   --  Receive byte from given port.
   procedure Inb
     (Port  :     Word16;
      Value : out Byte)
   is
   begin
      System.Machine_Code.Asm
        (Template => "inb %1, %0",
         Inputs   => (Word16'Asm_Input ("d", Port)),
         Outputs  => (Byte'Asm_Output ("=a", Value)),
         Volatile => True);
   end Inb;

   -------------------------------------------------------------------------

   function Empty_Send_Buffer return Boolean
   is
      Data : Byte;
   begin
      Inb (Port  => Port + 5,
           Value => Data);
      return (Data and 16#20#) /= 0;
   end Empty_Send_Buffer;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin

      --  Disable interrupts.

      Outb (Port  => Port + 1,
            Value => 0);

      --  Enable DLAB.

      Outb (Port  => Port + 3,
            Value => 16#80#);

      --  Set divisor (least/most significant byte).

      Outb (Port  => Port,
            Value => Divisor);
      Outb (Port  => Port + 1,
            Value => 0);

      --  Clear DLAB and set 8 bits, no parity, one stop bit (8N1).

      Outb (Port  => Port + 3,
            Value => 3);

      --  Enable FIFO.

      Outb (Port  => Port + 2,
            Value => 16#C7#);

      --  IRQS enabled, RTS/DSR set.

      Outb (Port  => Port + 4,
            Value => 16#0B#);
   end Init;

   -------------------------------------------------------------------------

   procedure New_Line
   is
   begin

      --#  Newline + Linefeed

      Put_Char (Item => Character'Val (10));
      Put_Char (Item => Character'Val (12));
   end New_Line;

   -------------------------------------------------------------------------

   procedure Put_Char (Item : Character)
   is
   begin
      while not Empty_Send_Buffer loop
         null;
      end loop;

      Outb (Port  => Port,
            Value => Character'Pos (Item));
   end Put_Char;

end SK.Console.Backend;
