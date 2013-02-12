package SK.Console
is

   --  Clear screen.
   procedure Clear;

   --  Output a new line.
   procedure New_Line;

   --  Output given character.
   procedure Put_Char (Item : Character);

   --  Output given string.
   procedure Put_String (Item : String);

   --  Output given string and append a new line.
   procedure Put_Line (Item : String);

   --  Output given byte in hex.
   procedure Put_Byte (Item : Byte);

   --  Output given word in hex.
   procedure Put_Word16 (Item : Word16);

   --  Output given doubleword in hex.
   procedure Put_Word32 (Item : Word32);

   --  Output given quadword in hex.
   procedure Put_Word64 (Item : Word64);

end SK.Console;
