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

   --  Output given word in hex.
   procedure Put_Word16 (Item : Word16);

   --  Output given quadword in hex.
   procedure Put_Word64 (Item : Word64);

end SK.Console;
