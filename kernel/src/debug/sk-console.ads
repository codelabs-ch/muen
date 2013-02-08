package SK.Console
is

   Console_Width  : constant Natural := 80;
   Console_Height : constant Natural := 25;

   subtype Console_Width_Range  is Natural range 1 .. Console_Width;
   subtype Console_Height_Range is Natural range 1 .. Console_Height;

   --  Clear screen.
   procedure Clear;

   --  Output a new line.
   procedure New_Line;

   --  Output given character.
   procedure Put_Char (Item : Character);

   --  Output given string.
   procedure Put_String (Item : String);

private

   --  Scroll screen if current Y position is equal to the last row.
   procedure Scroll;

end SK.Console;
