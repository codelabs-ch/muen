package SK.Console_VGA
is

   Console_Width  : constant Natural := 80;
   Console_Height : constant Natural := 25;

   subtype Console_Width_Range  is Natural range 1 .. Console_Width;
   subtype Console_Height_Range is Natural range 1 .. Console_Height;

   --  Clear screen and set initial cursor position.
   procedure Init;

   --  Start new line and scroll screen if necessary.
   procedure New_Line;

   --  Print character at current cursor position.
   procedure Put_Char (Item : Character);

   --  Set cursor position.
   procedure Set_Position
     (X : Console_Width_Range;
      Y : Console_Height_Range);

end SK.Console_VGA;
