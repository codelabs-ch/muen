package SK.Console.Backend
is

   --  Clear screen and set initial cursor position.
   procedure Init;

   --  Start new line and scroll screen if necessary.
   procedure New_Line;

   --  Print character at current cursor position.
   procedure Put_Char (Item : Character);

end SK.Console.Backend;
