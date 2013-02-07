package SK.Console
--# own
--#    out Screen,
--#        Buffer,
--#        Cursor_Position;
--# initializes
--#    Buffer,
--#    Cursor_Position;
is

   Console_Width  : constant Natural := 80;
   Console_Height : constant Natural := 25;

   subtype Console_Width_Range  is Natural range 1 .. Console_Width;
   subtype Console_Height_Range is Natural range 1 .. Console_Height;

   --  Clear screen.
   procedure Clear;
   --# global
   --#    out Cursor_Position;
   --#    out Buffer;
   --#    out Screen;
   --# derives
   --#    Cursor_Position, Buffer, Screen from ;

   --  Output a new line.
   procedure New_Line;
   --# global
   --#    in out Cursor_Position;
   --#    in out Buffer;
   --#       out Screen;
   --# derives
   --#    Cursor_Position from *                  &
   --#    Buffer          from *, Cursor_Position &
   --#    Screen          from Buffer, Cursor_Position;

   --  Output given character.
   procedure Put_Char (Item : Character);
   --# global
   --#    in out Cursor_Position;
   --#    in out Buffer;
   --#       out Screen;
   --# derives
   --#    Cursor_Position from * &
   --#    Buffer, Screen  from Buffer, Item, Cursor_Position;

   --  Output given string.
   procedure Put_String (Item : String);
   --# global
   --#    in out Cursor_Position;
   --#    in out Buffer;
   --#       out Screen;
   --# derives
   --#    Cursor_Position from *, Item &
   --#    Buffer, Screen  from Buffer, Item, Cursor_Position;

   --  Write contents of buffer to screen.
   procedure Update_Screen;
   --# global
   --#    in     Buffer;
   --#       out Screen;
   --# derives
   --#    Screen from Buffer;

private

   --  Scroll screen if current Y position is equal to the last row.
   procedure Scroll;
   --# global
   --#    in out Buffer;
   --#       out Screen;
   --# derives
   --#    Buffer from * &
   --#    Screen from Buffer;

end SK.Console;
