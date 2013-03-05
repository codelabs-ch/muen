with System;

generic

   --  Console width range.
   type Width_Type is range <>;

   --  Console heigth range.
   type Height_Type is range <>;

   --  Base address of video framebuffer.
   Base_Address : System.Address;

package SK.Console_VGA
is

   --  Clear screen and set initial cursor position.
   procedure Init;

   --  Start new line and scroll screen if necessary.
   procedure New_Line;

   --  Print character at current cursor position.
   procedure Put_Char (Item : Character);

   --  Set cursor position.
   procedure Set_Position
     (X : Width_Type;
      Y : Height_Type);

end SK.Console_VGA;
