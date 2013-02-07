with System;

package body SK.Console
--# own Cursor_Position is Cur_X, Cur_Y;
is

   type VGA_Color_Type is
     (Black,
      Blue,
      Green,
      Cyan,
      Red,
      Magenta,
      Brown,
      Light_Grey,
      Dark_Grey,
      Light_Blue,
      Light_Green,
      Light_Cyan,
      Light_Red,
      Light_Magenta,
      Yellow,
      White);

   for VGA_Color_Type use
     (Black         => 16#0#,
      Blue          => 16#1#,
      Green         => 16#2#,
      Cyan          => 16#3#,
      Red           => 16#4#,
      Magenta       => 16#5#,
      Brown         => 16#6#,
      Light_Grey    => 16#7#,
      Dark_Grey     => 16#8#,
      Light_Blue    => 16#9#,
      Light_Green   => 16#A#,
      Light_Cyan    => 16#B#,
      Light_Red     => 16#C#,
      Light_Magenta => 16#D#,
      Yellow        => 16#E#,
      White         => 16#F#);
   for VGA_Color_Type'Size use 4;

   type Screen_Cell_Type is record
      Char     : Character;
      FG_Color : VGA_Color_Type;
      BG_Color : VGA_Color_Type;
   end record;

   for Screen_Cell_Type use record
      Char     at 0 range 0 .. 7;
      FG_Color at 1 range 0 .. 3;
      BG_Color at 1 range 4 .. 7;
   end record;
   for Screen_Cell_Type'Size use 16;

   type Screen_Row_Type is array (Console_Width_Range) of Screen_Cell_Type;
   --  VGA screen row.

   type Screen_Type is array (Console_Height_Range) of Screen_Row_Type;
   --  VGA screen.

   Cur_X  : Console_Width_Range;
   Cur_Y  : Console_Height_Range;
   Buffer : Screen_Type;
   Screen : Screen_Type;
   pragma Import (Ada, Screen);
   for Screen'Address use System'To_Address (16#000B_8000#);

   -------------------------------------------------------------------------

   procedure Update_Screen
   is
   begin
      for Y in Console_Height_Range loop
         for X in Console_Width_Range loop
            Screen (Y)(X) := Buffer (Y)(X);
         end loop;
      end loop;
   end Update_Screen;

   -------------------------------------------------------------------------

   procedure Clear
   --# global
   --#    out Cur_X;
   --#    out Cur_Y;
   --#    out Buffer;
   --#    out Screen;
   --# derives
   --#    Cur_X, Cur_Y, Buffer, Screen from ;
   --# post
   --#    Cur_X = Console_Width_Range'First and
   --#    Cur_Y = Console_Height_Range'First;
   is
   begin
      Buffer := Screen_Type'
        (others => Screen_Row_Type'
           (others => Screen_Cell_Type'
              (Char     => ' ',
               FG_Color => White,
               BG_Color => Black)));
      Update_Screen;

      Cur_X := Console_Width_Range'First;
      Cur_Y := Console_Height_Range'First;
   end Clear;

   -------------------------------------------------------------------------

   procedure Scroll
   is
      subtype Console_To_Last_Row is Console_Height_Range range
        Console_Height_Range'First .. Console_Height_Range'Last - 1;
   begin
      for Y in Console_To_Last_Row
      loop
         Buffer (Y) := Buffer (Y + 1);
      end loop;

      Buffer (Console_Height_Range'Last) := Screen_Row_Type'
        (others => Screen_Cell_Type'
           (Char     => ' ',
            FG_Color => White,
            BG_Color => Black));

      Update_Screen;
   end Scroll;

   -------------------------------------------------------------------------

   procedure New_Line
   --# global
   --#    in out Buffer;
   --#    in out Cur_Y;
   --#       out Cur_X;
   --#       out Screen;
   --# derives
   --#    Cur_X          from   &
   --#    Cur_Y          from * &
   --#    Buffer, Screen from Buffer, Cur_Y;
   --# post
   --#    Cur_X = Console_Width_Range'First;
   is
   begin
      Cur_X := Console_Width_Range'First;
      if Cur_Y = Console_Height_Range'Last then
         Scroll;
      else
         Cur_Y := Cur_Y + 1;
      end if;
   end New_Line;

   -------------------------------------------------------------------------

   procedure Put_Char (Item : Character)
   --# global
   --#    in out Cur_X;
   --#    in out Cur_Y;
   --#    in out Buffer;
   --#       out Screen;
   --# derives
   --#    Cur_X          from *        &
   --#    Cur_Y          from *, Cur_X &
   --#    Buffer, Screen from Buffer, Item, Cur_X, Cur_Y;
   is
   begin
      Buffer (Cur_Y)(Cur_X) := Screen_Cell_Type'
        (Char     => Item,
         FG_Color => White,
         BG_Color => Black);

      if Cur_X = Console_Width_Range'Last then
         New_Line;
      else
         Cur_X := Cur_X + 1;
      end if;

      Update_Screen;
   end Put_Char;

   -------------------------------------------------------------------------

   procedure Put_String (Item : String)
   --# global
   --#    in out Cur_X;
   --#    in out Cur_Y;
   --#    in out Buffer;
   --#       out Screen;
   --# derives
   --#    Cur_X          from *, Cur_Y &
   --#    Cur_Y          from *, Cur_X &
   --#    Buffer, Screen from Buffer, Item, Cur_X, Cur_Y;
   is
      --# hide Put_String;
   begin
      for I in Item'First .. Item'Last
      loop
         Put_Char (Item => Item (I));
      end loop;
   end Put_String;

begin
   Buffer := Screen_Type'
     (others => Screen_Row_Type'
        (others => Screen_Cell_Type'
           (Char     => ' ',
            FG_Color => White,
            BG_Color => Black)));
   Cur_X := Console_Width_Range'First;
   Cur_Y := Console_Height_Range'First;
end SK.Console;
