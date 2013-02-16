with System;

package body SK.Console.Backend
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

   Console_Width  : constant Natural := 80;
   Console_Height : constant Natural := 25;

   subtype Position_Type is Natural range 1 .. Console_Width * Console_Height;
   subtype Console_Width_Range  is Natural range 1 .. Console_Width;
   subtype Console_Height_Range is Natural range 1 .. Console_Height;

   --  VGA screen row.
   type Screen_Row_Type is array (Console_Width_Range) of Screen_Cell_Type;

   --  VGA screen.
   type Screen_Type is array (Console_Height_Range) of Screen_Row_Type;

   Cur_X  : Console_Width_Range;
   Cur_Y  : Console_Height_Range;
   Screen : Screen_Type;
   pragma Import (Ada, Screen);
   for Screen'Address use System'To_Address (16#000B_8000#);

   -------------------------------------------------------------------------

   --  Scroll screen if current Y position is equal to the last row.
   procedure Scroll
   is
      subtype Console_To_Last_Row is Console_Height_Range range
        Console_Height_Range'First .. Console_Height_Range'Last - 1;
   begin
      for Y in Console_To_Last_Row
      loop
         Screen (Y) := Screen (Y + 1);
      end loop;

      Screen (Console_Height_Range'Last) := Screen_Row_Type'
        (others => Screen_Cell_Type'
           (Char     => ' ',
            FG_Color => White,
            BG_Color => Black));
   end Scroll;

   -------------------------------------------------------------------------

   --  Update cursor position.
   procedure Update_Cursor
   is
      Pos : Position_Type;
   begin
      Pos := (Cur_Y - 1) * Console_Width  + Cur_X - 1;

      --  Set high cursor byte

      Outb (Port  => 16#3D4#,
            Value => 14);
      Outb (Port  => 16#3D5#,
            Value => Byte (Pos / 2 ** 8));

      --  Set low cursor byte

      Outb (Port  => 16#3D4#,
            Value => 15);
      Outb (Port  => 16#3D5#,
            Value => Byte (Pos));
   end Update_Cursor;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Screen := Screen_Type'
        (others => Screen_Row_Type'
           (others => Screen_Cell_Type'
              (Char     => ' ',
               FG_Color => White,
               BG_Color => Black)));

      Cur_X := Console_Width_Range'First;
      Cur_Y := Console_Height_Range'First;
      Update_Cursor;
   end Init;

   -------------------------------------------------------------------------

   procedure New_Line
   is
   begin
      Cur_X := Console_Width_Range'First;
      if Cur_Y = Console_Height_Range'Last then
         Scroll;
      else
         Cur_Y := Cur_Y + 1;
      end if;
      Update_Cursor;
   end New_Line;

   -------------------------------------------------------------------------

   procedure Put_Char (Item : Character)
   is
   begin
      Screen (Cur_Y) (Cur_X) := Screen_Cell_Type'
        (Char     => Item,
         FG_Color => White,
         BG_Color => Black);

      if Cur_X = Console_Width_Range'Last then
         New_Line;
      else
         Cur_X := Cur_X + 1;
      end if;
      Update_Cursor;
   end Put_Char;

end SK.Console.Backend;
