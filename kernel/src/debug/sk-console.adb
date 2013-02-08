with System;

package body SK.Console
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
   Screen : Screen_Type;
   pragma Import (Ada, Screen);
   for Screen'Address use System'To_Address (16#000B_8000#);

   -------------------------------------------------------------------------

   procedure Clear
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
   end Clear;

   -------------------------------------------------------------------------

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

   procedure New_Line
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
   is
   begin
      Screen (Cur_Y)(Cur_X) := Screen_Cell_Type'
        (Char     => Item,
         FG_Color => White,
         BG_Color => Black);

      if Cur_X = Console_Width_Range'Last then
         New_Line;
      else
         Cur_X := Cur_X + 1;
      end if;
   end Put_Char;

   -------------------------------------------------------------------------

   procedure Put_String (Item : String)
   is
      Max_String_Length : constant := Console_Width * Console_Height;

      subtype String_Range is Positive range 1 .. Max_String_Length;
   begin
      for I in String_Range
      loop
         exit when I > Item'Length;
         Put_Char (Item => Item (I));
      end loop;
   end Put_String;

begin
   Screen := Screen_Type'
     (others => Screen_Row_Type'
        (others => Screen_Cell_Type'
           (Char     => ' ',
            FG_Color => White,
            BG_Color => Black)));
   Cur_X := Console_Width_Range'First;
   Cur_Y := Console_Height_Range'First;
end SK.Console;
