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

   Console_Width  : constant Natural := 80;
   Console_Height : constant Natural := 25;

   subtype Console_Width_Range  is Natural range 1 .. Console_Width;
   subtype Console_Height_Range is Natural range 1 .. Console_Height;

   type Screen_Row_Type is array (Console_Width_Range) of Screen_Cell_Type;
   --  VGA screen row.

   type Screen_Type is array (Console_Height_Range) of Screen_Row_Type;
   --  VGA screen.

   Cur_X  : Console_Width_Range;
   Cur_Y  : Console_Height_Range;
   Screen : Screen_Type;
   pragma Import (Ada, Screen);
   for Screen'Address use System'To_Address (16#000B_8000#);

   --  Convert given quadword to hex string and store it in specified buffer.
   procedure To_Hex
     (Item   :        Word64;
      Buffer : in out String);

   --  Return character representation of given quadword.
   function To_Character (Value : Word64) return Character;

   --  Scroll screen if current Y position is equal to the last row.
   procedure Scroll;

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

   procedure Put_Line (Item : String)
   is
   begin
      Put_String (Item => Item);
      New_Line;
   end Put_Line;

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

   ---------------------------------------------------------------------------

   procedure Put_Byte (Item : Byte)
   is
      subtype Byte_Range is Positive range 1 .. 2;
      subtype Byte_String is String (Byte_Range);

      Str : Byte_String := Byte_String'(others => '0');
   begin
      To_Hex (Item   => Word64 (Item),
              Buffer => Str);
      Put_String (Item => Str);
   end Put_Byte;

   ---------------------------------------------------------------------------

   procedure Put_Word16 (Item : Word16)
   is
      subtype Word16_Range is Positive range 1 .. 4;
      subtype Word16_String is String (Word16_Range);

      Str : Word16_String := Word16_String'(others => '0');
   begin
      To_Hex (Item   => Word64 (Item),
              Buffer => Str);
      Put_String (Item => Str);
   end Put_Word16;

   ---------------------------------------------------------------------------

   procedure Put_Word32 (Item : Word32)
   is
      subtype Word32_Range is Positive range 1 .. 8;
      subtype Word32_String is String (Word32_Range);

      Str : Word32_String := Word32_String'(others => '0');
   begin
      To_Hex (Item   => Word64 (Item),
              Buffer => Str);
      Put_String (Item => Str);
   end Put_Word32;

   ---------------------------------------------------------------------------

   procedure Put_Word64 (Item : Word64)
   is
      subtype Word64_Range is Positive range 1 .. 16;
      subtype Word64_String is String (Word64_Range);

      Str : Word64_String := Word64_String'(others => '0');
   begin
      To_Hex (Item   => Item,
              Buffer => Str);
      Put_String (Item => Str);
   end Put_Word64;

   ---------------------------------------------------------------------------

   function To_Character (Value : Word64) return Character
   is
      Result : Character;
   begin
      case Value is
         when 16#0#  => Result := '0';
         when 16#1#  => Result := '1';
         when 16#2#  => Result := '2';
         when 16#3#  => Result := '3';
         when 16#4#  => Result := '4';
         when 16#5#  => Result := '5';
         when 16#6#  => Result := '6';
         when 16#7#  => Result := '7';
         when 16#8#  => Result := '8';
         when 16#9#  => Result := '9';
         when 16#a#  => Result := 'a';
         when 16#b#  => Result := 'b';
         when 16#c#  => Result := 'c';
         when 16#d#  => Result := 'd';
         when 16#e#  => Result := 'e';
         when 16#f#  => Result := 'f';
         when others => Result := '?';
      end case;

      return Result;
   end To_Character;

   -------------------------------------------------------------------------

   procedure To_Hex
     (Item   :        Word64;
      Buffer : in out String)
   is
      Temp : Word64;
   begin
      Temp := Item;
      for Pos in reverse Buffer'Range loop
         Buffer (Pos) := To_Character (Temp mod 16);
         Temp         := Temp / 16;
         exit when Temp = 0;
      end loop;
   end To_Hex;

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
