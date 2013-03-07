package body SK.Console
is

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Initialize;
   end Init;

   -------------------------------------------------------------------------

   --  Return character representation of given quadword.
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

   --  Convert given quadword to hex string and store it in specified buffer.
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

   -------------------------------------------------------------------------

   procedure New_Line
   is
   begin
      Output_New_Line;
   end New_Line;

   -------------------------------------------------------------------------

   procedure Put_Char (Item : Character)
   is
   begin
      Output_Char (Item => Item);
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
   begin
      for I in Item'Range loop
         Put_Char (Item => Item (I));
      end loop;
   end Put_String;

   -------------------------------------------------------------------------

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

   -------------------------------------------------------------------------

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

   -------------------------------------------------------------------------

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

   -------------------------------------------------------------------------

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

end SK.Console;
