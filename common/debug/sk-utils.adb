package body SK.Utils
is

   --  Return character representation of given quadword.
   function To_Character (Value : Word64) return Character;

   -------------------------------------------------------------------------

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

   -------------------------------------------------------------------------

   function To_Hex (Item : Word16) return Word16_Hex_Str
   is
      Buffer : Word64_Hex_Str := (others => '0');
   begin
      To_Hex (Item   => Word64 (Item),
              Buffer => Buffer);
      return Buffer (13 .. 16);
   end To_Hex;

   -------------------------------------------------------------------------

   function To_Hex (Item : Word32) return Word32_Hex_Str
   is
      Buffer : Word64_Hex_Str := (others => '0');
   begin
      To_Hex (Item   => Word64 (Item),
              Buffer => Buffer);
      return Buffer (9 .. 16);
   end To_Hex;

   -------------------------------------------------------------------------

   function To_Hex (Item : Word64) return Word64_Hex_Str
   is
      Buffer : Word64_Hex_Str := (others => '0');
   begin
      To_Hex (Item   => Item,
              Buffer => Buffer);
      return Buffer;
   end To_Hex;

end SK.Utils;
