--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

package body SK.Utils
with
   SPARK_Mode => Off
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
