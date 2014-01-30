--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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

package body Mutools.Utils
is

   use type Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   function Bit_Clear
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Interfaces.Unsigned_64
   is
   begin
      return Value and not (2 ** Natural (Pos));
   end Bit_Clear;

   -------------------------------------------------------------------------

   function Bit_Set
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Interfaces.Unsigned_64
   is
   begin
      return (Value or 2 ** Natural (Pos));
   end Bit_Set;

   -------------------------------------------------------------------------

   function Bit_Test
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Boolean
   is
   begin
      return (Value and 2 ** Natural (Pos)) /= 0;
   end Bit_Test;

   -------------------------------------------------------------------------

   function Decode_Entity_Name (Encoded_Str : String) return String
   is
      Udrl_Idx : constant Natural := Ada.Strings.Fixed.Index
        (Source  => Encoded_Str,
         Pattern => "|");
   begin
      return Encoded_Str (Encoded_Str'First .. Udrl_Idx - 1);
   end Decode_Entity_Name;

   -------------------------------------------------------------------------

   function To_Hex
     (Number    : Interfaces.Unsigned_64;
      Prefix    : Boolean := True;
      Normalize : Boolean := False)
      return String
   is
      Num_Str : String (1 .. 23);
      Pos     : Natural := Num_Str'Last;
      Tmp     : Interfaces.Unsigned_64 := Number;
      Digit   : Natural := 0;

      function To_Hex_Digit (N : Interfaces.Unsigned_64) return Character
         with pre => N <= 16#F#;

      function To_Hex_Digit (N : Interfaces.Unsigned_64) return Character
      is
         Internal_Error : exception;
      begin
         case N is
            when 16#0# => return '0';
            when 16#1# => return '1';
            when 16#2# => return '2';
            when 16#3# => return '3';
            when 16#4# => return '4';
            when 16#5# => return '5';
            when 16#6# => return '6';
            when 16#7# => return '7';
            when 16#8# => return '8';
            when 16#9# => return '9';
            when 16#A# => return 'A';
            when 16#B# => return 'B';
            when 16#C# => return 'C';
            when 16#D# => return 'D';
            when 16#E# => return 'E';
            when 16#F# => return 'F';
            when others => raise Internal_Error;
         end case;
      end To_Hex_Digit;
   begin
      if Normalize or Prefix then
         Num_Str (Pos) := '#';
         Pos := Pos - 1;
      end if;

      loop
         Num_Str (Pos) := To_Hex_Digit (Tmp mod 16);
         Tmp   := Tmp / 16;
         Pos   := Pos - 1;
         Digit := Digit + 1;
         exit when Tmp = 0 and (not Normalize or Digit mod 4 = 0);

         if Normalize and Digit mod 4 = 0 then
            Num_Str (Pos) := '_';
            Pos := Pos - 1;
         end if;
      end loop;

      if Normalize or Prefix then
         Num_Str (Pos - 2 .. Pos) := "16#";
         Pos := Pos - 3;
      end if;

      return Num_Str (Pos + 1 .. Num_Str'Last);

   end To_Hex;

end Mutools.Utils;
