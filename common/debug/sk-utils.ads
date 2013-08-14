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

package SK.Utils
is

   --  Convert given quadword to hex string and store it in specified buffer.
   procedure To_Hex
     (Item   :        Word64;
      Buffer : in out String);

   subtype Word64_Hex_Str is String (1 .. 16);

   --  Convert given quadword to hex string.
   function To_Hex (Item : Word64) return Word64_Hex_Str;

   subtype Word16_Hex_Str is String (1 .. 4);

   --  Convert given word to hex string.
   function To_Hex (Item : Word16) return Word16_Hex_Str;

   subtype Word32_Hex_Str is String (1 .. 8);

   --  Convert given doubleword to hex string.
   function To_Hex (Item : Word32) return Word32_Hex_Str;

end SK.Utils;
