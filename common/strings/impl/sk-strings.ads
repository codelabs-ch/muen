--
--  Copyright (C) 2013, 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

--D @Interface
--D Utility package providing helper functions for converting numeric values to
--D strings.
--D Note: implementation is only present in debug builds. In release versions
--D this package is empty.
package SK.Strings
is

   subtype Word64_Hex_Str_Nobase is String (1 .. 16);

   --  Convert given quadword to hex string and store it in specified buffer.
   procedure Img
     (Item   :        Word64;
      Buffer : in out Word64_Hex_Str_Nobase);

   subtype Byte_Hex_Str is String (1 .. 6);

   --  Convert given byte to hex string.
   function Img (Item : Byte) return Byte_Hex_Str;

   subtype Word16_Hex_Str is String (1 .. 8);

   --  Convert given word to hex string.
   function Img (Item : Word16) return Word16_Hex_Str;

   subtype Word32_Hex_Str is String (1 .. 12);

   --  Convert given doubleword to hex string.
   function Img (Item : Word32) return Word32_Hex_Str;

   subtype Word64_Hex_Str is String (1 .. 20);

   --  Convert given quadword to hex string.
   function Img (Item : Word64) return Word64_Hex_Str;

   subtype Byte_Hex_Str_Nobase is String (1 .. 2);

   --  Convert given byte to hex string without base prefix/suffix.
   function Img_Nobase (Item : Byte) return Byte_Hex_Str_Nobase;

   subtype Word64_Dec_Str is String (1 .. 20);

   --  Convert given quadword to decimal string.
   function Img_Dec (Item : Word64) return Word64_Dec_Str;

end SK.Strings;
