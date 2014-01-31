--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

package Mutools.Utils
is

   type Unsigned_64_Pos is range 0 .. 63;

   --  Test if bit at given position is set.
   function Bit_Test
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Boolean;

   --  Set bit at given position.
   function Bit_Set
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Interfaces.Unsigned_64;

   --  Clear bit at given position.
   function Bit_Clear
     (Value : Interfaces.Unsigned_64;
      Pos   : Unsigned_64_Pos)
      return Interfaces.Unsigned_64;

   --  Return hexadecimal representation of given number. If prefix is True,
   --  the returned string includes the base (16#..#).
   function To_Hex
     (Number : Interfaces.Unsigned_64;
      Prefix : Boolean := True)
      return String;

   --  Extract entity name from given encoded string (e.g. 'linux|zp' or
   --  'kernel_0|vmxon').
   function Decode_Entity_Name (Encoded_Str : String) return String;

end Mutools.Utils;
