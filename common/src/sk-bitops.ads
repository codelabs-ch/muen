--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package SK.Bitops
is

   --  Needs to be a subtype of Word64 for now to make proof work (Q418-004).
   subtype Word64_Pos is Word64     range 0 .. 63;
   subtype Word32_Pos is Word64_Pos range 0 .. 31;

   function Power_Of_2 (Pos : Word64_Pos) return Word64
   is (Interfaces.Shift_Left
       (Value  => Interfaces.Unsigned_64'(1),
        Amount => Natural (Pos)));

   --  Test if bit at given position is set.
   function Bit_Test
     (Value : Word64;
      Pos   : Word64_Pos)
      return Boolean
   is
     ((Value and Power_Of_2 (Pos)) /= 0)
   with
      Post => Bit_Test'Result = ((Value and Power_Of_2 (Pos)) /= 0);

   --  Set bit at given position.
   function Bit_Set
     (Value : Word64;
      Pos   : Word64_Pos)
      return Word64
   is
     (Value or Power_Of_2 (Pos))
   with
      Post => (for all I in Word64_Pos =>
                 (Bit_Test (Value => Bit_Set'Result, Pos => I) =
                  (if I = Pos then True else
                   Bit_Test (Value => Value, Pos => I))));

   --  Clear bit at given position.
   function Bit_Clear
     (Value : Word64;
      Pos   : Word64_Pos)
      return Word64
   is
     (Value and not Power_Of_2 (Pos))
   with
      Post => (for all I in Word64_Pos =>
                 (Bit_Test (Value => Bit_Clear'Result, Pos => I) =
                  (if I = Pos then False else
                   Bit_Test (Value => Value, Pos => I))));

   --  Find highest bit set in given bitfield within the specified search
   --  range. If no bit is set, return False.
   generic
      type Search_Range is new Word64_Pos;
   procedure Find_Highest_Bit_Set
     (Field :     Word64;
      Found : out Boolean;
      Pos   : out Search_Range);

end SK.Bitops;
