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

   type Word64_Pos is range 0 .. 63;

   --  Test if bit at given position is set.
   function Bit_Test
     (Value : Word64;
      Pos   : Word64_Pos)
      return Boolean
   with
      Global => null;

   --  Set bit at given position.
   function Bit_Set
     (Value : Word64;
      Pos   : Word64_Pos)
      return Word64
   with
      Global => null;

   --  Clear bit at given position.
   function Bit_Clear
     (Value : Word64;
      Pos   : Word64_Pos)
      return Word64
   with
      Global => null;

end SK.Bitops;
