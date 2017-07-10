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

package body SK.Bitops
is

   -------------------------------------------------------------------------

   procedure Find_Highest_Bit_Set
     (Field :     Word64;
      Found : out Boolean;
      Pos   : out Search_Range)
   is
   begin
      Pos   := 0;
      Found := Field /= 0;

      if Found then
         for I in reverse Search_Range loop
            if Bit_Test (Value => Field,
                         Pos   => Word64_Pos (I))
            then
               Pos := I;
               return;
            end if;
         end loop;
      end if;
   end Find_Highest_Bit_Set;

end SK.Bitops;
