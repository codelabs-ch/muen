--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Sinfo.Utils
is

   -------------------------------------------------------------------------

   function To_Hash (Hex : String) return Musinfo.Hash_Type
   is
      Hash : Musinfo.Hash_Type;
      Idx  : Positive := 3;
   begin
      for B of Hash loop
         B := Interfaces.Unsigned_8'Value
           ("16#" & Hex (Hex'First + Idx .. Hex'First + Idx + 1) & "#");
         Idx := Idx + 2;
      end loop;

      return Hash;
   end To_Hash;

end Sinfo.Utils;
