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

with DOM.Core.Elements;

package body Mucfgcheck.Utils
is

   -------------------------------------------------------------------------

   function Is_Adjacent_Number
     (Left  : DOM.Core.Node;
      Right : DOM.Core.Node;
      Attr  : String)
      return Boolean
   is
      use Interfaces;

      L_Nr : constant Unsigned_64 := Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => Attr));
      R_Nr : constant Unsigned_64 := Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => Attr));
   begin
      return L_Nr + 1 = R_Nr or R_Nr + 1 = L_Nr;
   end Is_Adjacent_Number;

end Mucfgcheck.Utils;
