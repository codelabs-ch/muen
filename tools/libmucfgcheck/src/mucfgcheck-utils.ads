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

package Mucfgcheck.Utils
is

   --  Returns True if the 'Attr' attributes of the left and right node are
   --  adjacent numbers.
   function Is_Adjacent_Number
     (Left  : DOM.Core.Node;
      Right : DOM.Core.Node;
      Attr  : String)
      return Boolean;

   --  Returns True if the left and right memory regions are adjacent. The
   --  attribute given by Addr_Attr is treated as region address, and the size
   --  is expected in an attribute of the same name.
   function Is_Adjacent_Region
     (Left      : DOM.Core.Node;
      Right     : DOM.Core.Node;
      Addr_Attr : String)
      return Boolean;

end Mucfgcheck.Utils;
