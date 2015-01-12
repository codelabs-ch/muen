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

with DOM.Core;

package Mutools.Match
is

   --  Returns True if the left node's 'physical' attribute matches the 'name'
   --  attribute of the right node.
   function Is_Valid_Reference (Left, Right : DOM.Core.Node) return Boolean;

   --  Returns True if the left node's parent 'physical' attribute matches the
   --  'name' attribute of the right node.
   function Is_Valid_Reference_Lparent
     (Left_Child, Right : DOM.Core.Node)
      return Boolean;

end Mutools.Match;
