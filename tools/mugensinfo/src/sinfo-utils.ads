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

with DOM.Core.Nodes;

package Sinfo.Utils
is

   --  Calculate lower and upper bounds for node list attribute values
   --  specified by name.
   procedure Get_Bounds
     (Nodes     :     DOM.Core.Node_List;
      Attr_Name :     String;
      Lower     : out Integer;
      Upper     : out Integer)
   with
      Pre => DOM.Core.Nodes.Length (List => Nodes) > 0;

end Sinfo.Utils;
