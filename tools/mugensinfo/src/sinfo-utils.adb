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

package body Sinfo.Utils
is

   -------------------------------------------------------------------------

   procedure Get_Bounds
     (Nodes     :     DOM.Core.Node_List;
      Attr_Name :     String;
      Lower     : out Integer;
      Upper     : out Integer)
   is
   begin
      Lower := Integer'Last;
      Upper := Integer'First;

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Value : constant Integer
              := Integer'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => Attr_Name));
         begin
            if Value > Upper then
               Upper := Value;
            end if;
            if Value < Lower then
               Lower := Value;
            end if;
         end;
      end loop;
   end Get_Bounds;

end Sinfo.Utils;
