--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with DOM.Core.Elements;

package body Mutools.Match
is

   -------------------------------------------------------------------------

   function Is_Valid_Reference (Left, Right : DOM.Core.Node) return Boolean
   is
      Ref_Name : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "physical");
      Phy_Name : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "name");
   begin
      return Ref_Name'Length > 0
        and then Phy_Name'Length > 0
        and then Ref_Name = Phy_Name;
   end Is_Valid_Reference;

   -------------------------------------------------------------------------

   function Is_Valid_Reference_Lparent
     (Left_Child, Right : DOM.Core.Node)
      return Boolean
   is
   begin
      return Is_Valid_Reference
        (Left  => DOM.Core.Nodes.Parent_Node (N => Left_Child),
         Right => Right);
   end Is_Valid_Reference_Lparent;

   -------------------------------------------------------------------------

   function Is_Valid_Resource_Ref (Left, Right : DOM.Core.Node) return Boolean
   is
   begin
      return Is_Valid_Reference
        (Left  => Left,
         Right => Right)
        and then Is_Valid_Reference
          (Left  => DOM.Core.Nodes.Parent_Node (N => Left),
           Right => DOM.Core.Nodes.Parent_Node (N => Right));
   end Is_Valid_Resource_Ref;

end Mutools.Match;
