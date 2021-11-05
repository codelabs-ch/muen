--
--  Copyright (C) 2022 secunet Security Networks AG
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

with Muxml;
with DOM.Core;

package Mutools.Amend
is

   -- Expands all amend-nodes.
   -- For each <amend>-tag it does the following:
   --   Check that the given XPath matches exactly one node "Parent"
   --   For each node C in the body of amend:
   --     Recursive_Merge(Parent,  C)
   --   delete <amend>-nodes
   procedure Expand
      (XML_Data : Muxml.XML_Data_Type);

private

   -- merge New_Child into Parent such that parts of the child-tree
   -- that already exist in Parent are not duplicated
   -- In detail: for each child C of Parent:
   --   check if tag name and all attributes of C and New_Child are equal
   --     if yes: recursive function call on C and on each Child of New_Child
   --     if no: goto next child C of Parent
   -- if no child matched:
   --     append New_Child to children of Parent and return (with deep cloning)
   procedure Recursive_Merge
      (Parent    : DOM.Core.Node;
       New_Child : DOM.Core.Node);

   -- check if L and R have the same name and the same attributes
   --   (including values of attributes)
   function Nodes_Equal
      (L, R : DOM.Core.Node)
      return Boolean;

end Mutools.Amend;
