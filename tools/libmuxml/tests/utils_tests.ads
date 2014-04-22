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

with Ahven.Framework;

package Utils_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Append XML child node.
   procedure Append_Child;

   --  Merge two XML nodes.
   procedure Merge_Nodes;

   --  Try to merge XML nodes with different names.
   procedure Merge_Nodes_Name_Mismatch;

   --  Merge two XML nodes with list child elements.
   procedure Merge_Nodes_With_List;

   --  Get element for given XPath.
   procedure Get_Element;

   --  Get element value for given XPath.
   procedure Get_Element_Value;

   --  Get attribute for given XPath and attribute name.
   procedure Get_Attribute;

   --  Get ancestor node.
   procedure Get_Ancestor_Node;

   --  Remove XML child node.
   procedure Remove_Child;

end Utils_Tests;
