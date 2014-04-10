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

package Muxml.Utils
is

   --  Searches the element specified by an XPath in the given document and
   --  returns the attribute given by name as string. If no such attribute or
   --  element exists, an empty string is returned.
   function Get_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String)
      return String;

   --  Searches the element specified by an XPath in the given document and
   --  returns its value as string. If no such element exists, an empty string
   --  is returned.
   function Get_Element_Value
     (Doc   : DOM.Core.Node;
      XPath : String)
      return String;

   --  Append all nodes of 'Right' to specified node list 'Left'.
   procedure Append
     (Left  : in out DOM.Core.Node_List;
      Right :        DOM.Core.Node_List);

   --  Append new child node to given node.
   procedure Append_Child
     (Node      : DOM.Core.Node;
      New_Child : DOM.Core.Node);

   --  Merge the right node incl. all its children into the left node. Values
   --  provided by the right node take precedence and replace existing data in
   --  the left node tree. Nothing is done if left and right do not have
   --  matching names. Child nodes matching the list tag are appended instead
   --  of merged into a single element.
   procedure Merge
     (Left     : DOM.Core.Node;
      Right    : DOM.Core.Node;
      List_Tag : String := "");

end Muxml.Utils;
