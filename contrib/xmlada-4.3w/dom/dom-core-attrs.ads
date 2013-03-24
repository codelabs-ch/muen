------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with DOM.Core.Nodes; use DOM.Core.Nodes;

package DOM.Core.Attrs is
   function Name (Att : Attr) return DOM_String
      renames DOM.Core.Nodes.Node_Name;
   --  Return the name of the node

   function Specified (Att : Attr) return Boolean;
   --  Return True if the attribute was set explicitly in the XML file,
   --  or False if this is the default value for the node

   function Value (Att : Attr) return DOM_String
      renames DOM.Core.Nodes.Node_Value;
   --  Return the value of the attribute

   procedure Set_Value (Att : Attr; Value : DOM_String)
      renames DOM.Core.Nodes.Set_Node_Value;
   procedure Set_Value (Att : Attr; Value : Sax.Symbols.Symbol);
   --  Set the value of the attribute

   function Owner_Element (Att : Attr) return Element;
   --  return the node to which Att belongs

   function Is_Id (Att : Attr) return Boolean;
   --   Returns whether this attribute is known to be of type ID (i.e. to
   --   contain an identifier for its owner element)
   --
   --  Introduced in DOM 3.0

end DOM.Core.Attrs;
