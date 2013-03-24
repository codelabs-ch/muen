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

with Ada.Streams;
with Sax.Encodings;
with Unicode.Encodings;

package DOM.Core.Nodes is

   ----------
   -- Node --
   ----------

   function Node_Name (N : Node) return DOM_String;
   --  Return the name of the tag.
   --  Its meaning depends on the type of the node, see the DOM specifications.
   --  For an <element> node, this returns the qualified name, in the form of
   --  "prefix:local_name". See below for subprograms to get each of the
   --  components separately instead.

   function Node_Value (N : Node) return DOM_String;
   --  Return the value of the node.
   --  Its meaning depends on the type of the node, see the DOM specifications.

   function Node_Type (N : Node) return Node_Types;
   --  Return the type of the node

   procedure Set_Node_Value (N : Node; Value : DOM_String);
   --  Change the value of the node.
   --  No_Modification_Allowed_Err is raised when Node is read-only.

   function Child_Nodes (N : Node) return Node_List;
   --  Return the list of children for this node. This could be empty depending
   --  on the type of the node.

   function First_Child (N : Node) return Node;
   --  Return the first child of N.

   function Last_Child (N : Node) return Node;
   --  Return the last child of N.

   function Parent_Node (N : Node) return Node;
   --  Return the parent for this node. Note that for attribute nodes, this
   --  is always null

   function Previous_Sibling (N : Node) return Node;
   --  Return the node preceding N in their common parent.
   --  null is returned if there is no such node.
   --  Note that it is much more efficient to get the list of all children
   --  from the parent and use Item on the list to get each one of them.

   function Next_Sibling (N : Node) return Node;
   --  Return the node following N in their common parent.
   --  null is returned if there is no such node.

   function Attributes (N : Node) return Named_Node_Map;
   --  Return the list of attributes for N.
   --  null is returned, except for Element nodes.

   function Owner_Document (N : Node) return Node;
   --  Return the document to which N belongs.

   function Namespace_URI (N : Node) return DOM_String;
   --  Return the URI associated with N.
   --  This is the URI used when the node was created, and is independent from
   --  the prefix (see below).

   function Prefix (N : Node) return DOM_String;
   --  Return the prefix associated with N (first part of the qualified name)

   procedure Set_Prefix (N : Node; Prefix : DOM_String);
   --  Changing this prefix will affect the qualified name.
   --  Note: In the Ada implementation, this only works when N belongs to
   --  a tree already, it doesn't work for isolated nodes.

   function Local_Name (N : Node) return DOM_String;
   function Local_Name (N : Node) return Sax.Symbols.Symbol;
   --  Return the local name of N (second part of the qualified name). This is
   --  null if the node was created with a DOM level 1 method (no namespace at
   --  creation time).

   function Insert_Before
     (N         : Node;
      New_Child : Node;
      Ref_Child : Node := null) return Node;
   --  Insert New_Child just before Ref_Child in the list of children for N.
   --  If Ref_Child is null, New_Child is inserted at the end.
   --  If New_Child is a document_fragment, all of its children are inserted.
   --  If New_Child is already in the tree, it is first removed.
   --  raises:
   --    * Hierarchy_Request_Err: N doesn't allow a child of this type, or
   --      New_Child is already an ancestor of N.
   --    * Wrong_Document_Err: if New_Child was created from another document
   --    * No_Modification_Allowed_Err: N or New_Child is read-only.
   --    * Not_Found_Err: Ref_Child is not a child of N.

   function Replace_Child
     (N         : Node;
      New_Child : Node;
      Old_Child : Node) return Node;
   --  Replace Old_Child with New_Child in the list of children of N.
   --  If New_Child is a document fragment, all its children are inserted in
   --  place of Old_Child. Returns the replaced node.
   --  raises:
   --    * Hierarchy_Request_Err: N doesn't allow a child of this type, or
   --      New_Child is already an ancestor of N.
   --    * Wrong_Document_Err: if New_Child was created from another document
   --    * No_Modification_Allowed_Err: N or New_Child is read-only.
   --    * Not_Found_Err: Old_Child is not a child of N.
   --  The caller must free the returned node.

   function Remove_Child
     (N         : Node;
      Old_Child : Node) return Node;
   --  Remove Old_Child from the list of children of N, and return it.
   --  raises:
   --    * No_Modification_Allowed_Err: N is read-only
   --    * Not_Found_Err: Old_Child is not a child of N
   --  The caller must free the returned node.

   function Append_Child
     (N         : Node;
      New_Child : Node) return Node;
   --  Append New_Child at the end of the list of children of N, and return
   --  the added node.
   --  raises:
   --    * Hierarchy_Request_Err: N doesn't allow a child of this type, or
   --      New_Child is already an ancestor of N.
   --    * Wrong_Document_Err: if New_Child was created from another document
   --    * No_Modification_Allowed_Err: N or New_Child is read-only.

   function Has_Child_Nodes (N : Node) return Boolean;
   --  True if N has any children, False otherwise

   function Clone_Node (N : Node; Deep : Boolean) return Node;
   --  Returns a duplicate of N.
   --  The duplicate node has no parent.

   procedure Normalize (N : Node);
   --  Make sure there are no adjacent text nodes in the children of N.
   --  This processes the full-depth of the sub-tree underneath N.

   function Supports
     (N : Node;
      Feature : DOM_String;
      Version : DOM_String) return Boolean;
   --  Test whether the DOM implementation implements a specific feature, and
   --  that feature is supported by N.

   ---------------
   -- Node_List --
   ---------------

   function Item (List : Node_List; Index : Natural) return Node;
   --  Return index-nth element in the list (starting from 0)
   --  If Index is greated than or equal to the number of items in the list,
   --  null is returned.

   function Length (List : Node_List) return Natural;
   --  Return the number of elements in the list.

   --------------------
   -- Named_Node_Map --
   --------------------

   function Get_Named_Item
     (Map : Named_Node_Map; Name : DOM_String) return Node;
   function Get_Named_Item
     (Map : Named_Node_Map; Name : Sax.Symbols.Symbol) return Node;
   --  Retrieve a node specified by name.
   --  null is returned if no such node exists
   --  Consider using Get_Named_Item_NS instead for DOM level 2

   procedure Set_Named_Item
     (Map : in out Named_Node_Map; Arg : Node; Replaces : out Node);
   procedure Set_Named_Item (Map : in out Named_Node_Map; Arg : Node);
   --  Add a node using its Node_Name attribute. Note that you can not have
   --  multiple instances of nodes with special names (#Document, ...).
   --  It returns the node that Arg replaces in Map, or null if it didn't
   --  replace anything.
   --  Consider using Set_Named_Item_NS instead for DOM level 2

   procedure Remove_Named_Item
     (Map : in out Named_Node_Map; Name : DOM_String; Removed : out Node);
   procedure Remove_Named_Item
     (Map : in out Named_Node_Map; Name : DOM_String);
   --  Remove a node from Map, and returns it.
   --  Consider using Remove_Named_Item_NS instead for DOM level 2

   procedure Remove_Named_Item (Map : in out Named_Node_Map; N : Node);
   --  Remove a specific node from the map

   function Item
     (Map : Named_Node_Map; Index : Natural) return Node;
   --  Return the Index-nth node in the list (starting from 0)

   function Length (Map : Named_Node_Map) return Natural;
   --  Return the number of elements in the map.

   function Get_Named_Item_NS
     (Map           : Named_Node_Map;
      Namespace_URI : DOM_String;
      Local_Name    : DOM_String) return Node;
   function Get_Named_Item_NS
     (Map           : Named_Node_Map;
      Namespace_URI : Sax.Symbols.Symbol;
      Local_Name    : Sax.Symbols.Symbol) return Node;
   --  Retrieve a node specified by its (namespace, local_name)

   procedure Set_Named_Item_NS
     (Map : in out Named_Node_Map; Arg : Node; Replaces : out Node);
   procedure Set_Named_Item_NS
     (Map : in out Named_Node_Map; Arg : Node);
   --  Add a node using its namespace and local_name.
   --  It returns the node that Arg replaces (or null if none)

   procedure Remove_Named_Item_NS
     (Map           : in out Named_Node_Map;
      Namespace_URI : DOM_String;
      Local_Name    : DOM_String;
      Removed       : out Node);
   procedure Remove_Named_Item_NS
     (Map           : in out Named_Node_Map;
      Namespace_URI : DOM_String;
      Local_Name    : DOM_String);
   --  Remove a node specified by its namespace and local_name.

   ------------------
   -- Input/Output --
   ------------------

   procedure Write
     (Stream                : access Ada.Streams.Root_Stream_Type'Class;
      N                     : Node;
      Print_Comments        : Boolean := True;
      Print_XML_Declaration : Boolean := True;
      With_URI              : Boolean := False;
      Pretty_Print          : Boolean := False;
      EOL_Sequence          : String  := "" & ASCII.LF;
      Encoding              : Unicode.Encodings.Unicode_Encoding :=
        Unicode.Encodings.Get_By_Name ("utf-8");
      Collapse_Empty_Nodes  : Boolean := True);
   --  Print the contents of Node and its children in XML format.
   --  If Print_Comments is True, then nodes associated with comments are
   --  also displayed.
   --  EOL_Sequence is output at every end of line. It should be encoded in
   --  Sax.Encodings.Encoding, and will be automatically converted to the
   --  appropriate output encoding.
   --  Encoding specifies the encoding to use in the output stream.
   --
   --  The <?xml?> declaration is displayed only if Print_XML_Declaration and
   --  N is a Document_Node. In this case, a Byte-Order mark is also output
   --  so that proper decoding of the document can be performed later on.
   --  Note that you mustn't added <?xml?> yourself to the DOM tree. The XML
   --  standard doesn't define this as a processing instruction, which is why
   --  it has a different name ("XML declaration") and cannot be modified by
   --  users.
   --
   --  By default, names are of the form  ns_prefix:local_name. However, if
   --  with_URI is True, names will be  ns_URI:local_name instead
   --
   --  If Collapse_Empty_Nodes is true, then nodes with no child node will be
   --  output as <name/>, instead of <name></name>
   --
   --  If Pretty_Print is true, then the XML nodes will be indented so that
   --  children nodes are to the right of their parents. It is set to False
   --  by default because its use changes the document (addition or removal
   --  of whitespaces among other things), which in general has no effect for
   --  automatic tools reading the document. All whitespaces are modified
   --  outside of elements containing nothing but text nodes. For text nodes,
   --  leading and trailing whitespaces are also deleted

   -----------------------
   -- Extra subprograms --
   -----------------------
   --  The following subprograms are not part of the standard DOM interface.
   --  However, they are needed for a full usage of this DOM implementation.
   --  The output of any of these subprograms is likely to change from one
   --  version of XML/Ada to the next, do not rely on it.

   procedure Print
     (N              : Node;
      Print_Comments : Boolean := False;
      Print_XML_PI   : Boolean := False;
      With_URI       : Boolean := False;
      EOL_Sequence   : String  := Sax.Encodings.Lf_Sequence;
      Encoding       : Unicode.Encodings.Unicode_Encoding :=
        Unicode.Encodings.Get_By_Name ("utf-8");
      Collapse_Empty_Nodes : Boolean := False);
   --  For debugging purposes only!
   --
   --  Same as Write, but the output is done on Stdout.
   --  Warning: the default values for the parameters are not the same as for
   --  write. For the latter, they are chosen so that by default the output is
   --  valid XML, whereas Print is mostly intended to be used for testsuite
   --  purposes, and the default match that goal.

   procedure Dump (N : Node; With_URI : Boolean := False);
   --  Dump the contents of the node to standard output.
   --  This output shows the structure of the tree instead of a valid XML file.

   procedure Free (N : in out Node; Deep : Boolean := True);
   --  This procedure is not part of the DOM standard, but is required to
   --  free the memory used by a node.
   --  Beware that a node is not removed from its parent.
   --  If Deep is True, then the children are also removed

end DOM.Core.Nodes;
