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

--  This package is the root hierarchy for the Core part of the DOM
--  interface.
--  It is in fact made of several subpackages, since DOM provides
--  two views of the tree: Object-oriented throught the Element, Document,...
--  types; and direct access through the Node interface.

pragma Ada_05;

with Unicode.CES;
with Ada.Unchecked_Deallocation;
with Sax.HTable;
with Sax.Symbols;
with Sax.Utils;

package DOM.Core is

   Default_Node_List_Growth_Factor : constant Float := 1.0;
   --  Set to 1.0 the buffer is doubled in size (growth factor is 100%).
   --  If set to 0.0 only a single empty items is added.
   --  The higher this factor, the less memory allocations will be required
   --  (and thus the faster your program will run).
   --  Setting this to 0.0 will require more allocations, but will save memory,
   --  since no empty node will remain in the final tree.

   subtype DOM_String is Unicode.CES.Byte_Sequence;
   --  A simple redefinition of the strings, to be compatible with the
   --  standard DOM interface
   --  See the package Encodings for the exact encoding used for DOM_Strings

   subtype DOM_String_Access is Unicode.CES.Byte_Sequence_Access;

   -----------
   -- Nodes --
   -----------
   --  This is the base type for all DOM.Core types. It is declared in this
   --  package for visibility reasons, so that all DOM.Core.* packages have
   --  access to the components.

   type Node_Types is
     (Element_Node,
      Attribute_Node,
      Cdata_Section_Node,
      Entity_Reference_Node,
      Entity_Node,
      Processing_Instruction_Node,
      Text_Node,
      Comment_Node,
      Document_Node,
      Document_Type_Node,
      Document_Fragment_Node,
      Notation_Node);

   subtype Character_Data_Types is Node_Types range Text_Node .. Comment_Node;

   type Node_Record (Node_Type : Node_Types) is private;
   type Node is access Node_Record;

   subtype Character_Data is Node;
   subtype Element is Node (Element_Node);
   subtype Attr is Node (Attribute_Node);
   subtype Cdata_Section is Character_Data (Cdata_Section_Node);
   subtype Entity_Reference is Node (Entity_Reference_Node);
   subtype Entity is Node (Entity_Node);
   subtype Processing_Instruction is Node (Processing_Instruction_Node);
   subtype Text is Character_Data (Text_Node);
   subtype Comment is Character_Data (Comment_Node);
   subtype Document is Node (Document_Node);
   subtype Document_Type is Node (Document_Type_Node);
   subtype Document_Fragment is Node (Document_Fragment_Node);
   subtype Notation is Node (Notation_Node);

   type Node_List is private;
   --  A simple ordered list of nodes (see DOM.Core.Nodes for subprograms)

   type Named_Node_Map is private;
   --  A collection of nodes accessible by their names.
   --  This is unordered.

   procedure Free (List : in out Node_List);
   --  Free the memory occupied by the list. The items contained in the list
   --  are not freed, since they still exist in the XML tree.

   ------------------------
   -- Dom implementation --
   ------------------------
   --  This provides a number of methods for performing operations that are
   --  independent of any particular instance of the document object model.

   type DOM_Implementation is private;
   --  There are multiple implementations of DOM.
   --  They can be specialized for some special cases (HTML, Stylesheets,...)

   function Has_Feature
     (Implementation : DOM_Implementation;
      Feature        : DOM_String;
      Version        : String := "2.0") return Boolean;
   --  Return TRUE if this implementation of DOM has the Feature.

   function Create_Document
     (Implementation : DOM_Implementation;
      NameSpace_URI  : DOM_String := "";
      Qualified_Name : DOM_String := "";
      Doc_Type       : Node := null;
      Symbols        : Sax.Utils.Symbol_Table := Sax.Utils.No_Symbol_Table)
      return Node;
   --  Create an new document with its element.
   --  Note that NameSpace_URI can be the empty string if you do not want
   --  to use namespaces.
   --  The Document Type Definition can be null if there is none associated
   --  with the document.
   --  Wrong_Document_Err is raised if Doc_Type has already been used for
   --  another document.
   --  Symbols should be used to specify the symbol table used by the parser
   --  that generates the DOM. It is needed because the various string elements
   --  in the tree are represented as symbols and the correct symbol table must
   --  be specified. You can get it from the parser itself by using
   --  Get_Symbol_Table. Optionally, you can pass an explicit No_Symbol_Table
   --  to create one automatically. It is recommended to share the table with
   --  the parser whenever possible for maximum efficient.
   --  In general, the document is created from the Start_Document callback
   --  of a tree_reader, so the simplest is to call the inherited
   --  Start_Document.

   procedure Set_Node_List_Growth_Factor (Factor : Float);
   --  Set the growth factor, see Default_Node_List_Growth_Factor

   --------------------
   -- Dom exceptions --
   --------------------
   --  The following exceptions are declared in the DOM interface. If we
   --  were to follow exactly the interface, we should a single exception to
   --  which we associate an integer code. It seems easier to provide one
   --  exception for each case. However, we kept the standard names.

   Index_Size_Err : exception;
   --  If Index or size is invalid (negative or greated than max value).

   Domstring_Size_Err : exception;
   --  If the specified range of text does not fit into a DomString.

   Hierarchy_Request_Err : exception;
   --  If a node is inserted somewhere it doesn't belong.

   Wrong_Document_Err : exception;
   --  If a node is used with a document other than its own.

   Invalid_Character_Err : exception;
   --  If an invalid character is used, for instance in a name.

   No_Data_Allowed_Err : exception;
   --  If data is specified for a node that doesn't support data.

   No_Modification_Allowed_Err : exception;
   --  If an attempt is made to modify a read-only object.

   Not_Found_Err : exception;
   --  If an attempt is made to reference a node in a concept where it doesn't
   --  exist.

   Not_Supported_Err : exception;
   --  If the implementation does not support the type of object requested.

   Inuse_Attribute_Err : exception;
   --  If an attempt is made to add an attribute that is already used.

   Invalid_State_Err : exception;
   --  If an attempt is made to use an object that is not or no longer
   --  available.

   Syntax_Err : exception;
   --  If an invalid string is specified.

   Invalid_Modification_Err : exception;
   --  If an attempt is made to modify the type of the underlying object.

   Namespace_Err : exception;
   --  If an attempt is made to create or modify an object in a way
   --  incompatible with the namespace.

   Invalid_Access_Err : exception;
   --  If a parameter or an operation is not supported by the underlying
   --  object.

private

   type DOM_Implementation is null record;

   type Node_Array is array (Natural range <>) of Node;
   type Node_Array_Access is access Node_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Node_Array, Node_Array_Access);

   type Node_List is record
      Items  : Node_Array_Access := null;
      Last   : Integer := -1;
   end record;

   Null_List : constant Node_List := (null, -1);

   --  Not the most efficient way to implement a hash-table, but these are
   --  generally short lists anyway (attributes,...)
   type Named_Node_Map is new Node_List;
   Null_Node_Map : constant Named_Node_Map := (null, -1);

   ------------------
   -- Nodes htable --
   ------------------

   type Node_String is record
      N   : Node;
      Key : Sax.Symbols.Symbol;
   end record;
   No_Node_String : constant Node_String := (null, Sax.Symbols.No_Symbol);

   procedure Free (N : in out Node_String);
   function Get_Key (N : Node_String) return Sax.Symbols.Symbol;
   pragma Inline (Free, Get_Key);

   package Nodes_Htable is new Sax.HTable
     (Element       => Node_String,
      Empty_Element => No_Node_String,
      Free          => Free,
      Key           => Sax.Symbols.Symbol,
      Get_Key       => Get_Key,
      Hash          => Sax.Symbols.Hash,
      Equal         => Sax.Symbols."=");
   type Nodes_Htable_Access is access Nodes_Htable.HTable;

   -------------------
   -- Node_Name_Def --
   -------------------
   --  Attributes and Elements share the same kind description. These are
   --  grouped in the same type for ease of use

   type Node_Name_Def is record
      Prefix     : Sax.Symbols.Symbol;
      Local_Name : Sax.Symbols.Symbol;
      Namespace  : Sax.Symbols.Symbol;
   end record;
   No_Node_Name : constant Node_Name_Def :=
     (Prefix     => Sax.Symbols.No_Symbol,
      Local_Name => Sax.Symbols.No_Symbol,
      Namespace  => Sax.Symbols.No_Symbol);

   function Qualified_Name (N : Node_Name_Def) return DOM_String;
   pragma Inline (Qualified_Name);
   --  Return the qualified name of N

   procedure Set_Prefix
     (N : in out Node_Name_Def; Prefix : Sax.Symbols.Symbol);
   pragma Inline (Set_Prefix);
   --  Return or set the prefix of N

   function From_Qualified_Name
     (Doc       : Document;
      Symbols   : Sax.Utils.Symbol_Table;
      Name      : Sax.Symbols.Symbol;
      Namespace : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol)
      return Node_Name_Def;
   --  Build a node name from its qualified name. This is shared if
   --  Shared_Node_Names is True.
   --  Symbols is the symbol table in which Name and Namespace were created.

   -----------------
   -- Node_Record --
   -----------------

   type Node_Record (Node_Type : Node_Types) is record
      Parent_Is_Owner : Boolean;
      --  If False, the Parent node points to the owner document, not to the
      --  real parent in the tree (which is null).
      --  This boolean doesn't increase the size of this record, since because
      --  of alignment issues Node_Type already occupies more space than it
      --  really needs.

      Parent   : Node;
      case Node_Type is
         when Element_Node =>
            Name       : Node_Name_Def;
            Children   : Node_List;
            Attributes : Named_Node_Map;

         when Attribute_Node =>
            Attr_Name       : Node_Name_Def;
            Attr_Value      : Sax.Symbols.Symbol;

            Owner_Element   : Node;
            --  Generally an Element, but it can be a Document if the attribute
            --  hasn't been associated yet.

            Is_Id           : Boolean := False;
            Specified       : Boolean := False;
            --   ??? In fact, attributes can have children (text or
            --   entity_reference).

         when Text_Node =>
            Text : DOM_String_Access;

         when Cdata_Section_Node =>
            Cdata : DOM_String_Access;

         when Entity_Reference_Node =>
            Entity_Reference_Name : Sax.Symbols.Symbol;

         when Entity_Node =>
            Entity_Name : Sax.Symbols.Symbol;
            --  ??? Allows children for the substitution of the entity

         when Processing_Instruction_Node =>
            Target  : Sax.Symbols.Symbol;
            Pi_Data : Sax.Symbols.Symbol;

         when Comment_Node =>
            Comment : DOM_String_Access;

         when Document_Node =>
            Symbols        : Sax.Utils.Symbol_Table;
            --  Keep a handle on the symbol table to ensure the symbols remain
            --  valid while the tree exists

            Doc_Children   : Node_List;
            Doc_Type       : Node;
            Implementation : DOM_Implementation;
            Ids            : Nodes_Htable_Access;

         when Document_Type_Node =>
            Document_Type_Name : DOM_String_Access;
            Doc_Type_Children  : Node_List;

         when Document_Fragment_Node =>
            Doc_Frag_Children : Node_List;

         when Notation_Node =>
            Public_ID : DOM_String_Access;
            System_ID : DOM_String_Access;
      end case;
   end record;

   procedure Append (List : in out Node_List; N : Node);
   --  Insert N as the last element in List

   procedure Remove (List : in out Node_List; N : Node);
   --  Remove N from the list
   --  N must be an element of List, this is not checked.

   procedure Document_Add_Id
     (Doc  : Document;
      Id   : Sax.Symbols.Symbol;
      Elem : Element);
   --  Store in the document as fast access to Elem by its ID

   procedure Document_Remove_Id
     (Doc  : Document;
      Id   : Sax.Symbols.Symbol);
   --  Remove an ID associated with Elem in the fast htable access

end DOM.Core;
