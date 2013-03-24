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

with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Unicode;                   use Unicode;
with Unicode.CES;               use Unicode.CES;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;
with Sax.Encodings;             use Sax.Encodings;
with Sax.Symbols;               use Sax.Symbols;
with Sax.Utils;                 use Sax.Utils;
with Unicode.Encodings;         use Unicode.Encodings;

package body DOM.Core.Nodes is

   procedure Print_String
     (Stream       : access Ada.Streams.Root_Stream_Type'Class;
      Str          : DOM_String;
      EOL_Sequence : String;
      Encoding     : Unicode.Encodings.Unicode_Encoding);
   --  Print a string on standard output, in XML, protecting special
   --  characters.
   --  Str is encoded in Unicode/Sax.Encodings.Encoding, and the output is done
   --  with Encoding

   procedure Put
     (Stream   : access Ada.Streams.Root_Stream_Type'Class;
      Str      : DOM_String;
      Encoding : Unicode_Encoding);
   --  Print Str, but doesn't protect any special character in it

   procedure Print_Name
     (Stream       : access Ada.Streams.Root_Stream_Type'Class;
      N            : Node;
      With_URI     : Boolean;
      EOL_Sequence : String;
      Encoding     : Unicode.Encodings.Unicode_Encoding);
   --  Print the name of the node.

   function Clone_List (List : Node_List; Deep : Boolean) return Node_List;
   --  Return a clone of List. If Deep is True, then each item in the list
   --  is also cloned

   procedure Free (List : in out Node_List; Deep : Boolean);
   --  Free the list, and, if Deep is True, all its children

   function Child_Is_Valid (Parent : Node; Child : Node) return Boolean;
   --  Return True if Child can be added to Parent

   procedure Sort (Map : in out Named_Node_Map);
   --  Sort alphabetically the contents of Map (this is based on the value
   --  of Node_Name).

   function Namespace_URI (N : Node) return Symbol;
   --  Internal version returning symbols

   --------------------
   -- Child_Is_Valid --
   --------------------

   function Child_Is_Valid (Parent : Node; Child : Node) return Boolean is
   begin
      case Parent.Node_Type is
         when Attribute_Node =>
            return Child.Node_Type = Text_Node
              or else Child.Node_Type = Entity_Reference_Node;

         when Text_Node | Cdata_Section_Node | Processing_Instruction_Node
           | Comment_Node | Document_Type_Node | Notation_Node =>
            return False;

         when Entity_Reference_Node | Entity_Node | Element_Node
           | Document_Fragment_Node =>
            return Child.Node_Type = Element_Node
              or else Child.Node_Type = Processing_Instruction_Node
              or else Child.Node_Type = Comment_Node
              or else Child.Node_Type = Text_Node
              or else Child.Node_Type = Cdata_Section_Node
              or else Child.Node_Type = Entity_Reference_Node;

         when Document_Node =>
            --  ??? Should check there is one single Element_Node
            return Child.Node_Type = Processing_Instruction_Node
              or else Child.Node_Type = Comment_Node
              or else Child.Node_Type = Document_Type_Node
              or else Child.Node_Type = Element_Node;
      end case;
   end Child_Is_Valid;

   ---------------
   -- Node_Name --
   ---------------

   function Node_Name (N : Node) return DOM_String is
   begin
      case N.Node_Type is
         when Element_Node =>
            return Qualified_Name (N.Name);

         when Attribute_Node =>
            return Qualified_Name (N.Attr_Name);

         when Text_Node =>
            --  ??? Should this return an encoded string instead ?
            return "#text";

         when Cdata_Section_Node =>
            return "#cdata-section";

         when Entity_Reference_Node =>
            pragma Assert (N.Entity_Reference_Name /= No_Symbol);
            return Get (N.Entity_Reference_Name).all;

         when Entity_Node =>
            pragma Assert (N.Entity_Name /= No_Symbol);
            return Get (N.Entity_Name).all;

         when Processing_Instruction_Node =>
            pragma Assert (N.Target /= No_Symbol);
            return Get (N.Target).all;

         when Comment_Node =>
            return "#comment";

         when Document_Node =>
            return "#document";

         when Document_Type_Node =>
            pragma Assert (N.Document_Type_Name /= null);
            return N.Document_Type_Name.all;

         when Document_Fragment_Node =>
            return "document-fragment";

         when Notation_Node =>
            pragma Assert (N.Public_ID /= null);
            return N.Public_ID.all;
      end case;
   end Node_Name;

   ----------------
   -- Node_Value --
   ----------------

   function Node_Value (N : Node) return DOM_String is
   begin
      case N.Node_Type is
         when Attribute_Node =>
            pragma Assert (N.Attr_Value /= No_Symbol);
            return Get (N.Attr_Value).all;

         when Text_Node =>
            pragma Assert (N.Text /= null);
            return N.Text.all;

         when Cdata_Section_Node =>
            pragma Assert (N.Cdata /= null);
            return N.Cdata.all;

         when Processing_Instruction_Node =>
            pragma Assert (N.Pi_Data /= No_Symbol);
            return Get (N.Pi_Data).all;

         when Comment_Node =>
            pragma Assert (N.Comment /= null);
            return N.Comment.all;

         when others =>
            return "";
      end case;
   end Node_Value;

   --------------------
   -- Set_Node_Value --
   --------------------

   procedure Set_Node_Value (N : Node; Value : DOM_String) is
   begin
      case N.Node_Type is
         when Attribute_Node =>
            --  ??? If Specified is False, we should make a copy and assign
            --  it to the owner element
            N.Attr_Value := Find (Owner_Document (N).Symbols, Value);
            N.Specified := True;

         when Text_Node =>
            Free (N.Text);
            N.Text := new DOM_String'(Value);

         when Cdata_Section_Node =>
            Free (N.Cdata);
            N.Cdata := new DOM_String'(Value);

         when Processing_Instruction_Node =>
            N.Pi_Data := Find (Owner_Document (N).Symbols, Value);

         when Comment_Node =>
            Free (N.Comment);
            N.Comment := new DOM_String'(Value);

         when others =>
            null;
      end case;
   end Set_Node_Value;

   -----------------
   -- Child_Nodes --
   -----------------

   function Child_Nodes (N : Node) return Node_List is
   begin
      case N.Node_Type is
         when Element_Node => return N.Children;
         when Document_Node => return N.Doc_Children;
         when Document_Type_Node => return N.Doc_Type_Children;
         when Document_Fragment_Node => return N.Doc_Frag_Children;
         when Entity_Reference_Node =>
            --  ??? Should return the expansion of the entity itself
            return Null_List;
         when others => return Null_List;
      end case;
   end Child_Nodes;

   -----------------
   -- First_Child --
   -----------------

   function First_Child (N : Node) return Node is
      List : constant Node_List := Child_Nodes (N);
   begin
      if List.Items = null then
         return null;
      else
         return List.Items (0);
      end if;
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child (N : Node) return Node is
      List : constant Node_List := Child_Nodes (N);
   begin
      if List.Items = null then
         return null;
      else
         return List.Items (List.Last);
      end if;
   end Last_Child;

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling (N : Node) return Node is
      List : Node_List;
   begin
      if N.Parent = null
        or else N.Parent_Is_Owner
        or else N.Node_Type = Attribute_Node
      then
         return null;
      end if;

      List := Child_Nodes (N.Parent);

      for J in 1 .. List.Last loop
         if List.Items (J) = N then
            return List.Items (J - 1);
         end if;
      end loop;

      return null;
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling (N : Node) return Node is
      List : Node_List;
   begin
      if N.Parent = null
        or else N.Parent_Is_Owner
        or else N.Node_Type = Attribute_Node
      then
         return null;
      end if;

      List := Child_Nodes (N.Parent);
      for J in 0 .. List.Last - 1 loop
         if List.Items (J) = N then
            return List.Items (J + 1);
         end if;
      end loop;
      return null;
   end Next_Sibling;

   ---------------
   -- Node_Type --
   ---------------

   function Node_Type (N : Node) return Node_Types is
   begin
      return N.Node_Type;
   end Node_Type;

   -----------------
   -- Parent_Node --
   -----------------

   function Parent_Node (N : Node) return Node is
   begin
      if N.Node_Type = Attribute_Node
        or else N.Parent_Is_Owner
      then
         return null;
      else
         return N.Parent;
      end if;
   end Parent_Node;

   ----------------
   -- Attributes --
   ----------------

   function Attributes (N : Node) return Named_Node_Map is
   begin
      case N.Node_Type is
         when Element_Node =>
            return N.Attributes;

         when others =>
            return Null_Node_Map;
      end case;
   end Attributes;

   --------------------
   -- Owner_Document --
   --------------------

   function Owner_Document (N : Node) return Node is
      P : Node := N;
   begin
      if N.Parent_Is_Owner then
         return N.Parent;
      else
         while P /= null
           and then P.Node_Type /= Document_Node
         loop
            P := P.Parent;
         end loop;
         return P;
      end if;
   end Owner_Document;

   -------------------
   -- Namespace_URI --
   -------------------

   function Namespace_URI (N : Node) return DOM_String is
   begin
      return Get (Namespace_URI (N)).all;
   end Namespace_URI;

   -------------------
   -- Namespace_URI --
   -------------------

   function Namespace_URI (N : Node) return Symbol is
   begin
      case N.Node_Type is
         when Element_Node   =>
            if N.Name.Namespace = No_Symbol then
               return Empty_String;
            else
               return N.Name.Namespace;
            end if;
         when Attribute_Node =>
            if N.Attr_Name.Namespace = No_Symbol then
               return Empty_String;
            else
               return N.Attr_Name.Namespace;
            end if;
         when others         => return Empty_String;
      end case;
   end Namespace_URI;

   ------------
   -- Prefix --
   ------------

   function Prefix (N : Node) return DOM_String is
   begin
      case N.Node_Type is
         when Element_Node   =>
            if N.Name.Prefix = No_Symbol then
               return "";
            else
               return Get (N.Name.Prefix).all;
            end if;
         when Attribute_Node =>
            if N.Attr_Name.Prefix = No_Symbol then
               return "";
            else
               return Get (N.Attr_Name.Prefix).all;
            end if;
         when others         => return "";
      end case;
   end Prefix;

   ----------------
   -- Set_Prefix --
   ----------------

   procedure Set_Prefix (N : Node; Prefix : DOM_String) is
      Doc : constant Document := Owner_Document (N);
   begin
      if Doc = null then
         Put_Line ("Set_Prefix only works when the node is part of a"
                   & " tree already");
         return;
      end if;

      case N.Node_Type is
         when Element_Node   =>
            N.Name.Prefix := Find (Doc.Symbols, Prefix);
         when Attribute_Node =>
            N.Attr_Name.Prefix := Find (Doc.Symbols, Prefix);
         when others         => null;
      end case;
   end Set_Prefix;

   ----------------
   -- Local_Name --
   ----------------

   function Local_Name (N : Node) return DOM_String is
   begin
      case N.Node_Type is
         when Element_Node   => return Get (N.Name.Local_Name).all;
         when Attribute_Node => return Get (N.Attr_Name.Local_Name).all;
         when others         => return "";
      end case;
   end Local_Name;

   ----------------
   -- Local_Name --
   ----------------

   function Local_Name (N : Node) return Sax.Symbols.Symbol is
   begin
      case N.Node_Type is
         when Element_Node   => return N.Name.Local_Name;
         when Attribute_Node => return N.Attr_Name.Local_Name;
         when others         => return Empty_String;
      end case;
   end Local_Name;

   -------------------
   -- Insert_Before --
   -------------------

   function Insert_Before
     (N         : Node;
      New_Child : Node;
      Ref_Child : Node := null) return Node
   is
      procedure Insert_Before (List : in out Node_List);
      --  Insert New_Child before Ref_Child in List

      procedure Insert_Before (List : in out Node_List) is
         Old : Node_Array_Access := List.Items;
      begin
         for J in 0 .. List.Last loop
            if List.Items (J) = Ref_Child then
               if List.Items'Last = List.Last then
                  List.Items := new Node_Array (0 .. List.Last + 5);
                  List.Items (0 .. List.Last) := Old.all;
                  Free (Old);
               end if;
               List.Items (0 .. List.Last + 1) :=
                 List.Items (0 .. J - 1) & New_Child
                 & List.Items (J .. List.Last);
               List.Last := List.Last + 1;
               return;
            end if;
         end loop;
      end Insert_Before;

      Tmp : Node;
      pragma Unreferenced (Tmp);
   begin
      pragma Assert (Child_Is_Valid (N, New_Child));

      --  ??? New_Child should use Shared strings from the new document

      --  ??? Should check that New_Child was created from the same document
      --  (ie same DTD,...), or raise Wrong_Document_Err

      --  If New_Child is already in the tree, remove it first
      if New_Child.Parent /= null
        and then not New_Child.Parent_Is_Owner
      then
         Tmp := Remove_Child (New_Child.Parent, New_Child);
      end if;

      --  Ref_Child must be a child of N
      if Ref_Child /= null and then Ref_Child.Parent /= N then
         raise Not_Found_Err;
      end if;

      --  ???  if New_Child is Document_Fragment_Node, insert all its children

      if Ref_Child = null then
         case N.Node_Type is
            when Element_Node => Append (N.Children, New_Child);
            when Document_Node => Append (N.Doc_Children, New_Child);
            when Document_Type_Node =>
               Append (N.Doc_Type_Children, New_Child);
            when Document_Fragment_Node =>
               Append (N.Doc_Frag_Children, New_Child);
            when others => raise Hierarchy_Request_Err;
         end case;

      else
         case N.Node_Type is
            when Element_Node => Insert_Before (N.Children);
            when Document_Node => Insert_Before (N.Doc_Children);
            when Document_Type_Node => Insert_Before (N.Doc_Type_Children);
            when Document_Fragment_Node => Insert_Before (N.Doc_Frag_Children);
            when others => raise Hierarchy_Request_Err;
         end case;
      end if;
      New_Child.Parent := N;
      New_Child.Parent_Is_Owner := False;
      return New_Child;
   end Insert_Before;

   -------------------
   -- Replace_Child --
   -------------------

   function Replace_Child
     (N         : Node;
      New_Child : Node;
      Old_Child : Node) return Node
   is
      List : constant Node_List := Child_Nodes (N);
   begin
      pragma Assert (Child_Is_Valid (N, New_Child));
      --  ??? Case where New_Child is a document_fragment

      --  ??? New_Child should use Shared strings from the new document

      for J in 0 .. List.Last loop
         if List.Items (J) = Old_Child then
            List.Items (J) := New_Child;
            New_Child.Parent := N;
            New_Child.Parent_Is_Owner := False;
            return Old_Child;
         end if;
      end loop;
      return null;
   end Replace_Child;

   ------------------
   -- Remove_Child --
   ------------------

   function Remove_Child (N : Node; Old_Child : Node) return Node is
   begin
      --  ??? Shared strings should be duplicated, so as not to depend on the
      --  initial document

      case N.Node_Type is
         when Element_Node => Remove (N.Children, Old_Child);
         when Document_Node => Remove (N.Doc_Children, Old_Child);
         when Document_Type_Node => return null;
         when Document_Fragment_Node =>
            Remove (N.Doc_Frag_Children, Old_Child);
         when others => return null;
      end case;
      return Old_Child;
   end Remove_Child;

   ------------------
   -- Append_Child --
   ------------------

   function Append_Child
     (N         : Node;
      New_Child : Node) return Node is
   begin
      return Insert_Before (N, New_Child, null);
   end Append_Child;

   ---------------------
   -- Has_Child_Nodes --
   ---------------------

   function Has_Child_Nodes (N : Node) return Boolean is
   begin
      return First_Child (N) /= null;
   end Has_Child_Nodes;

   ----------------
   -- Clone_List --
   ----------------

   function Clone_List (List : Node_List; Deep : Boolean) return Node_List is
      L : Node_List := Null_List;
   begin
      if List /= Null_List and then Deep then
         L := (Items => new Node_Array'(List.Items.all), Last  => List.Last);
         for J in 0 .. L.Last loop
            L.Items (J) := List.Items (J);
         end loop;
      end if;
      return L;
   end Clone_List;

   ----------------
   -- Clone_Node --
   ----------------

   function Clone_Node (N : Node; Deep : Boolean) return Node is
      Clone : Node;
   begin
      Clone := new Node_Record (N.Node_Type);
      Clone.Parent := Owner_Document (N);
      Clone.Parent_Is_Owner := True;

      case N.Node_Type is
         when Element_Node =>
            Clone.Name := N.Name;
            Clone.Children := Clone_List (N.Children, Deep);
            Clone.Attributes := Named_Node_Map
              (Clone_List (Node_List (N.Attributes), True));

         when Attribute_Node =>
            Clone.Attr_Name := N.Attr_Name;
            Clone.Attr_Value := N.Attr_Value;

         when Text_Node =>
            if N.Text /= null then
               Clone.Text := new DOM_String'(N.Text.all);
            end if;

         when Cdata_Section_Node =>
            if N.Cdata /= null then
               Clone.Cdata := new DOM_String'(N.Cdata.all);
            end if;

         when Entity_Reference_Node =>
            Clone.Entity_Reference_Name := N.Entity_Reference_Name;

         when Entity_Node =>
            Clone.Entity_Name := N.Entity_Name;

         when Processing_Instruction_Node =>
            Clone.Target  := N.Target;
            Clone.Pi_Data := N.Pi_Data;

         when Comment_Node =>
            pragma Assert (N.Comment /= null);
            Clone.Comment := new DOM_String'(N.Comment.all);

         when Document_Node =>
            Clone.Doc_Children := Clone_List (N.Doc_Children, Deep);

         when Document_Type_Node =>
            Clone.Document_Type_Name :=
              new DOM_String'(N.Document_Type_Name.all);
            Clone.Doc_Type_Children := Clone_List (N.Doc_Type_Children, Deep);

         when Document_Fragment_Node =>
            Clone.Doc_Frag_Children := Clone_List (N.Doc_Frag_Children, Deep);

         when Notation_Node =>
            if N.Public_ID /= null then
               Clone.Public_ID := new DOM_String'(N.Public_ID.all);
            end if;

            if N.System_ID /= null then
               Clone.System_ID := new DOM_String'(N.System_ID.all);
            end if;
      end case;
      return Clone;
   end Clone_Node;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (N : Node) is
      List  : Node_List := Child_Nodes (N);
      J     : Natural := 0;
      Old   : DOM_String_Access;
      L1, L2 : Integer;
   begin
      while J < List.Last loop
         if List.Items (J).Node_Type = Text_Node
           and then List.Items (J + 1).Node_Type = Text_Node
         then
            Old := List.Items (J).Text;
            L1 := Old.all'Length;
            L2 := List.Items (J + 1).Text.all'Length;
            List.Items (J).Text := new DOM_String (1 .. L1 + L2);
            List.Items (J).Text (1 .. L1) := Old.all;
            List.Items (J).Text (L1 + 1 .. L1 + L2) :=
              List.Items (J + 1).Text.all;
            Free (List.Items (J + 1));
            Free (Old);
            List.Items (J + 1 .. List.Last - 1) :=
              List.Items (J + 2 .. List.Last);
            List.Last := List.Last - 1;
         else
            J := J + 1;
         end if;
      end loop;

      case N.Node_Type is
         when Element_Node => N.Children := List;
         when Document_Node => N.Doc_Children := List;
         when Document_Type_Node => N.Doc_Type_Children := List;
         when Document_Fragment_Node => N.Doc_Frag_Children := List;
         when others => null;
      end case;

      --  Normalize all the children
      J := 0;
      while J <= List.Last loop
         Normalize (List.Items (J));
         J := J + 1;
      end loop;
   end Normalize;

   --------------
   -- Supports --
   --------------

   function Supports
     (N : Node;
      Feature : DOM_String;
      Version : DOM_String) return Boolean
   is
      pragma Warnings (Off, N);
      pragma Warnings (Off, Feature);
      pragma Warnings (Off, Version);
   begin
      return False;
   end Supports;

   ----------
   -- Item --
   ----------

   function Item (List : Node_List; Index : Natural) return Node is
   begin
      if Index <= List.Last then
         return List.Items (Index);
      else
         return null;
      end if;
   end Item;

   ------------
   -- Length --
   ------------

   function Length (List : Node_List) return Natural is
   begin
      return List.Last + 1;
   end Length;

   --------------------
   -- Get_Named_Item --
   --------------------

   function Get_Named_Item
     (Map : Named_Node_Map; Name : DOM_String) return Node is
   begin
      for J in 0 .. Map.Last loop
         if Node_Name (Map.Items (J)) = Name then
            return Map.Items (J);
         end if;
      end loop;
      return null;
   end Get_Named_Item;

   --------------------
   -- Get_Named_Item --
   --------------------

   function Get_Named_Item
     (Map : Named_Node_Map; Name : Sax.Symbols.Symbol) return Node is
   begin
      for J in 0 .. Map.Last loop
         if Namespace_URI (Map.Items (J)) = Empty_String
           and then Local_Name (Map.Items (J)) = Name
         then
            return Map.Items (J);
         end if;
      end loop;
      return null;
   end Get_Named_Item;

   --------------------
   -- Set_Named_Item --
   --------------------

   procedure Set_Named_Item
     (Map : in out Named_Node_Map; Arg : Node; Replaces : out Node) is
   begin
      Remove_Named_Item (Map, Node_Name (Arg), Replaces);
      Append (Node_List (Map), Arg);
   end Set_Named_Item;

   --------------------
   -- Set_Named_Item --
   --------------------

   procedure Set_Named_Item (Map : in out Named_Node_Map; Arg : Node) is
      Replaces : Node;
   begin
      Set_Named_Item (Map, Arg, Replaces);
   end Set_Named_Item;

   -----------------------
   -- Remove_Named_Item --
   -----------------------

   procedure Remove_Named_Item (Map : in out Named_Node_Map; N : Node) is
   begin
      for J in 0 .. Map.Last loop
         if Map.Items (J) = N then
            Map.Items (J .. Map.Last - 1) := Map.Items (J + 1 .. Map.Last);
            Map.Last := Map.Last - 1;
            return;
         end if;
      end loop;
   end Remove_Named_Item;

   -----------------------
   -- Remove_Named_Item --
   -----------------------

   procedure Remove_Named_Item
     (Map : in out Named_Node_Map; Name : DOM_String; Removed : out Node) is
   begin
      for J in 0 .. Map.Last loop
         if Node_Name (Map.Items (J)) = Name then
            Removed := Map.Items (J);
            Map.Items (J .. Map.Last - 1) := Map.Items (J + 1 .. Map.Last);
            Map.Last := Map.Last - 1;
            return;
         end if;
      end loop;
      Removed := null;
   end Remove_Named_Item;

   -----------------------
   -- Remove_Named_Item --
   -----------------------

   procedure Remove_Named_Item
     (Map : in out Named_Node_Map; Name : DOM_String)
   is
      Remove : Node;
   begin
      Remove_Named_Item (Map, Name, Remove);
   end Remove_Named_Item;

   ----------
   -- Item --
   ----------

   function Item
     (Map : Named_Node_Map; Index : Natural) return Node is
   begin
      return Item (Node_List (Map), Index);
   end Item;

   ------------
   -- Length --
   ------------

   function Length (Map : Named_Node_Map) return Natural is
   begin
      return Map.Last + 1;
   end Length;

   -----------------------
   -- Get_Named_Item_NS --
   -----------------------

   function Get_Named_Item_NS
     (Map           : Named_Node_Map;
      Namespace_URI : DOM_String;
      Local_Name    : DOM_String) return Node is
   begin
      for J in 0 .. Map.Last loop
         if Symbol'(DOM.Core.Nodes.Namespace_URI (Map.Items (J))) =
           Namespace_URI
           and then
             Symbol'(DOM.Core.Nodes.Local_Name (Map.Items (J))) = Local_Name
         then
            return Map.Items (J);
         end if;
      end loop;
      return null;
   end Get_Named_Item_NS;

   --------------------
   -- Get_Named_Item --
   --------------------

   function Get_Named_Item_NS
     (Map           : Named_Node_Map;
      Namespace_URI : Symbol;
      Local_Name    : Symbol) return Node is
   begin
      for J in 0 .. Map.Last loop
         if DOM.Core.Nodes.Namespace_URI (Map.Items (J)) = Namespace_URI
           and then DOM.Core.Nodes.Local_Name (Map.Items (J)) = Local_Name
         then
            return Map.Items (J);
         end if;
      end loop;
      return null;
   end Get_Named_Item_NS;

   -----------------------
   -- Set_Named_Item_NS --
   -----------------------

   procedure Set_Named_Item_NS
     (Map : in out Named_Node_Map; Arg : Node; Replaces : out Node) is
   begin
      Remove_Named_Item_NS
        (Map, Namespace_URI (Arg), Local_Name (Arg), Replaces);
      Append (Node_List (Map), Arg);
   end Set_Named_Item_NS;

   -----------------------
   -- Set_Named_Item_NS --
   -----------------------

   procedure Set_Named_Item_NS
     (Map : in out Named_Node_Map; Arg : Node)
   is
      Replaces : Node;
   begin
      Set_Named_Item_NS (Map, Arg, Replaces);
   end Set_Named_Item_NS;

   --------------------------
   -- Remove_Named_Item_NS --
   --------------------------

   procedure Remove_Named_Item_NS
     (Map           : in out Named_Node_Map;
      Namespace_URI : DOM_String;
      Local_Name    : DOM_String;
      Removed       : out Node) is
   begin
      for J in 0 .. Map.Last loop
         if Symbol'(DOM.Core.Nodes.Namespace_URI (Map.Items (J))) =
           Namespace_URI
           and then
             Symbol'(DOM.Core.Nodes.Local_Name (Map.Items (J))) = Local_Name
         then
            Removed := Map.Items (J);
            Map.Items (J .. Map.Last - 1) := Map.Items (J + 1 .. Map.Last);
            Map.Last := Map.Last - 1;
            return;
         end if;
      end loop;
      Removed := null;
   end Remove_Named_Item_NS;

   --------------------------
   -- Remove_Named_Item_NS --
   --------------------------

   procedure Remove_Named_Item_NS
     (Map           : in out Named_Node_Map;
      Namespace_URI : DOM_String;
      Local_Name    : DOM_String)
   is
      Removed : Node;
   begin
      Remove_Named_Item_NS (Map, Namespace_URI, Local_Name, Removed);
   end Remove_Named_Item_NS;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Node_List; Deep : Boolean) is
   begin
      if Deep then
         for J in 0 .. List.Last loop
            Free (List.Items (J), Deep => True);
         end loop;
      end if;
      Free (List.Items);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (N : in out Node; Deep : Boolean := True) is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Node_Record, Node);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Nodes_Htable.HTable, Nodes_Htable_Access);
   begin
      if N = null then
         return;
      end if;

      case N.Node_Type is
         when Element_Node =>
            --  If we have an ID attribute, remove the element from the
            --  htable.

            if N.Attributes.Items /= null then
               for Att in N.Attributes.Items'First .. N.Attributes.Last loop
                  if Attr (N.Attributes.Items (Att)).Is_Id then
                     Document_Remove_Id
                       (Owner_Document (N),
                        N.Attributes.Items (Att).Attr_Value);
                  end if;
               end loop;
            end if;

            Free (Node_List (N.Attributes), Deep => True);
            Free (N.Children, Deep);

         when Attribute_Node =>
            null;

         when Text_Node =>
            Free (N.Text);

         when Cdata_Section_Node =>
            Free (N.Cdata);

         when Entity_Reference_Node =>
            null;

         when Entity_Node =>
            null;

         when Processing_Instruction_Node =>
            null;

         when Comment_Node =>
            Free (N.Comment);

         when Document_Node =>
            Free (N.Doc_Children, Deep);
            if N.Ids /= null then
               Nodes_Htable.Reset (N.Ids.all);
               Unchecked_Free (N.Ids);
            end if;
            N.Symbols := No_Symbol_Table;

         when Document_Type_Node =>
            Free (N.Document_Type_Name);
            Free (N.Doc_Type_Children, Deep);

         when Document_Fragment_Node =>
            Free (N.Doc_Frag_Children, Deep);

         when Notation_Node =>
            Free (N.Public_ID);
            Free (N.System_ID);
      end case;

      Internal_Free (N);
   end Free;

   ----------
   -- Sort --
   ----------

   procedure Sort (Map : in out Named_Node_Map) is
      Arr : Node_Array (0 .. Map.Last + 1) := (others => null);
      Index : Natural;
   begin
      --  ??? The algorithm is not efficient, we use Insertion_Sort
      for J in 0 .. Map.Last loop
         Index := 0;
         loop
            if Arr (Index) = null then
               Arr (Index) := Map.Items (J);
               exit;
            end if;

            if Node_Name (Map.Items (J)) <= Node_Name (Arr (Index)) then
               Arr (Index + 1 .. Arr'Last) := Arr (Index .. Arr'Last - 1);
               Arr (Index) := Map.Items (J);
               exit;
            end if;
            Index := Index + 1;
         end loop;
      end loop;
      for J in 0 .. Map.Last loop
         Map.Items (J) := Arr (J);
      end loop;
   end Sort;

   ------------------
   -- Print_String --
   ------------------

   procedure Print_String
     (Stream       : access Ada.Streams.Root_Stream_Type'Class;
      Str          : DOM_String;
      EOL_Sequence : String;
      Encoding     : Unicode.Encodings.Unicode_Encoding)
   is
      J : Natural := Str'First;
      C : Unicode.Unicode_Char;
      Buffer : Byte_Sequence (1 .. 20);
      Index : Natural;
   begin
      while J <= Str'Last loop
         Sax.Encodings.Encoding.Read (Str, J, C);
         case C is
            when Ampersand      =>
               String'Write (Stream, Amp_DOM_Sequence);
            when Less_Than_Sign =>
               String'Write (Stream, Lt_DOM_Sequence);
            when Greater_Than_Sign     =>
               String'Write (Stream, Gt_DOM_Sequence);
            when Quotation_Mark        =>
               String'Write (Stream, Quot_DOM_Sequence);
               --  when Apostrophe            => Put ("&apos;");
            when Horizontal_Tabulation =>
               String'Write (Stream, Tab_Sequence);
            when Line_Feed             =>
               Put (Stream, EOL_Sequence, Encoding);
            when Carriage_Return       =>
               String'Write (Stream, Cr_Sequence);
            when 0 .. 8 | 11 .. 12 | 14 .. 31 =>
               declare
                  Img : constant String := Unicode_Char'Image (C);
               begin
                  String'Write
                    (Stream, "&#" & Img (Img'First + 1 .. Img'Last) & ";");
               end;
            when others                =>
               Index := Buffer'First - 1;
               Encoding.Encoding_Scheme.Encode
                 (Encoding.Character_Set.To_CS (C), Buffer, Index);
               String'Write (Stream, Buffer (Buffer'First .. Index));
         end case;
      end loop;
   end Print_String;

   ---------
   -- Put --
   ---------

   procedure Put
     (Stream   : access Ada.Streams.Root_Stream_Type'Class;
      Str      : DOM_String;
      Encoding : Unicode_Encoding)
   is
      J : Natural := Str'First;
      C : Unicode.Unicode_Char;
      Buffer : Byte_Sequence (1 .. 20);
      Index : Natural;
   begin
      while J <= Str'Last loop
         Sax.Encodings.Encoding.Read (Str, J, C);
         Index := Buffer'First - 1;
         Encoding.Encoding_Scheme.Encode
           (Encoding.Character_Set.To_CS (C), Buffer, Index);
         String'Write (Stream, Buffer (Buffer'First .. Index));
      end loop;
   end Put;

   ----------------
   -- Print_Name --
   ----------------

   procedure Print_Name
     (Stream       : access Ada.Streams.Root_Stream_Type'Class;
      N            : Node;
      With_URI     : Boolean;
      EOL_Sequence : String;
      Encoding     : Unicode.Encodings.Unicode_Encoding) is
   begin
      if With_URI then
         Print_String
           (Stream,
            Namespace_URI (N) & Colon_Sequence & Local_Name (N),
            EOL_Sequence, Encoding);
      else
         Print_String (Stream, Node_Name (N), EOL_Sequence, Encoding);
      end if;
   end Print_Name;

   -----------
   -- Write --
   -----------

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
      Collapse_Empty_Nodes  : Boolean := True)
   is
      procedure Recursive_Print (N : Node);
      --  Print N recursively

      procedure Recursive_Print (List : Node_List);
      --  Print all nodes in List

      function Has_Non_Whitespace (N : Text) return Boolean;
      --  True if the text code contains text other than whitespaces

      procedure Newline;
      --  Go to the next line, when pretty-printing is activated

      procedure Indent_Line;
      --  Indent the newline if needed

      Indent : Natural := 0;
      At_Bol : Boolean := True;

      -------------
      -- Newline --
      -------------

      procedure Newline is
      begin
         if Pretty_Print then
            String'Write (Stream, "" & ASCII.LF);
            At_Bol := True;
         end if;
      end Newline;

      -----------------
      -- Indent_Line --
      -----------------

      procedure Indent_Line is
      begin
         if Pretty_Print and At_Bol then
            String'Write (Stream, (1 .. Indent => ' '));
            At_Bol := False;
         end if;
      end Indent_Line;

      ------------------------
      -- Has_Non_Whitespace --
      ------------------------

      function Has_Non_Whitespace (N : Text) return Boolean is
         Val   : constant Byte_Sequence := Node_Value (N);
      begin
         for V in Val'Range loop
            if Val (V) /= ' '
              and then Val (V) /= ASCII.HT
              and then Val (V) /= ASCII.LF
            then
               return True;
            end if;
         end loop;
         return False;
      end Has_Non_Whitespace;

      ---------------------
      -- Recursive_Print --
      ---------------------

      procedure Recursive_Print (List : Node_List) is
      begin
         for J in 0 .. List.Last loop
            Recursive_Print (List.Items (J));
         end loop;
      end Recursive_Print;

      ---------------------
      -- Recursive_Print --
      ---------------------

      procedure Recursive_Print (N : Node) is
      begin
         if N = null then
            return;
         end if;

         case N.Node_Type is
            when Element_Node =>
               Indent_Line;
               Put (Stream, Less_Than_Sequence, Encoding);
               Print_Name (Stream, N, With_URI, EOL_Sequence, Encoding);

               --  Sort the XML attributes as required for canonical XML
               Sort (N.Attributes);

               for J in 0 .. N.Attributes.Last loop
                  Put (Stream, Space_Sequence, Encoding);
                  Recursive_Print (N.Attributes.Items (J));
               end loop;

               if Collapse_Empty_Nodes and then N.Children = Null_List then
                  Put
                    (Stream, Slash_Sequence & Greater_Than_Sequence, Encoding);
               else
                  Put (Stream, Greater_Than_Sequence, Encoding);

                  --  If the first child is a text node with text other than
                  --  whitespaces, we'll have to preserve whitespaces in the
                  --  children, otherwise we are free to modify them when
                  --  pretty-printing.

                  if Pretty_Print then
                     if Length (N.Children) = 0
                       or else N.Children.Items (0).Node_Type /= Text_Node
                       or else not Has_Non_Whitespace (N.Children.Items (0))
                     then
                        Newline;
                     end if;
                  end if;

                  Indent := Indent + 1;
                  Recursive_Print (N.Children);
                  Indent := Indent - 1;

                  Indent_Line;
                  Put (Stream, Less_Than_Sequence & Slash_Sequence, Encoding);
                  Print_Name (Stream, N, With_URI, EOL_Sequence, Encoding);
                  Put (Stream, Greater_Than_Sequence, Encoding);
               end if;
               Newline;

            when Attribute_Node =>
               At_Bol := False;
               Print_Name (Stream, N, With_URI, EOL_Sequence, Encoding);
               Put (Stream,
                    Equals_Sign_Sequence & Quotation_Mark_Sequence, Encoding);
               Print_String (Stream, Node_Value (N), EOL_Sequence, Encoding);
               Put (Stream, Quotation_Mark_Sequence, Encoding);

            when Processing_Instruction_Node =>
               Indent_Line;
               Put
                 (Stream,
                  Less_Than_Sequence
                  & Question_Mark_Sequence
                  & Get (N.Target).all, Encoding);

               if N.Pi_Data = Empty_String then
                  Put (Stream, Space_Sequence, Encoding);

               else
                  declare
                     P : constant Cst_Byte_Sequence_Access := Get (N.Pi_Data);
                     C : Unicode_Char;
                     Index : Natural := P'First;
                  begin
                     Sax.Encodings.Encoding.Read (P.all, Index, C);

                     if C /= Space then
                        Put (Stream, Space_Sequence, Encoding);
                     end if;
                  end;
               end if;

               Put
                 (Stream,
                  Get (N.Pi_Data).all & Question_Mark_Sequence
                  & Greater_Than_Sequence, Encoding);
               Newline;

            when Comment_Node =>
               if Print_Comments then
                  if Pretty_Print then
                     Newline;
                     Indent_Line;
                  end if;
                  Put (Stream, "<!--", Encoding);
                  Put (Stream, Node_Value (N), Encoding);
                  Put (Stream, "-->", Encoding);
                  Newline;
               end if;

            when Document_Node =>
               if Print_XML_Declaration then
                  String'Write
                    (Stream, Write_Bom (Encoding.Encoding_Scheme.BOM));
                  Put
                    (Stream, "<?xml version=""1.0"" encoding="""
                     & Encoding.Name.all & """?>", Encoding);
                  Print_String (Stream, "" & ASCII.LF, EOL_Sequence, Encoding);
               end if;
               Recursive_Print (N.Doc_Children);

            when Document_Fragment_Node =>
               Recursive_Print (N.Doc_Frag_Children);

            when Document_Type_Node | Notation_Node =>
               null;

            when Text_Node =>
               if not Pretty_Print then
                  Print_String
                    (Stream, Node_Value (N), EOL_Sequence, Encoding);

               elsif Has_Non_Whitespace (N) then
                  declare
                     Val   : constant Byte_Sequence := Node_Value (N);
                     First : Integer := Val'Last + 1;
                     Last  : Integer := Val'Last;
                  begin
                     for V in Val'Range loop
                        if Val (V) /= ' '
                          and then Val (V) /= ASCII.HT
                          and then Val (V) /= ASCII.LF
                        then
                           First := V;
                           exit;
                        end if;
                     end loop;

                     for V in reverse First + 1 .. Val'Last loop
                        if Val (V) /= ' '
                          and then Val (V) /= ASCII.HT
                          and then Val (V) /= ASCII.LF
                        then
                           Last := V;
                           exit;
                        end if;
                     end loop;

                     Print_String
                       (Stream, Val (First .. Last),
                        EOL_Sequence, Encoding);
                  end;
               end if;

            when others =>
               Print_String (Stream, Node_Value (N), EOL_Sequence, Encoding);
         end case;
      end Recursive_Print;

   begin
      Recursive_Print (N);
   end Write;

   -----------
   -- Print --
   -----------

   procedure Print
     (N                    : Node;
      Print_Comments       : Boolean := False;
      Print_XML_PI         : Boolean := False;
      With_URI             : Boolean := False;
      EOL_Sequence         : String  := Sax.Encodings.Lf_Sequence;
      Encoding             : Unicode.Encodings.Unicode_Encoding :=
        Unicode.Encodings.Get_By_Name ("utf-8");
      Collapse_Empty_Nodes : Boolean := False)
   is
   begin
      Write
        (Stream            => Ada.Text_IO.Text_Streams.Stream (Current_Output),
         N                 => N,
         Print_Comments    => Print_Comments,
         Print_XML_Declaration => Print_XML_PI,
         With_URI          => With_URI,
         EOL_Sequence      => EOL_Sequence,
         Encoding          => Encoding,
         Collapse_Empty_Nodes => Collapse_Empty_Nodes);
   end Print;

   ----------
   -- Dump --
   ----------

   procedure Dump (N : Node; With_URI : Boolean := False) is
      procedure Dump (N : Node; Prefix : String);
      --  Dump N, with a leading Prefix on the line

      procedure Dump (List : Node_List; Prefix : String);
      --  Same as above, but for a list.

      Encoding : constant Unicode_Encoding := Get_By_Name ("utf-8");
      EOL_Sequence : constant Byte_Sequence := Sax.Encodings.Lf_Sequence;

      Stream : constant Ada.Text_IO.Text_Streams.Stream_Access :=
        Ada.Text_IO.Text_Streams.Stream (Current_Output);

      ----------
      -- Dump --
      ----------

      procedure Dump (List : Node_List; Prefix : String) is
      begin
         for J in 0 .. List.Last loop
            Dump (List.Items (J), Prefix);
         end loop;
      end Dump;

      ----------
      -- Dump --
      ----------

      procedure Dump (N : Node; Prefix : String) is
      begin
         case N.Node_Type is
            when Element_Node =>
               String'Write (Stream, Prefix & "Element: ");
               Print_Name
                 (Stream, N, With_URI, EOL_Sequence & ASCII.LF, Encoding);

               --  Sort the XML attributes as required for canonical XML
               Sort (N.Attributes);

               for J in 0 .. N.Attributes.Last loop
                  Dump (N.Attributes.Items (J),
                        Prefix => Prefix & "  ");
               end loop;

               Dump (N.Children, Prefix & "  ");

            when Attribute_Node =>
               String'Write (Stream, Prefix & "Attribute: ");
               Print_Name (Stream, N, With_URI, EOL_Sequence, Encoding);
               Character'Write (Stream, '=');
               --  ??? Could be a tree
               Print_String
                 (Stream, Node_Value (N), EOL_Sequence, Encoding);
               Character'Write (Stream, ASCII.LF);

            when Processing_Instruction_Node =>
               String'Write (Stream, Prefix & "PI: " & Get (N.Target).all);
               String'Write
                 (Stream, Prefix & "   Data: " & Get (N.Pi_Data).all);

            when Comment_Node =>
               String'Write (Stream, Prefix & "Comment: " & Node_Value (N));

            when Document_Node =>
               String'Write (Stream, Prefix & "Document: ");
               Dump (N.Doc_Children, Prefix => Prefix & "  ");

            when Document_Fragment_Node =>
               String'Write (Stream, Prefix & "Document_Fragment: ");
               Dump (N.Doc_Frag_Children, Prefix => Prefix & "  ");

            when Document_Type_Node =>
               String'Write (Stream, Prefix & "Document_Type: ");

            when Notation_Node =>
               String'Write (Stream, Prefix & "Notation:");

            when Text_Node =>
               String'Write (Stream, Prefix & "Text: ");
               Print_String (Stream, Node_Value (N), EOL_Sequence, Encoding);
               Character'Write (Stream, ASCII.LF);

            when Cdata_Section_Node =>
               String'Write (Stream, Prefix & "Cdata: ");
               Print_String (Stream, Node_Value (N), EOL_Sequence, Encoding);
               Character'Write (Stream, ASCII.LF);

            when Entity_Reference_Node =>
               String'Write (Stream, Prefix & "Entity_Reference: ");
               Print_String (Stream, Node_Value (N), EOL_Sequence, Encoding);
               Character'Write (Stream, ASCII.LF);

            when Entity_Node =>
               String'Write (Stream, Prefix & "Entity: ");
               Print_String (Stream, Node_Value (N), EOL_Sequence, Encoding);
               Character'Write (Stream, ASCII.LF);
         end case;
      end Dump;

   begin
      if N /= null then
         Dump (N, Prefix => "");
      end if;
   end Dump;

end DOM.Core.Nodes;
