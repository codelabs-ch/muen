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

with Muxml.system_src_schema;
with Muxml.Utils;
--with Mulog;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
---with Ada.Text_IO;
with McKae.XML.XPath.XIA;

package body Mutools.Amend.Ordering
is
   Order_Info : Order_Information;

   ------------------------------------------------------------------------

   function "=" (L, R : String_Vector.Vector) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      if L.Length /= R.Length then
         return False;
      end if;
      for I in L.First_Index .. L.Last_Index loop
         if L (I) /= R (I) then
            return False;
         end if;
      end loop;
      return True;
   end "=";

   ------------------------------------------------------------------------

   function "=" (L, R : Vector_Tuple) return Boolean
   is
   begin
      return L.Node_Names = R.Node_Names and L.Type_Names = R.Type_Names;
   end "=";

   ------------------------------------------------------------------------

   function Get_Insert_Index
      (Anchestors : String_Vector.Vector;
       New_Child  : String;
       Siblings   : String_Vector.Vector)
      return Insert_Index
   is
      Child_Order : String_Vector.Vector;
      Index_NC, Index_Sib : String_Vector.Extended_Index;

      ---------------------------------------------------------------------

      -- determine the type of the first anchestor
      -- by going back in the Anchestor-list until some anchestor has a
      -- unique type and then trace that type forward
      function Get_Parent_Type
         (Anchestors : String_Vector.Vector)
         return String;

      ---------------------------------------------------------------------

      function Get_Parent_Type
         (Anchestors : String_Vector.Vector)
         return String
      is
         package ASU renames Ada.Strings.Unbounded;
         function U (S : String) return ASU.Unbounded_String
            renames ASU.To_Unbounded_String;
         function S (U : ASU.Unbounded_String) return String
            renames ASU.To_String;

         Index_Unique, Index : String_Vector.Extended_Index
            := String_Vector.No_Index;
         Parent_Type : ASU.Unbounded_String;
         Children : Vector_Tuple;
      begin
         ---Ada.Text_IO.Put_Line ("anchestors: " & To_String (Anchestors));

         for I in Anchestors.First_Index .. Anchestors.Last_Index loop
            if Order_Info.Tag_To_Type.Contains (Anchestors (I)) then
               Index_Unique := I;
               exit;
            end if;
         end loop;

         if Index_Unique = String_Vector.No_Index then
            raise Insufficient_Information with
               "Cannot determine type of any anchestor.";
         end if;
         ---Ada.Text_IO.Put_Line ("Index_Unique: " & Index_Unique'Image
         ---                      & " First: " &  Anchestors.First_Index'Image);

         Parent_Type := U (Order_Info.Tag_To_Type
                           (Anchestors (Index_Unique)).First_Element);
         ---Ada.Text_IO.Put_Line ("Parent_Type: " & S (Parent_Type));

         for I in reverse Anchestors.First_Index .. Index_Unique - 1 loop
            ---Ada.Text_IO.Put_Line ("I: " & I'Image);

            Children := Order_Info.Type_To_Children (S (Parent_Type));
            ---Ada.Text_IO.Put_Line ("Children: " & To_String (Children.Node_Names));

            Index := Children.Node_Names.Find_Index (Anchestors (I));
            ---Ada.Text_IO.Put_Line ("index in anchestor trace: "
            ---                      & Index'Image
            ---                      & Anchestors (I));
            if Index = String_Vector.No_Index then
               raise Validation_Error with
                  "Anchestors sequence given to Get_Parent_Type violates schema";
            end if;
            Parent_Type := U (Children.Type_Names (Index));
            ---Ada.Text_IO.Put_Line ("Parent_Type: " & S (Parent_Type));
         end loop;

         return S (Parent_Type);
      end Get_Parent_Type;

      ---------------------------------------------------------------------

   begin
      if Anchestors.Is_Empty then
         -- this is not supported on purpose (to avoid mistakes)
         raise Not_Implemented with
            "Get_Insert_Index called with empty anchestor list.";
      end if;

      ---Ada.Text_IO.Put_Line ("DEBUG: Parent Type: " & Get_Parent_Type (Anchestors));

      -- get type of the parent
      if Order_Info.Tag_To_Type.Contains (Anchestors.First_Element) then
         Child_Order := Order_Info.Type_To_Children
            (Order_Info.Tag_To_Type (Anchestors.First_Element).First_Element).Node_Names;

         ---Ada.Text_IO.Put_Line ("DEBUG: Took quick path for: " & Anchestors.First_Element);
      else
         begin
            Child_Order := Order_Info.Type_To_Children
               (Get_Parent_Type (Anchestors)).Node_Names;
            ---Ada.Text_IO.Put_Line ("DEBUG: Slow path for: " & Anchestors.First_Element);
         exception
            when Insufficient_Information =>
               return Insert_Index (-1);
         end;
      end if;

      -- get index of the new_child in the child-list of the parent
      Index_NC := Child_Order.Find_Index (Item => New_Child);
      if Index_NC = String_Vector.No_Index then
         return Insert_Index (-2);
      end if;

      -- find index of first sibling which occurs after New_Child in
      -- the child-list of Parent
      for I in Siblings.First_Index .. Siblings.Last_Index loop
         Index_Sib := Child_Order.Find_Index (Item => Siblings (I));
         if Index_Sib > Index_NC then
            return Insert_Index (I);
         end if;
      end loop;

      -- default return value: 'behind the last element'
      return Insert_Index (Siblings.Length);
   end Get_Insert_Index;

   ------------------------------------------------------------------------

   procedure Init_Order_Information
       (Schema_XML_Data : String)
   is
      use all type DOM.Core.Node_Types;
      use all type DOM.Core.Node;
      use all type Ada.Containers.Count_Type;

      package Node_Vector is new Ada.Containers.Indefinite_Vectors
         (Index_Type   => Natural,
          Element_Type => DOM.Core.Node);

      Schema      : Muxml.XML_Data_Type;
      Schema_Node : DOM.Core.Node; -- the root node of the actual schema
      Simple_XMLTypes, Elem_And_Container_Tags, Elem_Container_Tags
                  : String_Vector.Vector;
      Backtrace   : Node_Vector.Vector;

      ---------------------------------------------------------------------

      -- check that:
      --   there is only one namespace
      --   there are no elements "import", "include", " redefine", "example"
      --      or "any"
      --   there is no <attribute> element with name='substitutionGroup'
      procedure Check_NS_And_Tag_Assumptions
         (Schema_Node : DOM.Core.Node;
          Schema_XML_Data : String);

      ---------------------------------------------------------------------

      -- check that the children of each type in Order_Info have unique names
      procedure Check_Unique_Names;

      ---------------------------------------------------------------------

      -- recursive evaluation of a node to get a vector with information
      -- about potential children.
      -- The name of the function specifies the type of node it is meant for
      --   (except for "Grouptags", which is a wrapper)
      function Eval_ComplexContent
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_Element
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_Extension
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_Grouptags
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_Group
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_Restriction
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_SeqChoAll
         (Node : DOM.Core.Node) return Vector_Tuple;

      -- Head of recursive evaluation of a fixed type
      -- returns finished entry of the given type
      function Eval_Type_By_Name (Type_Name : String) return Vector_Tuple;

      -- Start recursive evaluation of a new type
      -- Writes map entries and initiates evaluation of types of children
      procedure Eval_Entrypoint (Type_Name : String);

      -- get the value of attribute Name of Node.
      -- An empty string is returned if the attribute does not exist.
      function Get_Attr (Node : DOM.Core.Node; Name : String) return String;

      -- get list of all children which are of type "Element_Node"
      -- (as defined in DOM.Core.Nodes.Node_Type)
      function Get_Element_Children
         (Node : DOM.Core.Node) return Node_Vector.Vector;

      -- return node defining a group of type "Name_Attr"
      function Get_Group_Definition (Name_Attr : String) return DOM.Core.Node;

      -- return a node with tag "Node_Name" with attribute "name=Name_Attrib"
      -- if Name_Attr is empty, it is not considered
      function Get_Toplevel_Definition
         (Name_Attr : String;
          Node_Name : String := "")
         return DOM.Core.Node;

      ---------------------------------------------------------------------

      -- Initialize a Vec with values defined by Name
      procedure Initialize_Vectors
         (Vec  : out String_Vector.Vector;
          Name :     String);

      ---------------------------------------------------------------------

      -- shortcut for Node_Name without prefix
      function Name (Node : DOM.Core.Node) return String;

      ---------------------------------------------------------------------

      -- Remove last element from backtrace
      procedure Pop_From_Backtrace;

      ---------------------------------------------------------------------

      -- Add Node to backtrace and raise error if loop is detected
      procedure Push_To_Backtrace (Node : DOM.Core.Node);

      ---------------------------------------------------------------------

      -- if Name contains ':' return the slice of Name after the first
      -- occurrence. Otherwise, Name is returned.
      function Remove_Namespace (Name : String) return String;

      ---------------------------------------------------------------------

      procedure Check_NS_And_Tag_Assumptions
         (Schema_Node : DOM.Core.Node;
          Schema_XML_Data : String)
      is
         Bad_Nodes : DOM.Core.Node_List
            := McKae.XML.XPath.XIA.XPath_Query
            (N     => Schema_Node,
             XPath => "//*[local-name(.)='import'] | "
             & "//*[local-name(.)='include'] | "
             & "//*[local-name(.)='redefine'] | "
             & "//*[local-name(.)='example'] | "
             & "//*[local-name(.)='any']");

      begin
         -- check namespaces
         --- workaround: The namespace declarations are not attributes of
         --- the root node. It seems impossible to access all declared
         --- namespaces via the DOM-interface. Hence, we do it by hand.
         declare
            Whitespace : constant String
               := "" & Character'Val (9)    -- horizontal tab
                     & Character'Val (10)   -- line feed
                     & Character'Val (13)   -- carriage return
                     & Character'Val (32); -- space

            Schema_Start : constant String
               := "<" & DOM.Core.Nodes.Node_Name (Schema_Node);
            L_Bound : constant Natural
               := Schema_Start'Length +
                   Ada.Strings.Fixed.Index (Source => Schema_XML_Data,
                                            Pattern => Schema_Start,
                                            From => 1);
            R_Bound : constant Natural
               := Ada.Strings.Fixed.Index
               (Source => Schema_XML_Data,
                Pattern => ">",
                From => L_Bound) - 1;
            Count : Natural := 0;
         begin
            if L_Bound = 0 or R_Bound = 0 then
               raise Validation_Error with
                  "Cound not find schema-node for namespace-checking";
            end if;
            for I in Whitespace'Range loop
               Count := Count +
                  Ada.Strings.Fixed.Count
                  (Source  => Schema_XML_Data (L_Bound .. R_Bound),
                   Pattern => "" & Whitespace (I) & "xmlns");
            end loop;
            if Count > 1 then
               raise Not_Implemented with
                  "Schema declares multiple namespaces.";
            end if;
         end;

         -- check forbidden element names
         if  DOM.Core.Nodes.Length (List => Bad_Nodes) > 0 then
            raise Not_Implemented with
               "Schema contains elements named "
               & "'import', 'include', 'redefine', 'example' or 'any'";
         end if;

         -- check that no attribute-node defines a substitution group
         Bad_Nodes   := McKae.XML.XPath.XIA.XPath_Query
            (N     => Schema_Node,
             XPath => "//*[local-name(.)='attribute']");
         for I in 0 .. DOM.Core.Nodes.Length (List => Bad_Nodes) - 1 loop
            declare
               Node : constant DOM.Core.Node
                  := DOM.Core.Nodes.Item (List  => Bad_Nodes,
                                          Index => I);
            begin
               if Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "name")
                  and then
                  Get_Attr (Node => Node, Name => "name") = "substitutionGroup"
               then
                  raise Not_Implemented with
                     "Schema contains element with attribute 'substitutionGroup'";
               end if;
            end;
         end loop;

      end Check_NS_And_Tag_Assumptions;

      ---------------------------------------------------------------------

      procedure Check_Unique_Names
      is
         Names : String_Vector.Vector;
         Index : String_Vector.Extended_Index;
      begin
         for Cursor in Order_Info.Type_To_Children.Iterate loop
            Names := String_To_VectorTuple.Element (Cursor).Node_Names;
            Index := Names.First_Index;
            while Index < Names.Last_Index loop
               if String_Vector.Find_Index
                  (Container => Names,
                   Item      => Names (Index),
                   Index     => Index + 1) /= String_Vector.No_Index
               then
                  raise Not_Implemented with
                     "Node_Names for Key '"
                     & String_To_VectorTuple.Key (Cursor)
                     & "' of Order_Info contain entry '"
                     & Names (Index)
                     & "' at least twice. Name must be unique within one type";
               end if;
               Index := Index + 1;
            end loop;
         end loop;
      end Check_Unique_Names;

      ---------------------------------------------------------------------

      function Eval_ComplexContent
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Empty_Vectors : Vector_Tuple;
      begin
         ---Ada.Text_IO.Put_Line ("DEBUG: Begin ComplexContent. Name=" &  Name (Node => Node));

         for Child of Get_Element_Children (Node => Node) loop
            if Name (Node => Child) = "restriction" then
               return Eval_Restriction (Node => Child);
            elsif Name (Node => Child) = "extension" then
               return Eval_Extension (Node => Child);
            end if;
         end loop;

         return Empty_Vectors;

      end Eval_ComplexContent;

      ---------------------------------------------------------------------

      function Eval_Element
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Output : Vector_Tuple;
      begin
         Push_To_Backtrace (Node => Node);
         ---Ada.Text_IO.Put_Line ("DEBUG: Begin Element. Name=" &  Name (Node => Node));

         if    not Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "name")
            or not Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "type")
         then
            raise Not_Implemented with
               "Found element-node without 'name' or 'type' attribute.";
         elsif Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "substitutionGroup") then
            raise Not_Implemented with
               "Found element-node with 'substitutionGroup' attribute.";
         end if;

         Output.Node_Names.Append
            (New_Item => Get_Attr (Node => Node,
                                   Name => "name"));
         Output.Type_Names.Append
            (New_Item =>  Remove_Namespace (Get_Attr (Node => Node,
                                                      Name => "type")));
         Pop_From_Backtrace;
         return Output;
      end Eval_Element;

      ---------------------------------------------------------------------

      procedure Eval_Entrypoint (Type_Name : String)
      is
         Types_Vector, Names_Vector : String_Vector.Vector;
         VT : Vector_Tuple;
      begin
         ---Ada.Text_IO.Put_Line ("DEBUG: Begin Entrypoint.");

         if Order_Info.Type_To_Children.Contains (Key => Type_Name) then
            return;
         end if;

         -- assign VT to make sure Eval_Type_By_Name is out of scope
         -- when tempering with cursors
         VT := Eval_Type_By_Name (Type_Name => Type_Name);
         Order_Info.Type_To_Children.Insert
            (Key => Type_Name,
             New_Item => VT);

         Names_Vector := Order_Info.Type_To_Children.Element
            (Key => Type_Name).Node_Names;
         Types_Vector := Order_Info.Type_To_Children.Element
            (Key => Type_Name).Type_Names;

         for I in Types_Vector.First_Index .. Types_Vector.Last_Index loop
            Eval_Entrypoint (Type_Name => Types_Vector (I));

            if not Order_Info.Tag_To_Type.Contains (Key => Names_Vector (I)) then
               Order_Info.Tag_To_Type.Insert
                  (Key      => Names_Vector (I),
                   New_Item => String_Vector.To_Vector
                                 (New_Item => Types_Vector (I),
                                  Length => 1));
            elsif not Order_Info.Tag_To_Type (Names_Vector (I)).Contains
               (Item => Types_Vector (I))
            then
               Order_Info.Tag_To_Type
                  (Names_Vector (I)).Append (New_Item => Types_Vector (I));
            end if;
         end loop;
      end Eval_Entrypoint;

      ---------------------------------------------------------------------

      function Eval_Extension
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Output, Tuple : Vector_Tuple;

      begin
         ---Ada.Text_IO.Put_Line ("DEBUG: Begin Extension. Name=" &  Name (Node => Node));

         if not Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "base") then
            raise Validation_Error with
               "Evaluation of schema found 'extension' "
               & "without 'base' attribute.";
         end if;

         Output := Eval_Type_By_Name (Get_Attr (Node => Node, Name => "base"));
         for Child of Get_Element_Children (Node => Node) loop
            if Elem_Container_Tags.Contains (Item => Name (Node => Child)) then
               Tuple := Eval_Grouptags (Node => Child);
               Output.Node_Names.Append (Tuple.Node_Names);
               Output.Type_Names.Append (Tuple.Type_Names);
            end if;
         end loop;

         return Output;
      end Eval_Extension;

      ---------------------------------------------------------------------

      function Eval_Group
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Output, Tuple : Vector_Tuple;
         Def_Node : DOM.Core.Node;
      begin
         Push_To_Backtrace (Node => Node);
         ---Ada.Text_IO.Put_Line ("DEBUG: Begin Group. Name=" &  Name (Node => Node));

         if Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "ref") then
            Def_Node := Get_Group_Definition
               (Name_Attr => Get_Attr (Node => Node,
                                       Name => "ref"));
            Output := Eval_Grouptags (Node => Def_Node);
            Pop_From_Backtrace;
            return Output;

         elsif Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "name") then
            for Child of Get_Element_Children (Node => Node) loop
               if Elem_And_Container_Tags.Contains (Item => Name (Node => Child)) then
                  Tuple := Eval_Grouptags (Node => Child);
                  Output.Node_Names.Append (Tuple.Node_Names);
                  Output.Type_Names.Append (Tuple.Type_Names);
               end if;
            end loop;
            Pop_From_Backtrace;
            return Output;

         else
            raise Validation_Error with
               "Evaluation of Group found node which is neither "
               & "'name' nor 'ref' attribute. Not implemented.";
         end if;
      end Eval_Group;

      ---------------------------------------------------------------------

      function Eval_Grouptags
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Node_Name : constant String := Name (Node => Node);
      begin
         ---Ada.Text_IO.Put_Line ("DEBUG: Begin Grouptags. Name=" &  Name (Node => Node));

         if Node_Name = "element" then
            return Eval_Element (Node => Node);

         elsif Node_Name = "sequence"
            or Node_Name = "choice"
            or Node_Name = "all"
         then
            return Eval_SeqChoAll (Node => Node);

         elsif Node_Name = "group" then
            return Eval_Group (Node => Node);
         else
            raise Validation_Error with
               "Evaluation of Grouptags found node with name '"
               & Node_Name
               & "' which is unknown.";
         end if;

         -- dummy statement to supress compiler warning
         -- return Vector_Tuple.Vector;
      end Eval_Grouptags;

      ---------------------------------------------------------------------

      function Eval_Restriction
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Empty_Tuple : Vector_Tuple;
      begin
         ---Ada.Text_IO.Put_Line ("DEBUG: Begin Restriction. Name=" &  Name (Node => Node));

         for Child of Get_Element_Children (Node => Node) loop
            if Elem_Container_Tags.Contains (Name (Node => Child)) then
               return Eval_Grouptags (Node => Child);
            end if;
         end loop;

         return Empty_Tuple;
      end Eval_Restriction;

      ---------------------------------------------------------------------

      function Eval_SeqChoAll
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Output, Tuple : Vector_Tuple;
      begin
         Push_To_Backtrace (Node => Node);
         ---Ada.Text_IO.Put_Line ("DEBUG: Begin SeqChoAll. Name=" &  Name (Node => Node));

         for Child of Get_Element_Children (Node => Node) loop
            if Elem_And_Container_Tags.Contains (Item => Name (Node => Child)) then
               ---Ada.Text_IO.Put_Line ("DEBUG: SeqChoAll. Name=" & Name (Node => Child));

               Tuple := Eval_Grouptags (Node => Child);
               Output.Node_Names.Append (New_Item => Tuple.Node_Names);
               Output.Type_Names.Append (New_Item => Tuple.Type_Names);
            end if;
         end loop;
         Pop_From_Backtrace;
         return Output;

      end Eval_SeqChoAll;

      ---------------------------------------------------------------------

      function Eval_Type_By_Name (Type_Name : String) return Vector_Tuple
      is
         Empty_Vector : Vector_Tuple;
         Node : DOM.Core.Node;
      begin
         if Type_Name = "any" or Type_Name = "anyType" then
            raise Not_Implemented with
               "Schema contains elements with type 'any' or 'anyType'";
         end if;
         ---Ada.Text_IO.Put_Line ("DEBUG: Begin Type_By_Name. Type=" & Type_Name);

         if Order_Info.Type_To_Children.Contains (Key => Type_Name) then
            return Order_Info.Type_To_Children.Element (Key => Type_Name);
         elsif Simple_XMLTypes.Contains (Item => Type_Name) then
            return Empty_Vector;
         end if;

         Node := Get_Toplevel_Definition (Name_Attr => Type_Name);
         if Name (Node => Node) = "simpleType" then
            return Empty_Vector;
         elsif Name (Node => Node) = "complexType" then
            for Child of Get_Element_Children (Node => Node) loop
               if Name (Node => Child) = "simpleContent" then
                  return Empty_Vector;
               elsif Name (Node => Child) = "complexContent" then
                  return Eval_ComplexContent (Node => Child);
               elsif Elem_Container_Tags.Contains (Name (Node => Child)) then
                  return Eval_Grouptags (Node => Child);
               end if;
            end loop;
            return Empty_Vector;
         else
            raise Validation_Error with
               "Evaluation of type '"
               & Type_Name
               & "' lead to definition node with unexpected name '"
               & Name (Node => Node)
               & "'";
         end if;
      end Eval_Type_By_Name;

      ---------------------------------------------------------------------

      function Get_Attr (Node : DOM.Core.Node; Name : String) return String
      is
      begin
         return DOM.Core.Elements.Get_Attribute (Elem => Node, Name => Name);
      end Get_Attr;

      ---------------------------------------------------------------------

      function Get_Element_Children
         (Node : DOM.Core.Node) return Node_Vector.Vector
      is
         Output : Node_Vector.Vector;
         Child : DOM.Core.Node
            := DOM.Core.Nodes.First_Child (N => Node);
      begin
         while Child /= null loop
            if   DOM.Core.Nodes.Node_Type (N => Child)
               = DOM.Core.Element_Node
            then
               Output.Append (Child);
            end if;
            Child := DOM.Core.Nodes.Next_Sibling (N => Child);
         end loop;

         return Output;
      end Get_Element_Children;

      ---------------------------------------------------------------------

      function Get_Group_Definition (Name_Attr : String) return DOM.Core.Node
      is
      begin
         return Get_Toplevel_Definition (Name_Attr => Name_Attr,
                                         Node_Name => "group");
      end Get_Group_Definition;

      ---------------------------------------------------------------------

      function Get_Toplevel_Definition
         (Name_Attr : String;
          Node_Name : String := "")
         return DOM.Core.Node
      is
         Def_Tags : String_Vector.Vector;
         Child : DOM.Core.Node;
      begin
         if Node_Name /= "" then
            Def_Tags.Append (Node_Name);
         else
            Def_Tags.Append ("simpleType");
            Def_Tags.Append ("complexType");
         end if;

         Child := DOM.Core.Nodes.First_Child (N => Schema_Node);
         while Child /= null loop
            if  DOM.Core.Nodes.Node_Type (N => Child)
               = DOM.Core.Element_Node
               and then Def_Tags.Contains (Name (Child))
               and then Muxml.Utils.Has_Attribute (Node => Child, Attr_Name => "name")
               and then Get_Attr (Node => Child, Name => "name") = Name_Attr
            then
               return Child;
            end if;
            Child := DOM.Core.Nodes.Next_Sibling (N => Child);
         end loop;

         raise Validation_Error with
            "Could not find a node defining the type '"
            & Name_Attr
            & "'";

      end Get_Toplevel_Definition;

      ---------------------------------------------------------------------

      procedure Initialize_Vectors
         (Vec  : out String_Vector.Vector;
          Name :     String)
      is
      begin
         if Name = "simple_types" then
            Vec.Append ("string");
            Vec.Append ("normalizedString");
            Vec.Append ("token");
            Vec.Append ("base64Binary");
            Vec.Append ("hexBinary");
            Vec.Append ("integer");
            Vec.Append ("positiveInteger");
            Vec.Append ("negativeInteger");
            Vec.Append ("nonNegativeInteger");
            Vec.Append ("nonPositiveInteger");
            Vec.Append ("long");
            Vec.Append ("unsignedLong");
            Vec.Append ("int");
            Vec.Append ("unsignedInt");
            Vec.Append ("short");
            Vec.Append ("unsignedShort");
            Vec.Append ("byte");
            Vec.Append ("unsignedByte");
            Vec.Append ("decimal");
            Vec.Append ("float");
            Vec.Append ("double");
            Vec.Append ("boolean");
            Vec.Append ("duration");
            Vec.Append ("dateTime");
            Vec.Append ("date");
            Vec.Append ("time");
            Vec.Append ("gYear");
            Vec.Append ("gYearMonth");
            Vec.Append ("gMonth");
            Vec.Append ("gMonthDay");
            Vec.Append ("gDay");
            Vec.Append ("Name");
            Vec.Append ("QName");
            Vec.Append ("NCName");
            Vec.Append ("anyURI");
            Vec.Append ("language");
            Vec.Append ("ID");
            Vec.Append ("IDREF");
            Vec.Append ("IDREFS");
            Vec.Append ("ENTITY");
            Vec.Append ("ENTITIES");
            Vec.Append ("NOTATION");
            Vec.Append ("NMTOKEN");
            Vec.Append ("NMTOKENS");

         elsif Name = "containers" then
            Vec.Append ("sequence");
            Vec.Append ("choice");
            Vec.Append ("all");
            Vec.Append ("group");

         elsif Name = "element_and_containers" then
            Vec.Append ("element");
            Initialize_Vectors (Vec => Vec, Name => "containers");

         else
            -- Program_Error (nur arcane stuff)
            raise Program_Error with
               "Unknown option " & Name & " for Initialize_Vectors ";
         end if;
      end Initialize_Vectors;

      ---------------------------------------------------------------------

      function Name (Node : DOM.Core.Node) return String
      is
         Node_Name : constant String := DOM.Core.Nodes.Node_Name (N => Node);
      begin
         return Remove_Namespace (Node_Name);
      end Name;

      ---------------------------------------------------------------------

      procedure Pop_From_Backtrace
      is
      begin
         Backtrace.Delete_Last;
      end Pop_From_Backtrace;

      ---------------------------------------------------------------------

      procedure Push_To_Backtrace (Node : DOM.Core.Node)
      is
      begin
         if Node_Vector.Contains (Container => Backtrace,
                                  Item      => Node)
         then
            declare
               Path : Ada.Strings.Unbounded.Unbounded_String;
            begin
               for I in Node_Vector.Find_Index (Container => Backtrace,
                                                Item      => Node)
                  .. Backtrace.Last_Index loop
                  Ada.Strings.Unbounded.Append
                     (Source   => Path,
                      New_Item => DOM.Core.Nodes.Node_Name (Backtrace (I)));
                  if Muxml.Utils.Has_Attribute (Node => Backtrace (I), Attr_Name => "name") then
                     Ada.Strings.Unbounded.Append
                     (Source   => Path,
                      New_Item => " name='"
                      & Get_Attr (Node => Backtrace (I),
                                  Name => "name")
                      & "'");
                  end if;
                  Ada.Strings.Unbounded.Append
                     (Source   => Path,
                      New_Item => " > ");
               end loop;
               Ada.Strings.Unbounded.Append
                  (Source   => Path,
                   New_Item => DOM.Core.Nodes.Node_Name (Node));
               if Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "name") then
                  Ada.Strings.Unbounded.Append
                     (Source   => Path,
                      New_Item => " name='"
                      & Get_Attr (Node => Node,
                                  Name => "name")
                      & "'");
               end if;
               raise Not_Implemented with
                  "Found cyclic inclusion of elements within one type. "
                  & "The cycle is: "
                  & Ada.Strings.Unbounded.To_String (Path);
            end;
         end if;
         Node_Vector.Append (Container => Backtrace,
                             New_Item  => Node);
      end Push_To_Backtrace;

      ---------------------------------------------------------------------

      function Remove_Namespace (Name : String) return String
      is
      begin
         for I in Name'First .. Name'Last - 1 loop
            if Name (I) = ':' then
               return Name (I + 1 .. Name'Last);
            end if;
         end loop;
         return Name;
      end Remove_Namespace;

      ---------------------------------------------------------------------

   begin
      Muxml.Parse_String (Data => Schema,
                          Kind => Muxml.None,
                          XML  => Schema_XML_Data);

      -- find the root-node of the schema (without knowing the namespace)
      Schema_Node := DOM.Core.Nodes.First_Child (N => Schema.Doc);
      while Schema_Node /= null loop
         if  DOM.Core.Nodes.Node_Type (N => Schema_Node)
            = DOM.Core.Element_Node
            and then Name (Node => Schema_Node) = "schema"
         then
            exit;
         end if;
         Schema_Node := DOM.Core.Nodes.Next_Sibling (N => Schema_Node);
      end loop;
      if Schema_Node = null then
         raise Validation_Error with
         "Could not find schema root node";
      end if;

      Initialize_Vectors (Vec => Simple_XMLTypes,
                          Name => "simple_types");
      Initialize_Vectors (Vec => Elem_And_Container_Tags,
                          Name => "element_and_containers");
      Initialize_Vectors (Vec => Elem_Container_Tags,
                          Name => "containers");

      Check_NS_And_Tag_Assumptions
         (Schema_Node => Schema_Node,
          Schema_XML_Data => Schema_XML_Data);

      -- find root 'element'-node and recurse
      declare
         Found : Boolean := False;
      begin
         for Child of Get_Element_Children (Node => Schema_Node) loop
            if Name (Node => Child) = "element" then
               if Found then
                  raise Not_Implemented with
                     "The schema allows two root elements";
               end if;
               Found := True;

               -- insert root element in types dictionary
               Order_Info.Type_To_Children.Insert
                  (Key => "schemaRoot",
                   New_Item => Eval_Element (Node => Child));

               -- insert root element in tags-dictionary
               Order_Info.Tag_To_Type.Insert
                  (Key =>  Order_Info.Type_To_Children
                             ("schemaRoot").Node_Names.First_Element,
                   New_Item => String_Vector.To_Vector
                                  (New_Item => Order_Info.Type_To_Children
                                    ("schemaRoot").Type_Names.First_Element,
                                   Length => 1));

               -- evaluate the root element
               -- assignment before calling Eval is neccessarry
               -- in order to drop the reference to Order_Info which prevents
               -- tampering with Order_Info within Eval
               declare
                  Type_Name : constant String
                     := Order_Info.Type_To_Children
                     ("schemaRoot").Type_Names.First_Element;
               begin
                  Eval_Entrypoint (Type_Name =>  Type_Name);
               end;
            end if;
         end loop;
         if not Found then
            raise Not_Implemented with
               "The schema does not contain an 'element' node "
               & "at root level";
         end if;
      end;

      Check_Unique_Names;

      -- delete elements of Order_Info.tag_to_type with more than one entry
      declare
         Key_To_Del : String_Vector.Vector;
      begin
         for Cursor in Order_Info.Tag_To_Type.Iterate loop
            if String_To_StringVector.Element (Cursor).Length > 1 then
               Key_To_Del.Append
                  (New_Item => String_To_StringVector.Key (Cursor));
            end if;
         end loop;

         for K of Key_To_Del loop
            Order_Info.Tag_To_Type.Delete (K);
         end loop;
      end;

   end Init_Order_Information;

   ------------------------------------------------------------------------

   function To_String (OI : Order_Information) return String
   is
      package ASU renames Ada.Strings.Unbounded;
      use all type String_To_VectorTuple.Cursor;
      use all type String_To_StringVector.Cursor;
      use Ada.Strings.Unbounded;

      function U (S : String) return ASU.Unbounded_String
         renames ASU.To_Unbounded_String;

      Output, Key : ASU.Unbounded_String
         := U ("");
      Newline : constant String
         := "" & Character'Val (10) & Character'Val (13);

      Cursor_VT : String_To_VectorTuple.Cursor
         := OI.Type_To_Children.First;
      Cursor_SV : String_To_StringVector.Cursor
         := OI.Tag_To_Type.First;

   begin
      Output := U ("{" & Newline);

      while Cursor_VT /= String_To_VectorTuple.No_Element loop
         Key := U (String_To_VectorTuple.Key (Cursor_VT));

         Output := Output
            & Key
            & " : "
            & To_String (OI.Type_To_Children (Cursor_VT))
            & Newline;

         String_To_VectorTuple.Next (Cursor_VT);
      end loop;
      Output := Output & "}" & Newline;

      ----

      Output :=  Output & Newline & "{" & Newline;
      while Cursor_SV /= String_To_StringVector.No_Element loop
         Key := U (String_To_StringVector.Key (Cursor_SV));

         Output := Output
            & Key
            & " : "
            & To_String (OI.Tag_To_Type (Cursor_SV))
            & Newline;

         String_To_StringVector.Next (Cursor_SV);
      end loop;
      Output := Output & "}";

      return ASU.To_String (Output);
   end To_String;

   ------------------------------------------------------------------------

   function To_String (VT : Vector_Tuple) return String
   is
      package ASU renames Ada.Strings.Unbounded;
      use Ada.Strings.Unbounded;

      function U (S : String) return ASU.Unbounded_String
         renames ASU.To_Unbounded_String;

      use all type Ada.Containers.Count_Type;

      Output : ASU.Unbounded_String
         := U ("");
      String_Vec1, String_Vec2 : String_Vector.Vector;

   begin
      String_Vec1 := VT.Node_Names;
      String_Vec2 := VT.Type_Names;

      if String_Vec1.Length /= String_Vec2.Length then
         raise Validation_Error with
            "To_String got vector Tuple with unequal length.";
      elsif String_Vec1.Is_Empty then
         return "[]";
      end if;

      for I in String_Vec1.First_Index .. String_Vec1.Last_Index - 1 loop
         Output := Output
            & "["  & String_Vec1 (I)
            & ", " & String_Vec2 (I)
            & "], ";
      end loop;
      Output := Output
         & "["  & String_Vec1.Last_Element
         & ", " & String_Vec2.Last_Element
         & "]";
      return ASU.To_String (Output);
   end To_String;

   ------------------------------------------------------------------------

   function To_String (SV : String_Vector.Vector) return String
   is
      package ASU renames Ada.Strings.Unbounded;
      use Ada.Strings.Unbounded;

      function U (S : String) return ASU.Unbounded_String
         renames ASU.To_Unbounded_String;

      Output : ASU.Unbounded_String
         := U ("");
   begin
      if SV.Is_Empty then
         return "[]";
      end if;

      Output := Output & "[";
      for I in SV.First_Index .. SV.Last_Index - 1 loop
         Output := Output
            & SV (I)
            & ", ";
      end loop;
      Output := Output
         & SV.Last_Element
         & "]";
      return ASU.To_String (Output);
   end To_String;

begin
   Init_Order_Information (Schema_XML_Data => Muxml.system_src_schema.Data);

end  Mutools.Amend.Ordering;
