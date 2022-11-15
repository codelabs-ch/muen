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

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with Muxml.system_src_schema;
with Muxml.Utils;

with McKae.XML.XPath.XIA;

package body Mutools.Amend.Ordering
is

   -------------------------------------------------------------------------

   --  Return a string representation of a Vector_Tuple as tuples
   --  provided that the entries of VT have the same length.
   function To_String (VT : Vector_Tuple) return String;

   -------------------------------------------------------------------------

   --  Return a string representation of a String_Vector.
   function To_String (SV : String_Vector.Vector) return String;

   -------------------------------------------------------------------------

   function Get_Insert_Index
      (Ancestors : String_Vector.Vector;
       New_Child : String;
       Siblings  : String_Vector.Vector)
      return Insert_Query_Result_Type
   is
      Child_Order : String_Vector.Vector;

      ----------------------------------------------------------------------

      --  Determine the type of the first ancestor
      --  by going back in the Ancestor-list until some ancestor has a
      --  unique type and then trace that type forward.
      function Get_Parent_Type
         (Ancestors : String_Vector.Vector)
         return String;

      ----------------------------------------------------------------------

      function Get_Parent_Type
         (Ancestors : String_Vector.Vector)
         return String
      is
         package ASU renames Ada.Strings.Unbounded;
         function U (S : String) return ASU.Unbounded_String
            renames ASU.To_Unbounded_String;
         function S (U : ASU.Unbounded_String) return String
            renames ASU.To_String;

         Index_Unique, Index : String_Vector.Extended_Index
                             := String_Vector.No_Index;
         Parent_Type         : ASU.Unbounded_String;
         Children            : Vector_Tuple;
      begin
         for I in Ancestors.First_Index .. Ancestors.Last_Index loop
            if Order_Info.Name_To_Type.Contains (Ancestors (I)) then
               Index_Unique := I;
               exit;
            end if;
         end loop;

         if Index_Unique = String_Vector.No_Index then
            raise Insufficient_Information with
               "Cannot determine type of any ancestor.";
         end if;

         Parent_Type := U (Order_Info.Name_To_Type
                           (Ancestors (Index_Unique)).First_Element);

         for I in reverse Ancestors.First_Index .. Index_Unique - 1 loop
            Children := Order_Info.Type_To_Children (S (Parent_Type));
            Index := Children.Node_Names.Find_Index (Ancestors (I));

            if Index = String_Vector.No_Index then
               raise Validation_Error with
                  "Ancestors sequence given to Get_Parent_Type violates schema";
            end if;
            Parent_Type := U (Children.Type_Names (Index));
         end loop;

         return S (Parent_Type);
      end Get_Parent_Type;

      ----------------------------------------------------------------------

   begin
      if Ancestors.Is_Empty then
         --  This is not supported on purpose (to avoid mistakes).
         raise Not_Implemented with
            "Get_Insert_Index called with empty ancestor list.";
      end if;

      --  Get type of the parent.
      if Order_Info.Name_To_Type.Contains (Ancestors.First_Element) then
         Child_Order := Order_Info.Type_To_Children
            (Order_Info.Name_To_Type (Ancestors.First_Element).First_Element)
            .Node_Names;
      else
         begin
            Child_Order := Order_Info.Type_To_Children
               (Get_Parent_Type (Ancestors)).Node_Names;
         exception
            when Insufficient_Information =>
               return No_Unique_Index;
         end;
      end if;

      declare
         --  Get index of the new_child in the child-list of the parent.
         New_Child_Index : constant String_Vector.Extended_Index
                         := Child_Order.Find_Index (Item => New_Child);
         Sibling_Index   : String_Vector.Extended_Index;
      begin
         if New_Child_Index = String_Vector.No_Index then
            return No_Legal_Index;
         end if;

         --  Find index of first sibling which occurs after New_Child in
         --  the child-list of Parent.
         for I in Siblings.First_Index .. Siblings.Last_Index loop
            Sibling_Index := Child_Order.Find_Index (Item => Siblings (I));
            if Sibling_Index = String_Vector.No_Index then
               return No_Legal_Index;
            elsif Sibling_Index > New_Child_Index then
               return Insert_Index (I);
            end if;
         end loop;
      end;

      -- default return value: 'behind the last element'
      return Insert_Index (Siblings.Length);
   end Get_Insert_Index;

   -------------------------------------------------------------------------

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
      Simple_XMLTypes, Elem_And_Container_Names, Elem_Container_Names
                  : String_Vector.Vector;

      --  Backtrace is accessed as a global variable (within
      --  Init_Order_Information).
      --  It stores the evaluation-path and is used for cycle detection
      Backtrace   : Node_Vector.Vector;

      ----------------------------------------------------------------------

      --  Check that there is only one namespace.
      procedure Check_Namespace
         (Schema_Node : DOM.Core.Node;
          Schema_XML_Data : String);

      ----------------------------------------------------------------------

      --  Check that there are no nodes named
      --  "import", "include", " redefine", "example" or "any".
      procedure Check_Forbidden_Element_Names
         (Schema_Node : DOM.Core.Node);

      ----------------------------------------------------------------------

      --  Check that no attribute-node defines a substitution group.
      procedure Check_Substitution_Group
         (Schema_Node : DOM.Core.Node);

      ----------------------------------------------------------------------

      --  Check that the children of each type in Order_Info have unique names.
      procedure Check_Unique_Names;

      ----------------------------------------------------------------------

      --  Delete elements of Order_Info.Name_To_Type with more than one entry.
      procedure Delete_Nonuniqe_From_Name_To_Type;

      ----------------------------------------------------------------------

      --  Recursive evaluation of a node to get a vector with information
      --  about potential children.
      --  The name of the function specifies the type of node it is meant for
      --  (except for "Container_Wrapper", which is a wrapper).
      function Eval_ComplexContent
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_Element
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_Extension
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_Container_Wrapper
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_Group
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_Restriction
         (Node : DOM.Core.Node) return Vector_Tuple;
      function Eval_Sequence_Choice_All
         (Node : DOM.Core.Node) return Vector_Tuple;

      --  Head of recursive evaluation of a fixed type.
      --  Returns finished entry of the given type.
      function Eval_Type_By_Name (Type_Name : String) return Vector_Tuple;

      --  Start recursive evaluation of a new type.
      --  Writes map entries and initiates evaluation of types of children.
      procedure Eval_Entrypoint (Type_Name : String);

      --  Search for a node called "element" within the schema, add it to
      --  The lists and evaluate its type.
      procedure Find_Root_Element_Node_And_Recurse
         (Schema_Node : DOM.Core.Node);

      --  Get the value of attribute Name of Node.
      --  An empty string is returned if the attribute does not exist.
      function Get_Attr (Node : DOM.Core.Node; Name : String) return String
         renames DOM.Core.Elements.Get_Attribute;

      --  Get list of all children which are of type "Element_Node"
      --  (as defined in DOM.Core.Nodes.Node_Type).
      function Get_Element_Children
         (Node : DOM.Core.Node) return Node_Vector.Vector;

      --  Return node defining a group of type "Name_Attr".
      function Get_Group_Definition (Name_Attr : String) return DOM.Core.Node;

      --  Return a node with name "Node_Name" with attribute "name=Name_Attr"
      --  if Node_Name is empty, it is not considered.
      function Get_Toplevel_Definition
         (Name_Attr : String;
          Node_Name : String := "")
         return DOM.Core.Node;

      ----------------------------------------------------------------------

      --  Initialize a Vec with values defined by Name.
      procedure Initialize_Vectors
         (Vec  : out String_Vector.Vector;
          Name :     String);

      ----------------------------------------------------------------------

      --  Shortcut for Node_Name without prefix.
      function Name (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      --  Remove last element from backtrace.
      procedure Pop_From_Backtrace renames Backtrace.Delete_Last;

      ----------------------------------------------------------------------

      --  Add Node to backtrace and raise error if loop is detected.
      procedure Push_To_Backtrace (Node : DOM.Core.Node);

      ----------------------------------------------------------------------

      --  If Name contains ':' return the slice of Name after the first
      --  occurrence. Otherwise, Name is returned.
      function Remove_Namespace (Name : String) return String;

      ----------------------------------------------------------------------

      procedure Check_Forbidden_Element_Names
         (Schema_Node : DOM.Core.Node)
      is
         Bad_Nodes : constant DOM.Core.Node_List
            := McKae.XML.XPath.XIA.XPath_Query
            (N     => Schema_Node,
             XPath => "//*[local-name(.)='import'] | "
             & "//*[local-name(.)='include'] | "
             & "//*[local-name(.)='redefine'] | "
             & "//*[local-name(.)='example'] | "
             & "//*[local-name(.)='any']");
      begin
         -- check forbidden element names
         if  DOM.Core.Nodes.Length (List => Bad_Nodes) > 0 then
            raise Not_Implemented with
               "Schema contains elements named "
               & "'import', 'include', 'redefine', 'example' or 'any'";
         end if;
      end Check_Forbidden_Element_Names;

      ----------------------------------------------------------------------

      --  Check namespaces:
      --  Workaround: The namespace declarations are not attributes of
      --  the root node. It seems impossible to access all declared
      --  namespaces via the DOM-interface. Hence, we do it by hand.
      procedure Check_Namespace
         (Schema_Node : DOM.Core.Node;
          Schema_XML_Data : String)
      is
         Whitespace : constant String
            := ""
            & Ada.Characters.Latin_1.HT     -- horizontal tab
            & Ada.Characters.Latin_1.LF     -- line feed
            & Ada.Characters.Latin_1.CR     -- carriage return
            & Ada.Characters.Latin_1.Space; -- space

         Schema_Start : constant String
            := "<" & DOM.Core.Nodes.Node_Name (Schema_Node);
         Left_Bound : constant Natural
            := Schema_Start'Length +
            Ada.Strings.Fixed.Index (Source  => Schema_XML_Data,
                                     Pattern => Schema_Start,
                                     From    => 1);
         Right_Bound : constant Natural
            := Ada.Strings.Fixed.Index (Source  => Schema_XML_Data,
                                        Pattern => ">",
                                        From    => Left_Bound) - 1;
         Count : Natural := 0;
      begin
         if Left_Bound = Schema_Start'Length or Right_Bound = 0 then
            raise Validation_Error with
               "Coud not find schema-node for namespace-checking";
         end if;
         for I in Whitespace'Range loop
            Count := Count +
               Ada.Strings.Fixed.Count
               (Source  => Schema_XML_Data (Left_Bound .. Right_Bound),
                Pattern => "" & Whitespace (I) & "xmlns");
         end loop;
         if Count > 1 then
            raise Not_Implemented with
               "Schema declares multiple namespaces.";
         end if;
      end Check_Namespace;

      ----------------------------------------------------------------------

      procedure Check_Substitution_Group
         (Schema_Node : DOM.Core.Node)
      is
         Bad_Nodes : constant DOM.Core.Node_List
            := McKae.XML.XPath.XIA.XPath_Query
            (N     => Schema_Node,
             XPath => "//*[local-name(.)='attribute']");
      begin
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
      end Check_Substitution_Group;

      ----------------------------------------------------------------------

      procedure Check_Unique_Names
      is
         Names : String_Vector.Vector;
         Index : String_Vector.Extended_Index;
      begin
         for Cursor in Order_Info.Type_To_Children.Iterate loop
            Names := String_To_Vector_Tuple.Element (Cursor).Node_Names;
            Index := Names.First_Index;
            while Index < Names.Last_Index loop
               if String_Vector.Find_Index
                  (Container => Names,
                   Item      => Names (Index),
                   Index     => Index + 1) /= String_Vector.No_Index
               then
                  raise Not_Implemented with
                     "Node_Names for Key '"
                     & String_To_Vector_Tuple.Key (Cursor)
                     & "' of Order_Info contain entry '"
                     & Names (Index)
                     & "' at least twice. Name must be unique within one type";
               end if;
               Index := Index + 1;
            end loop;
         end loop;
      end Check_Unique_Names;

      ----------------------------------------------------------------------

      procedure Delete_Nonuniqe_From_Name_To_Type
      is
         Key_To_Del : String_Vector.Vector;
      begin
         for Cursor in Order_Info.Name_To_Type.Iterate loop
            if String_To_String_Vector.Element (Cursor).Length > 1 then
               Key_To_Del.Append
                  (New_Item => String_To_String_Vector.Key (Cursor));
            end if;
         end loop;

         for K of Key_To_Del loop
            Order_Info.Name_To_Type.Delete (K);
         end loop;
      end Delete_Nonuniqe_From_Name_To_Type;

      ----------------------------------------------------------------------

      function Eval_ComplexContent
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Empty_Vectors : Vector_Tuple;
      begin
         for Child of Get_Element_Children (Node => Node) loop
            if Name (Node => Child) = "restriction" then
               return Eval_Restriction (Node => Child);
            elsif Name (Node => Child) = "extension" then
               return Eval_Extension (Node => Child);
            end if;
         end loop;

         return Empty_Vectors;
      end Eval_ComplexContent;

      ----------------------------------------------------------------------

      function Eval_Container_Wrapper
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Node_Name : constant String := Name (Node => Node);
      begin
         if Node_Name = "element" then
            return Eval_Element (Node => Node);

         elsif Node_Name = "sequence"
            or Node_Name = "choice"
            or Node_Name = "all"
         then
            return Eval_Sequence_Choice_All (Node => Node);

         elsif Node_Name = "group" then
            return Eval_Group (Node => Node);
         else
            raise Validation_Error with
               "Evaluation of Eval_Container_Wrapper found node with name '"
               & Node_Name
               & "' which is unknown.";
         end if;

      end Eval_Container_Wrapper;

      ----------------------------------------------------------------------

      function Eval_Element
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Output : Vector_Tuple;
      begin
         Push_To_Backtrace (Node => Node);

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
            (New_Item => Remove_Namespace (Get_Attr (Node => Node,
                                                     Name => "type")));
         Pop_From_Backtrace;
         return Output;
      end Eval_Element;

      ----------------------------------------------------------------------

      procedure Eval_Entrypoint (Type_Name : String)
      is
         Types_Vector, Names_Vector : String_Vector.Vector;
         VT : Vector_Tuple;
      begin
         if Order_Info.Type_To_Children.Contains (Key => Type_Name) then
            return;
         end if;

         --  Assign VT to make sure Eval_Type_By_Name is out of scope
         --  when tempering with cursors.
         VT := Eval_Type_By_Name (Type_Name => Type_Name);
         Order_Info.Type_To_Children.Insert
            (Key      => Type_Name,
             New_Item => VT);

         Names_Vector := Order_Info.Type_To_Children.Element
            (Key => Type_Name).Node_Names;
         Types_Vector := Order_Info.Type_To_Children.Element
            (Key => Type_Name).Type_Names;

         for I in Types_Vector.First_Index .. Types_Vector.Last_Index loop
            Eval_Entrypoint (Type_Name => Types_Vector (I));

            if not Order_Info.Name_To_Type.Contains (Key => Names_Vector (I)) then
               Order_Info.Name_To_Type.Insert
                  (Key      => Names_Vector (I),
                   New_Item => String_Vector.To_Vector
                                 (New_Item => Types_Vector (I),
                                  Length   => 1));
            elsif not Order_Info.Name_To_Type (Names_Vector (I)).Contains
               (Item => Types_Vector (I))
            then
               Order_Info.Name_To_Type
                  (Names_Vector (I)).Append (New_Item => Types_Vector (I));
            end if;
         end loop;
      end Eval_Entrypoint;

      ----------------------------------------------------------------------

      function Eval_Extension
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Output, Tuple : Vector_Tuple;
      begin
         if not Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "base") then
            raise Validation_Error with
               "Evaluation of schema found 'extension' "
               & "without 'base' attribute.";
         end if;

         Output := Eval_Type_By_Name (Get_Attr (Node => Node, Name => "base"));
         for Child of Get_Element_Children (Node => Node) loop
            if Elem_Container_Names.Contains (Item => Name (Node => Child)) then
               Tuple := Eval_Container_Wrapper (Node => Child);
               Output.Node_Names.Append (Tuple.Node_Names);
               Output.Type_Names.Append (Tuple.Type_Names);
            end if;
         end loop;

         return Output;
      end Eval_Extension;

      ----------------------------------------------------------------------

      function Eval_Group
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Output, Tuple : Vector_Tuple;
         Def_Node : DOM.Core.Node;
      begin
         Push_To_Backtrace (Node => Node);
         if Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "ref") then
            Def_Node := Get_Group_Definition
               (Name_Attr => Get_Attr (Node => Node,
                                       Name => "ref"));
            Output := Eval_Container_Wrapper (Node => Def_Node);
            Pop_From_Backtrace;
            return Output;

         elsif Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "name") then
            for Child of Get_Element_Children (Node => Node) loop
               if Elem_And_Container_Names.Contains (Item => Name (Node => Child)) then
                  Tuple := Eval_Container_Wrapper (Node => Child);
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

      ----------------------------------------------------------------------

      function Eval_Restriction
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Empty_Tuple : Vector_Tuple;
      begin
         for Child of Get_Element_Children (Node => Node) loop
            if Elem_Container_Names.Contains (Name (Node => Child)) then
               return Eval_Container_Wrapper (Node => Child);
            end if;
         end loop;

         return Empty_Tuple;
      end Eval_Restriction;

      ----------------------------------------------------------------------

      function Eval_Sequence_Choice_All
         (Node : DOM.Core.Node) return Vector_Tuple
      is
         Output, Tuple : Vector_Tuple;
      begin
         Push_To_Backtrace (Node => Node);

         for Child of Get_Element_Children (Node => Node) loop
            if Elem_And_Container_Names.Contains (Item => Name (Node => Child)) then
               Tuple := Eval_Container_Wrapper (Node => Child);
               Output.Node_Names.Append (New_Item => Tuple.Node_Names);
               Output.Type_Names.Append (New_Item => Tuple.Type_Names);
            end if;
         end loop;
         Pop_From_Backtrace;
         return Output;

      end Eval_Sequence_Choice_All;

      ----------------------------------------------------------------------

      function Eval_Type_By_Name (Type_Name : String) return Vector_Tuple
      is
         Empty_Vector : Vector_Tuple;
         Node : DOM.Core.Node;
      begin
         if Type_Name = "any" or Type_Name = "anyType" then
            raise Not_Implemented with
               "Schema contains elements with type 'any' or 'anyType'";
         end if;

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
               elsif Elem_Container_Names.Contains (Name (Node => Child)) then
                  return Eval_Container_Wrapper (Node => Child);
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

      ----------------------------------------------------------------------

      procedure Find_Root_Element_Node_And_Recurse
         (Schema_Node : DOM.Core.Node)
      is
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

               -- insert root element in node-names-dictionary
               Order_Info.Name_To_Type.Insert
                  (Key      => Order_Info.Type_To_Children
                                 ("schemaRoot").Node_Names.First_Element,
                   New_Item => String_Vector.To_Vector
                                 (New_Item => Order_Info.Type_To_Children
                                    ("schemaRoot").Type_Names.First_Element,
                                  Length   => 1));

               --  Evaluate the root element.
               --  Assignment before calling Eval is necessarry
               --  in order to drop the reference to Order_Info which prevents
               --  tampering with Order_Info within Eval.
               declare
                  Type_Name : constant String
                     := Order_Info.Type_To_Children
                     ("schemaRoot").Type_Names.First_Element;
               begin
                  Eval_Entrypoint (Type_Name => Type_Name);
               end;
            end if;
         end loop;
         if not Found then
            raise Not_Implemented with
               "The schema does not contain an 'element' node "
               & "at root level";
         end if;
      end Find_Root_Element_Node_And_Recurse;

      ----------------------------------------------------------------------

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

      ----------------------------------------------------------------------

      function Get_Group_Definition (Name_Attr : String) return DOM.Core.Node
      is
      begin
         return Get_Toplevel_Definition (Name_Attr => Name_Attr,
                                         Node_Name => "group");
      end Get_Group_Definition;

      ----------------------------------------------------------------------

      function Get_Toplevel_Definition
         (Name_Attr : String;
          Node_Name : String := "")
         return DOM.Core.Node
      is
         Def_Names : String_Vector.Vector;
         Child : DOM.Core.Node;
      begin
         if Node_Name /= "" then
            Def_Names.Append (Node_Name);
         else
            Def_Names.Append ("simpleType");
            Def_Names.Append ("complexType");
         end if;

         Child := DOM.Core.Nodes.First_Child (N => Schema_Node);
         while Child /= null loop
            if  DOM.Core.Nodes.Node_Type (N => Child)
               = DOM.Core.Element_Node
               and then Def_Names.Contains (Name (Child))
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

      ----------------------------------------------------------------------

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
            raise Program_Error with
               "Unknown option " & Name & " for Initialize_Vectors ";
         end if;
      end Initialize_Vectors;

      ----------------------------------------------------------------------

      function Name (Node : DOM.Core.Node) return String
      is
         Node_Name : constant String := DOM.Core.Nodes.Node_Name (N => Node);
      begin
         return Remove_Namespace (Node_Name);
      end Name;

      ----------------------------------------------------------------------

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
                      New_Item => "/");
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

      ----------------------------------------------------------------------

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

      ----------------------------------------------------------------------

   begin
      Muxml.Parse_String (Data => Schema,
                          Kind => Muxml.None,
                          XML  => Schema_XML_Data);

      --  Find the root-node of the schema (without knowing the namespace).
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

      Initialize_Vectors (Vec  => Simple_XMLTypes,
                          Name => "simple_types");
      Initialize_Vectors (Vec  => Elem_And_Container_Names,
                          Name => "element_and_containers");
      Initialize_Vectors (Vec  => Elem_Container_Names,
                          Name => "containers");

      --  Check assumption that are easy to check "a priori".
      Check_Namespace (Schema_Node     => Schema_Node,
                       Schema_XML_Data => Schema_XML_Data);
      Check_Forbidden_Element_Names (Schema_Node => Schema_Node);
      Check_Substitution_Group (Schema_Node => Schema_Node);

      --  Main evaluation
      Find_Root_Element_Node_And_Recurse (Schema_Node => Schema_Node);

      --  "a posteriori"-check
      Check_Unique_Names;

      Delete_Nonuniqe_From_Name_To_Type;

   end Init_Order_Information;

   -------------------------------------------------------------------------

   function To_String (OI : Order_Information) return String
   is
      package ASU renames Ada.Strings.Unbounded;
      use all type String_To_Vector_Tuple.Cursor;
      use all type String_To_String_Vector.Cursor;
      use Ada.Strings.Unbounded;

      function U (S : String) return ASU.Unbounded_String
         renames ASU.To_Unbounded_String;

      Output, Key : ASU.Unbounded_String
         := U ("");
      Newline : constant String
         := "" & Ada.Characters.Latin_1.LF;

      Cursor_VT : String_To_Vector_Tuple.Cursor
         := OI.Type_To_Children.First;
      Cursor_SV : String_To_String_Vector.Cursor
         := OI.Name_To_Type.First;

   begin
      --  Image of Type_To_Children
      Output := U ("{" & Newline);

      while Cursor_VT /= String_To_Vector_Tuple.No_Element loop
         Key := U (String_To_Vector_Tuple.Key (Cursor_VT));

         Output := Output
            & Key
            & " : "
            & To_String (OI.Type_To_Children (Cursor_VT))
            & Newline;

         String_To_Vector_Tuple.Next (Cursor_VT);
      end loop;
      Output := Output & "}" & Newline;

      --  Image of Name_To_Type
      Output :=  Output & "{" & Newline;
      while Cursor_SV /= String_To_String_Vector.No_Element loop
         Key := U (String_To_String_Vector.Key (Cursor_SV));

         Output := Output
            & Key
            & " : "
            & To_String (OI.Name_To_Type (Cursor_SV))
            & Newline;

         String_To_String_Vector.Next (Cursor_SV);
      end loop;
      Output := Output & "}";

      return ASU.To_String (Output);
   end To_String;

   -------------------------------------------------------------------------

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

   -------------------------------------------------------------------------

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
