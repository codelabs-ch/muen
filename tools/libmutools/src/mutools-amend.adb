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

with Mulog;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents.Local;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;

with McKae.XML.XPath.XIA;
with Mutools.Amend.Ordering;
with Ada.Containers.Indefinite_Vectors;

package body Mutools.Amend
is
   procedure Expand
      (XML_Data : Muxml.XML_Data_Type)
   is
      Amend_Statements : constant DOM.Core.Node_List
                       := McKae.XML.XPath.XIA.XPath_Query
                             (N     => XML_Data.Doc,
                              XPath => "//amend");
   begin
      Mulog.Log (Msg => "Expanding "
         & DOM.Core.Nodes.Length (List => Amend_Statements)'Img
         & " amend-statement(s).");
      for I in 0 .. DOM.Core.Nodes.Length (List => Amend_Statements) - 1 loop
         declare
            Amend_Statement : DOM.Core.Node
                            := DOM.Core.Nodes.Item
                                  (List  => Amend_Statements,
                                   Index => I);
            Target_Nodes    : constant DOM.Core.Node_List
                            := McKae.XML.XPath.XIA.XPath_Query
                                  (N     => DOM.Core.Nodes.Parent_Node
                                                   (N => Amend_Statement),
                                   XPath => DOM.Core.Elements.Get_Attribute
                                               (Elem => Amend_Statement,
                                                Name => "xpath"));
            Target_Node     : DOM.Core.Node;
            Amend_Children  : DOM.Core.Node_List;
         begin
            if DOM.Core.Nodes.Length (List => Target_Nodes) /= 1 then
               raise Muxml.Validation_Error with
                  "Found"
                  & Integer'Image (DOM.Core.Nodes.Length (List => Target_Nodes))
                  & " matches (instead of 1) for XPath "
                  & DOM.Core.Elements.Get_Attribute
                       (Elem => Amend_Statement,
                        Name => "xpath")
                  & " in amend statement.";
            end if;

            Target_Node := DOM.Core.Nodes.Item
                              (List  => Target_Nodes,
                               Index => 0);
            Amend_Children := DOM.Core.Nodes.Child_Nodes
                                 (N => Amend_Statement);
            for C in 0 .. DOM.Core.Nodes.Length
                            (List => Amend_Children) - 1 loop
               Recursive_Merge (Parent    => Target_Node,
                                New_Child => DOM.Core.Nodes.Item
                                                (List  => Amend_Children,
                                                 Index => C));
            end loop;

            Amend_Statement := DOM.Core.Nodes.Remove_Child
                                  (N         => DOM.Core.Nodes.Parent_Node
                                                   (N => Amend_Statement),
                                   Old_Child => Amend_Statement);
            DOM.Core.Nodes.Free (N => Amend_Statement);
         end;
      end loop;
   end Expand;

   ------------------------------------------------------------------------

   function Nodes_Equal (L : DOM.Core.Node; R : DOM.Core.Node)
      return Boolean
   is
      use type DOM.Core.Node;
      use all type DOM.Core.Node_Types;

      L_Node_Type : constant DOM.Core.Node_Types
                  := DOM.Core.Nodes.Node_Type (N => L);
      R_Node_Type : constant DOM.Core.Node_Types
                  := DOM.Core.Nodes.Node_Type (N => R);
      L_Node_Name : constant String := DOM.Core.Nodes.Node_Name (N => L);
      R_Node_Name : constant String := DOM.Core.Nodes.Node_Name (N => R);
      L_Attr_List : constant DOM.Core.Named_Node_Map
                  := DOM.Core.Nodes.Attributes (N => L);
      R_Attr_List : constant DOM.Core.Named_Node_Map
                  := DOM.Core.Nodes.Attributes (N => R);
      Length      : constant Natural
                  := DOM.Core.Nodes.Length (Map => L_Attr_List);
   begin
      if    L_Node_Type /= R_Node_Type
         or L_Node_Name /= R_Node_Name
         or Length /= DOM.Core.Nodes.Length (Map => R_Attr_List)
      then
         return False;
      end if;

      for I in 0 .. Length - 1 loop
         declare
            L_Attr : constant DOM.Core.Node
                   := DOM.Core.Nodes.Item
                         (Map => L_Attr_List, Index => I);
            R_Attr : constant DOM.Core.Node
                   := DOM.Core.Nodes.Get_Named_Item
                         (Map  => R_Attr_List,
                          Name => DOM.Core.Nodes.Node_Name (N => L_Attr));
         begin
            if R_Attr = null then
               return False;
            end if;
            if DOM.Core.Nodes.Node_Value (N => L_Attr) /=
               DOM.Core.Nodes.Node_Value (N => R_Attr)
            then
               return False;
            end if;
         end;
      end loop;

      -- for text nodes
      if L_Node_Type = Text_Node then
         return     DOM.Core.Nodes.Node_Value (N => L)
                 =  DOM.Core.Nodes.Node_Value (N => R);
      end if;

      return True;
   end Nodes_Equal;

   ------------------------------------------------------------------------

   procedure Recursive_Merge
      (Parent    : DOM.Core.Node;
       New_Child : DOM.Core.Node)
   is
      use type DOM.Core.Node;
      use all type DOM.Core.Node_Types;
      use all type Mutools.Amend.Ordering.Insert_Index;

      package Node_Vector is new Ada.Containers.Indefinite_Vectors
         (Index_Type   => Natural,
          Element_Type => DOM.Core.Node);

      New_Child_Type : constant DOM.Core.Node_Types
                     := DOM.Core.Nodes.Node_Type (N => New_Child);
      Dummy          : DOM.Core.Node;
      pragma Unreferenced (Dummy);

      ---------------------------------------------------------------------

      -- return a vector with the names of the ancestors of Node,
      -- starting with the name of Node
      function Get_Ancestor_Names (Node : DOM.Core.Node)
               return  Mutools.Amend.Ordering.String_Vector.Vector;

      ---------------------------------------------------------------------

      -- check if the given node has only one child which is a text child
      -- returns 1 if there is exactly 1 child and that is an essential text-node
      -- returns 0 if there is no child or exactly 1 text-node-child which
      --    contains only white-space
      -- returns -1 otherwise
      function Has_Only_Text_Child (Node : DOM.Core.Node) return Integer;

      ---------------------------------------------------------------------

      -- insert element-node New_Child into parent
      -- uses siblings (i.e., children of Parent) to determine insertion position
      procedure Insert
         (Parent         : DOM.Core.Node;
          New_Child      : DOM.Core.Node;
          Siblings_Names : Mutools.Amend.Ordering.String_Vector.Vector;
          Siblings_Nodes : Node_Vector.Vector);

      ---------------------------------------------------------------------

      -- returns true iff the given node is a text-node and its text
      -- contains non-whitespace characters
      function Is_Essential_Textnode (N : DOM.Core.Node) return Boolean;

      ---------------------------------------------------------------------

      -- insert New_Child into Parent
      -- used only if New_Child is not a "special case",
      -- (i.e., covered by Merge_Text_Branch)
      procedure Merge_Regular_Element
         (Parent    : DOM.Core.Node;
          New_Child : DOM.Core.Node);

      ---------------------------------------------------------------------

      -- insert New_Child into Parent
      -- in the case where New_Child is of the form
      -- <tagname ...>text only</tagname>
      procedure Merge_Text_Branch
         (Parent    : DOM.Core.Node;
          New_Child : DOM.Core.Node);

      ---------------------------------------------------------------------

      function Get_Ancestor_Names (Node : DOM.Core.Node)
               return  Mutools.Amend.Ordering.String_Vector.Vector
      is
         Output : Mutools.Amend.Ordering.String_Vector.Vector;
         Parent : DOM.Core.Node
                := DOM.Core.Nodes.Parent_Node (Node);
      begin
         Output.Append (DOM.Core.Nodes.Node_Name (N => Node));
         while Parent /= null loop
            Output.Append (DOM.Core.Nodes.Node_Name (N => Parent));
            Parent := DOM.Core.Nodes.Parent_Node (Parent);
         end loop;
         return Output;
      end Get_Ancestor_Names;

      ---------------------------------------------------------------------

      function Has_Only_Text_Child (Node : DOM.Core.Node) return Integer
      is
         Children : constant DOM.Core.Node_List
                  := DOM.Core.Nodes.Child_Nodes
                       (N => Node);
         Length   : constant Natural
                  :=  DOM.Core.Nodes.Length (List => Children);
         Child    :  DOM.Core.Node;
      begin
         if Length > 1 then
            return -1;
         elsif Length = 0 then
            return 0;
         else
            Child := DOM.Core.Nodes.Item
                       (List  => Children,
                        Index => 0);
            if DOM.Core.Nodes.Node_Type (N => Child) = Text_Node then
               if Is_Essential_Textnode (N => Child) then
                  return 1;
               else
                  return 0;
               end if;
            else
               return -1;
            end if;
         end if;

      end Has_Only_Text_Child;

      ---------------------------------------------------------------------

      procedure Insert
         (Parent         : DOM.Core.Node;
          New_Child      : DOM.Core.Node;
          Siblings_Names : Mutools.Amend.Ordering.String_Vector.Vector;
          Siblings_Nodes : Node_Vector.Vector)
      is
         Ancestors   : constant Mutools.Amend.Ordering.String_Vector.Vector
                      := Get_Ancestor_Names (Node => Parent);
         Query_Result : constant Mutools.Amend.Ordering.Insert_Query_Result_Type
                      := Mutools.Amend.Ordering.Get_Insert_Index
                        (Ancestors => Ancestors,
                         New_Child  => DOM.Core.Nodes.Node_Name (N => New_Child),
                         Siblings   => Siblings_Names);
      begin
         if Query_Result = Mutools.Amend.Ordering.No_Legal_Index then
            raise Muxml.Validation_Error with
               "Could not find valid place to insert '"
               & DOM.Core.Nodes.Node_Name (N => New_Child)
               & "' into node with name '"
               & DOM.Core.Nodes.Node_Name (N => Parent)
               & "'";
         elsif Query_Result = Mutools.Amend.Ordering.No_Unique_Index then
            raise Muxml.Validation_Error with
               "Insufficient information to insert '"
               & DOM.Core.Nodes.Node_Name (N => New_Child)
               & "' into node with name '"
               & DOM.Core.Nodes.Node_Name (N => Parent)
               & "'";
         elsif Query_Result = Mutools.Amend.Ordering.
            Insert_Query_Result_Type (Siblings_Names.Length)
         then
            Dummy := DOM.Core.Nodes.Append_Child
               (N         => Parent,
                New_Child => DOM.Core.Documents.Local.Clone_Node
                               (N    => New_Child,
                                Deep => True));
         else
            Dummy := DOM.Core.Nodes.Insert_Before
               (N         => Parent,
                New_Child => DOM.Core.Documents.Local.Clone_Node
                               (N    => New_Child,
                                Deep => True),
                Ref_Child => Siblings_Nodes (Integer (Query_Result)));
         end if;
      end Insert;

      ---------------------------------------------------------------------

      function Is_Essential_Textnode (N : DOM.Core.Node) return Boolean
      is
         Whitespace : constant Ada.Strings.Maps.Character_Set
                    := Ada.Strings.Maps.To_Set
                         ("" & Character'Val (9)    -- horizontal tab
                          & Character'Val (10)   -- line feed
                          & Character'Val (13)   -- carriage return
                          & Character'Val (32)); -- space

         Node_Type  : constant DOM.Core.Node_Types
                    := DOM.Core.Nodes.Node_Type (N => N);
         Node_Value : constant DOM.Core.DOM_String
                    := DOM.Core.Nodes.Node_Value (N => N);
      begin
         if Node_Type /= Text_Node then
            return False;
         elsif Ada.Strings.Fixed.Trim
               (Source => Node_Value,
                Left   => Whitespace,
                Right  => Whitespace) = ""
         then
            return False;
         else
            return True;
         end if;
      end Is_Essential_Textnode;

      ---------------------------------------------------------------------

      procedure Merge_Regular_Element
         (Parent    : DOM.Core.Node;
          New_Child : DOM.Core.Node)
      is
         --consider all children of parent which are not <amend>
         Parent_Children    : constant DOM.Core.Node_List
                            := McKae.XML.XPath.XIA.XPath_Query
                             (N     => Parent,
                              XPath => "./*[not(self::amend)]");
         New_Child_Children : constant DOM.Core.Node_List
                            := DOM.Core.Nodes.Child_Nodes
                                 (N => New_Child);
         Siblings_Names     : Mutools.Amend.Ordering.String_Vector.Vector;
         Siblings_Nodes     : Node_Vector.Vector;
         Parent_Child       : DOM.Core.Node;

      begin
         -- in all other cases we use the first child of Parent that fits
         -- and append new nodes at the end
         for I in 0 .. DOM.Core.Nodes.Length (List => Parent_Children) - 1 loop
            Parent_Child := DOM.Core.Nodes.Item
               (List  => Parent_Children,
                Index => I);

            if Nodes_Equal (L => Parent_Child, R => New_Child) then
               for J in 0 ..  DOM.Core.Nodes.Length
                  (List => New_Child_Children) - 1 loop
                  declare
                     New_Child_Child : constant DOM.Core.Node
                        := DOM.Core.Nodes.Item
                        (List  => New_Child_Children,
                         Index => J);
                  begin
                        Recursive_Merge (Parent    => Parent_Child,
                                         New_Child => New_Child_Child);
                  end;
               end loop;
               return;
            end if;

            Siblings_Names.Append (DOM.Core.Nodes.Node_Name
                                      (N => Parent_Child));
            Siblings_Nodes.Append (Parent_Child);
         end loop;

         Insert
            (Parent         => Parent,
             New_Child      => New_Child,
             Siblings_Names => Siblings_Names,
             Siblings_Nodes => Siblings_Nodes);

      end Merge_Regular_Element;

      ---------------------------------------------------------------------

      procedure Merge_Text_Branch
         (Parent    : DOM.Core.Node;
          New_Child : DOM.Core.Node)
      is
         --consider all children of parent which are not <amend>
         Parent_Children : constant DOM.Core.Node_List
            := McKae.XML.XPath.XIA.XPath_Query
            (N     => Parent,
             XPath => "./*[not(self::amend)]");
         Parent_Child, Parent_Child_Marked : DOM.Core.Node;
         Siblings_Names  : Mutools.Amend.Ordering.String_Vector.Vector;
         Siblings_Nodes  : Node_Vector.Vector;
      begin
         for I in 0 .. DOM.Core.Nodes.Length
            (List => Parent_Children) - 1 loop
            Parent_Child := DOM.Core.Nodes.Item
               (List  => Parent_Children,
                Index => I);

            if Nodes_Equal (L => Parent_Child, R => New_Child) then
               if Has_Only_Text_Child (Node => Parent_Child) = 1 then
                  if DOM.Core.Nodes.Node_Value
                       (N => DOM.Core.Nodes.First_Child
                               (N => Parent_Child))
                     = DOM.Core.Nodes.Node_Value
                         (N => DOM.Core.Nodes.First_Child
                                 (N => (New_Child)))
                  then
                     return;
                  end if;
               elsif Has_Only_Text_Child (Node => Parent_Child) = 0 then
                  Parent_Child_Marked := Parent_Child;
               end if;
            end if;

            Siblings_Names.Append (DOM.Core.Nodes.Node_Name
                                      (N => Parent_Child));
            Siblings_Nodes.Append (Parent_Child);
         end loop;

         if Parent_Child_Marked /= null then
            declare
               Parent_Child_Child : constant DOM.Core.Node
                  := DOM.Core.Nodes.Item
                       (List  => DOM.Core.Nodes.Child_Nodes
                                   (N => Parent_Child_Marked),
                        Index => 0);
               New_Child_Child : constant DOM.Core.Node
                               := DOM.Core.Nodes.Item
                                    (List  => DOM.Core.Nodes.Child_Nodes
                                                (N => New_Child),
                                     Index => 0);
            begin
               if Parent_Child_Child /= null then
                  DOM.Core.Nodes.Set_Node_Value
                     (N     => Parent_Child_Child,
                      Value => DOM.Core.Nodes.Node_Value (N => New_Child_Child));
                  return;
               else
                  Dummy := DOM.Core.Nodes.Append_Child
                             (N         => Parent_Child_Marked,
                              New_Child => DOM.Core.Documents.Local.Clone_Node
                                             (N    => New_Child_Child,
                                              Deep => True));
                  return;
               end if;
            end;
         else
            Insert
               (Parent         => Parent,
                New_Child      => New_Child,
                Siblings_Names => Siblings_Names,
                Siblings_Nodes => Siblings_Nodes);
         end if;
      end Merge_Text_Branch;

   begin
      if New_Child_Type = Text_Node then
         if Is_Essential_Textnode (N => New_Child) then
            -- how to properly add a text-node to an xml-tree
            --    (possibly after another text-node) needs knowledge about
            --    the meaning and syntax of the text
            --    (whitespace may be important). This is not supported.
            raise Muxml.Validation_Error with
               "Recursive_Merge got text-node with text '"
               & DOM.Core.Nodes.Node_Value (N => New_Child)
               & "'. Cannot process isolated text-nodes. "
               & "Add surrounding element-node.";
         else
            -- text-nodes that contain only whitespace are ignored
            return;
         end if;
      elsif New_Child_Type = Comment_Node then
         return;
      elsif New_Child_Type = Element_Node then
         if Has_Only_Text_Child (Node => New_Child) = 1 then
            Merge_Text_Branch (Parent => Parent, New_Child => New_Child);
         else
            Merge_Regular_Element (Parent => Parent, New_Child => New_Child);
         end if;
      else
         raise Muxml.Validation_Error with
               "Recursive_Merge got node of type '"
               & New_Child_Type'Image
               & "' which is not meant to be handled.";
      end if;
   end Recursive_Merge;

end Mutools.Amend;
