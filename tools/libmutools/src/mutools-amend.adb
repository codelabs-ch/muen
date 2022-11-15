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

with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Containers.Indefinite_Vectors;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents.Local;

with McKae.XML.XPath.XIA;

with Mulog;
with Mutools.Amend.Ordering;
with Mutools.Xmldebuglog;

package body Mutools.Amend
is
   --  Merge New_Child into Parent such that parts of the child-tree
   --  that already exist in Parent are not duplicated.
   --  In detail: for each child C of Parent:
   --   Check if tag-name and all attributes of C and New_Child are equal
   --     If yes: recursive function call on C and on each Child of New_Child
   --     If no: goto next child C of Parent
   --   If no child matched:
   --     append New_Child to children of Parent and return (with deep cloning)
   procedure Recursive_Merge
      (Parent       : DOM.Core.Node;
       New_Child    : DOM.Core.Node;
       Debug_Active : Boolean);

   -------------------------------------------------------------------------

   procedure Expand
      (XML_Data    : Muxml.XML_Data_Type;
      Debug_Active : Boolean := False)
   is
      Amend_Statements : constant DOM.Core.Node_List
                       := McKae.XML.XPath.XIA.XPath_Query
                             (N     => XML_Data.Doc,
                              XPath => "//amend");
   begin
      Mulog.Log (Msg => "Expanding "
         & DOM.Core.Nodes.Length (List => Amend_Statements)'Img
         & " amend-statements");
      for I in 0 .. DOM.Core.Nodes.Length (List => Amend_Statements) - 1 loop
         declare
            Amend_Statement : DOM.Core.Node
                            := DOM.Core.Nodes.Item
                                  (List  => Amend_Statements,
                                   Index => I);
            Target_Nodes    : DOM.Core.Node_List;
            Target_Node     : DOM.Core.Node;
            Amend_Children  : DOM.Core.Node_List;
         begin
            begin
               Target_Nodes := McKae.XML.XPath.XIA.XPath_Query
                  (N     => DOM.Core.Nodes.Parent_Node
                      (N => Amend_Statement),
                   XPath => DOM.Core.Elements.Get_Attribute
                      (Elem => Amend_Statement,
                       Name => "xpath"));
            exception
               when Error : others =>
                  if Debug_Active then
                     Mulog.Log (Msg => "Error when evaluating xpath. "
                                   & Mutools.Xmldebuglog.Get_Log_For_Error_Message
                                   (Node => Amend_Statement));
                  end if;
                  Ada.Exceptions.Raise_Exception
                     (E => Ada.Exceptions.Exception_Identity (X => Error),
                      Message => "XIA was evaluating XPath """
                         & DOM.Core.Elements.Get_Attribute
                         (Elem => Amend_Statement,
                          Name => "xpath")
                         & """ when reporting: "
                         &  Ada.Exceptions.Exception_Message (X => Error));
            end;
            if DOM.Core.Nodes.Length (List => Target_Nodes) /= 1 then
               if Debug_Active then
                     Mulog.Log (Msg => "Error when evaluating amend. "
                                   & Mutools.Xmldebuglog.Get_Log_For_Error_Message
                                   (Node => Amend_Statement));
               end if;

               raise Muxml.Validation_Error with
                  "Found"
                  & Integer'Image (DOM.Core.Nodes.Length (List => Target_Nodes))
                  & " matches (instead of 1) for XPath """
                  & DOM.Core.Elements.Get_Attribute
                       (Elem => Amend_Statement,
                        Name => "xpath")
                  & """ in amend statement.";
            end if;

            if Debug_Active then
               --  Before the nodes leave their place, we have push the
               --  backtrace-info to the leafs of the subtree (because we need
               --  their ancestors for that).
               declare
                  Log_Index : Mutools.Xmldebuglog.Transaction_Log_Index_Type;
               begin
                  Log_Index := Mutools.Xmldebuglog.Add_Amend_Transaction
                     (Amend_Node => Amend_Statement,
                      Xpath      => DOM.Core.Elements.Get_Attribute
                         (Elem => Amend_Statement,
                          Name => "xpath"));
                  -- add log with dummy inheritance
                  Mutools.Xmldebuglog.Add_Log_For_Node
                     (Node      => Amend_Statement,
                      Ancestor  => Amend_Statement,
                      TA_Number => Log_Index);

                  Mutools.Xmldebuglog.Gather_Backtrace_Info
                     (Node                => Amend_Statement,
                      Examine_Only_Parent => False,
                      Deep                => True);
               end;
            end if;

            Target_Node := DOM.Core.Nodes.Item
               (List  => Target_Nodes,
                Index => 0);
            Amend_Children := DOM.Core.Nodes.Child_Nodes
               (N => Amend_Statement);

            for C in 0 .. DOM.Core.Nodes.Length
               (List => Amend_Children) - 1 loop
               begin
                  Recursive_Merge (Parent    => Target_Node,
                                   New_Child => DOM.Core.Nodes.Item
                                      (List  => Amend_Children,
                                       Index => C),
                                   Debug_Active => Debug_Active);
               exception
                  when others =>
                     if Debug_Active then
                        Mulog.Log (Msg => Mutools.Xmldebuglog.Get_Log_For_Error_Message
                                      (Node => Amend_Statement));
                     end if;
                     raise;
               end;
            end loop;

            if Debug_Active then
               Mutools.Xmldebuglog.Remove_Log_Of_Subtree (Node => Amend_Statement);
            end if;

            Amend_Statement := DOM.Core.Nodes.Remove_Child
               (N         => DOM.Core.Nodes.Parent_Node
                   (N => Amend_Statement),
                Old_Child => Amend_Statement);
            DOM.Core.Nodes.Free (N => Amend_Statement);
         end;
      end loop;
   end Expand;

   -------------------------------------------------------------------------

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

   -------------------------------------------------------------------------

   procedure Recursive_Merge
      (Parent       : DOM.Core.Node;
       New_Child    : DOM.Core.Node;
       Debug_Active : Boolean)
   is
      use type DOM.Core.Node;
      use all type DOM.Core.Node_Types;
      use all type Mutools.Amend.Ordering.Insert_Index;

      type Text_Child_Status_Type is
         (Unique_Essential, None_Or_Not_Essential, Many_Or_Other_Type);

      package Node_Vector is new Ada.Containers.Indefinite_Vectors
         (Index_Type   => Natural,
          Element_Type => DOM.Core.Node);

      New_Child_Type : constant DOM.Core.Node_Types
                     := DOM.Core.Nodes.Node_Type (N => New_Child);
      Dummy          : DOM.Core.Node;
      pragma Unreferenced (Dummy);

      ----------------------------------------------------------------------

      --  Return a vector with the names of the ancestors of Node,
      --  starting with the name of Node.
      function Get_Ancestor_Names (Node : DOM.Core.Node)
               return  Mutools.Amend.Ordering.String_Vector.Vector;

      ----------------------------------------------------------------------

      --  Check if the given node has only one child which is a text child.
      --  Returns "Unique_Essential" if there is exactly 1 child and that is an
      --  essential text-node.
      --  Returns "None_Or_Not_Essential" if there is no child or exactly 1
      --  text-node-child which contains only white-space.
      --  Returns "Many_Or_Other_Type" otherwise.
      function Has_Only_Text_Child (Node : DOM.Core.Node)
                                   return Text_Child_Status_Type;

      ----------------------------------------------------------------------

      --  Insert element-node New_Child into parent.
      --  Uses siblings (i.e., children of Parent) to determine insertion
      --  position.
      procedure Insert
         (Parent         : DOM.Core.Node;
          New_Child      : DOM.Core.Node;
          Siblings_Names : Mutools.Amend.Ordering.String_Vector.Vector;
          Siblings_Nodes : Node_Vector.Vector);

      ----------------------------------------------------------------------

      --  Returns true if and only if the given node is a text-node and its text
      --  contains non-whitespace characters.
      function Is_Essential_Textnode (N : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      --  Insert New_Child into Parent.
      --  Used only if New_Child is not a "special case",
      --  (i.e., covered by Merge_Text_Branch).
      procedure Merge_Regular_Element
         (Parent    : DOM.Core.Node;
          New_Child : DOM.Core.Node);

      ----------------------------------------------------------------------

      --  Insert New_Child into Parent in the case where New_Child is of the
      --  form   <tagname ...>text only</tagname>
      procedure Merge_Text_Branch
         (Parent    : DOM.Core.Node;
          New_Child : DOM.Core.Node);

      ----------------------------------------------------------------------

      --  Get a string representation of Node, excluding its children.
      function Node_To_String (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

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

      ----------------------------------------------------------------------

      function Has_Only_Text_Child (Node : DOM.Core.Node)
                                   return Text_Child_Status_Type
      is
         Children : constant DOM.Core.Node_List
                  := DOM.Core.Nodes.Child_Nodes
                       (N => Node);
         Length   : constant Natural
                  :=  DOM.Core.Nodes.Length (List => Children);
         Child    :  DOM.Core.Node;
      begin
         if Length > 1 then
            return Many_Or_Other_Type;
         elsif Length = 0 then
            return None_Or_Not_Essential;
         else
            Child := DOM.Core.Nodes.Item
                       (List  => Children,
                        Index => 0);
            if DOM.Core.Nodes.Node_Type (N => Child) = Text_Node then
               if Is_Essential_Textnode (N => Child) then
                  return Unique_Essential;
               else
                  return None_Or_Not_Essential;
               end if;
            else
               return Many_Or_Other_Type;
            end if;
         end if;
      end Has_Only_Text_Child;

      ----------------------------------------------------------------------

      procedure Insert
         (Parent         : DOM.Core.Node;
          New_Child      : DOM.Core.Node;
          Siblings_Names : Mutools.Amend.Ordering.String_Vector.Vector;
          Siblings_Nodes : Node_Vector.Vector)
      is
         Ancestors    : constant Mutools.String_Vector.Vector
                      := Get_Ancestor_Names (Node => Parent);
         Query_Result : constant Mutools.Amend.Ordering.Insert_Query_Result_Type
                      := Mutools.Amend.Ordering.Get_Insert_Index
                        (Ancestors => Ancestors,
                         New_Child => DOM.Core.Nodes.Node_Name (N => New_Child),
                         Siblings  => Siblings_Names);
         Copy_Of_New_Child, Ref_Child : DOM.Core.Node;
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
            --  This triggers Insert_Before to append the new child at the end.
            Ref_Child := null;
         else
            Ref_Child := Siblings_Nodes (Integer (Query_Result));
         end if;

         Copy_Of_New_Child := DOM.Core.Nodes.Insert_Before
            (N         => Parent,
             New_Child => DOM.Core.Documents.Local.Clone_Node
                (N    => New_Child,
                 Deep => True),
             Ref_Child => Ref_Child);

         if Debug_Active then
            Mutools.Xmldebuglog.Copy_Log_Entry
               (Old_Node => New_Child,
                New_Node => Copy_Of_New_Child,
                Deep     => True);
         end if;

      end Insert;

      ----------------------------------------------------------------------

      function Is_Essential_Textnode (N : DOM.Core.Node) return Boolean
      is
         Whitespace : constant Ada.Strings.Maps.Character_Set
                    := Ada.Strings.Maps.To_Set
                         ("" & Character'Val (9) -- horizontal tab
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

      ----------------------------------------------------------------------

      procedure Merge_Regular_Element
         (Parent    : DOM.Core.Node;
          New_Child : DOM.Core.Node)
      is
         Siblings_Names : Mutools.Amend.Ordering.String_Vector.Vector;
         Siblings_Nodes : Node_Vector.Vector;
         Parent_Child   : DOM.Core.Node;
      begin

         Parent_Child := DOM.Core.Nodes.First_Child (N => Parent);
         while Parent_Child /= null loop
            if DOM.Core.Nodes.Node_Type (N => Parent_Child) = Element_Node
               and then DOM.Core.Nodes.Node_Name (N => Parent_Child) /= "amend"
            then
               if Nodes_Equal (L => Parent_Child, R => New_Child) then
                  if Debug_Active then
                     Mulog.Log (Msg => "Info: Amend skipped insertion of '"
                                   & Mutools.Xmldebuglog.Get_Xpath
                                   (Node => Parent)
                                   & "/"
                                   & Node_To_String (New_Child)
                                   & "'");
                  end if;

                  declare
                     New_Child_Child : DOM.Core.Node
                        := DOM.Core.Nodes.First_Child (N => New_Child);
                  begin
                     while New_Child_Child /= null loop
                        Recursive_Merge (Parent       => Parent_Child,
                                         New_Child    => New_Child_Child,
                                         Debug_Active => Debug_Active);

                        New_Child_Child := DOM.Core.Nodes.Next_Sibling
                           (N => New_Child_Child);
                     end loop;
                  end;

                  return;
               end if;

               Siblings_Names.Append (DOM.Core.Nodes.Node_Name
                                         (N => Parent_Child));
               Siblings_Nodes.Append (Parent_Child);
            end if;

            Parent_Child := DOM.Core.Nodes.Next_Sibling (N => Parent_Child);
         end loop;

         --  As the 'return' above was not reached
         --  none of the children of Parent matches.
         Insert
            (Parent         => Parent,
             New_Child      => New_Child,
             Siblings_Names => Siblings_Names,
             Siblings_Nodes => Siblings_Nodes);

      end Merge_Regular_Element;

      ----------------------------------------------------------------------

      procedure Merge_Text_Branch
         (Parent    : DOM.Core.Node;
          New_Child : DOM.Core.Node)
      is
         Parent_Child, Parent_Child_Marked : DOM.Core.Node;
         Siblings_Names : Mutools.Amend.Ordering.String_Vector.Vector;
         Siblings_Nodes : Node_Vector.Vector;
      begin

         Parent_Child := DOM.Core.Nodes.First_Child (N => Parent);
         while Parent_Child /= null loop
            if DOM.Core.Nodes.Node_Type (N => Parent_Child) = Element_Node
               and then DOM.Core.Nodes.Node_Name (N => Parent_Child) /= "amend"
            then

               if Nodes_Equal (L => Parent_Child, R => New_Child) then
                  if Has_Only_Text_Child (Node => Parent_Child) = Unique_Essential
                  then
                     if DOM.Core.Nodes.Node_Value
                        (N => DOM.Core.Nodes.First_Child
                            (N => Parent_Child))
                        = DOM.Core.Nodes.Node_Value
                        (N => DOM.Core.Nodes.First_Child
                            (N => (New_Child)))
                     then
                        return;
                     end if;
                  elsif Has_Only_Text_Child (Node => Parent_Child)
                     = None_Or_Not_Essential
                  then
                     Parent_Child_Marked := Parent_Child;
                  end if;
               end if;

               Siblings_Names.Append (DOM.Core.Nodes.Node_Name
                                         (N => Parent_Child));
               Siblings_Nodes.Append (Parent_Child);
            end if;
            Parent_Child := DOM.Core.Nodes.Next_Sibling (N => Parent_Child);
         end loop;

         --  There exists a fitting node in parent in which we insert the text
         --  from new child.
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

               else
                  Dummy := DOM.Core.Nodes.Append_Child
                             (N         => Parent_Child_Marked,
                              New_Child => DOM.Core.Documents.Local.Clone_Node
                                             (N    => New_Child_Child,
                                              Deep => True));
               end if;

               if Debug_Active then
                  Mutools.Xmldebuglog.Copy_Log_Entry
                     (Old_Node => New_Child,
                      New_Node => Parent_Child_Marked,
                      Deep     => False);
               end if;

               return;

            end;
         else
            Insert
               (Parent         => Parent,
                New_Child      => New_Child,
                Siblings_Names => Siblings_Names,
                Siblings_Nodes => Siblings_Nodes);
         end if;
      end Merge_Text_Branch;

      ----------------------------------------------------------------------

      function Node_To_String (Node : DOM.Core.Node) return String
      is
         package ASU renames Ada.Strings.Unbounded;

         Attr_List : constant DOM.Core.Named_Node_Map
            := DOM.Core.Nodes.Attributes (N => Node);
         Node_Rep  : ASU.Unbounded_String;
         Attr      : DOM.Core.Node;

      begin
         Node_Rep := ASU.To_Unbounded_String
            ("<" & DOM.Core.Nodes.Node_Name (New_Child));

         for I in 0 .. DOM.Core.Nodes.Length (Map => Attr_List) - 1 loop
            Attr := DOM.Core.Nodes.Item (Map => Attr_List, Index => I);
            ASU.Append
               (Source   => Node_Rep,
                New_Item => " "
                   & DOM.Core.Nodes.Node_Name (N => Attr)
                   & "="""
                   & DOM.Core.Nodes.Node_Value (N => Attr)
                   & """");
         end loop;

         ASU.Append (Source => Node_Rep,
                     New_Item => ">");

         return ASU.To_String (Node_Rep);
      end Node_To_String;

   begin
      if New_Child_Type = Text_Node then
         if Is_Essential_Textnode (N => New_Child) then
            --  How to properly add a text-node to an xml-tree
            --    (possibly after another text-node) needs knowledge about
            --    the meaning and syntax of the text
            --    (whitespace may be important). This is not supported.
            raise Muxml.Validation_Error with
               "Recursive_Merge got text-node with text '"
               & DOM.Core.Nodes.Node_Value (N => New_Child)
               & "'. Cannot process isolated text-nodes. "
               & "Please add surrounding element-node.";
         else
            --  Text-nodes that contain only whitespace are ignored.
            return;
         end if;
      elsif New_Child_Type = Comment_Node then
         return;
      elsif New_Child_Type = Element_Node then
         if Has_Only_Text_Child (Node => New_Child) = Unique_Essential then
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
