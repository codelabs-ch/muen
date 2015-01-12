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

with DOM.Core.Append_Node;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Attrs;
with DOM.Core.Documents.Local;

with McKae.XML.XPath.XIA;

package body Muxml.Utils
is

   -------------------------------------------------------------------------

   function Ancestor_Node
     (Node  : DOM.Core.Node;
      Level : Positive)
      return DOM.Core.Node
   is
      use type DOM.Core.Node;

      Result : DOM.Core.Node := Node;
   begin
      for I in 1 .. Level loop
         exit when Result = null;
         Result := DOM.Core.Nodes.Parent_Node (N => Result);
      end loop;

      return Result;
   end Ancestor_Node;

   -------------------------------------------------------------------------

   procedure Append
     (Left  : in out DOM.Core.Node_List;
      Right :        DOM.Core.Node_List)
   is

   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Right) - 1 loop
         DOM.Core.Append_Node
           (List => Left,
            N    => DOM.Core.Nodes.Item
              (List  => Right,
               Index => I));
      end loop;
   end Append;

   -------------------------------------------------------------------------

   procedure Append_Child
     (Node      : DOM.Core.Node;
      New_Child : DOM.Core.Node)
   is
      Dummy : DOM.Core.Node;
      pragma Unreferenced (Dummy);
   begin
      Dummy := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => New_Child);
   end Append_Child;

   -------------------------------------------------------------------------

   function Get_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String)
      return String
   is
      use type DOM.Core.Node;

      Node : constant DOM.Core.Node := Get_Element
        (Doc   => Doc,
         XPath => XPath);
   begin
      if Node = null then
         return "";
      else
         return DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => Name);
      end if;
   end Get_Attribute;

   -------------------------------------------------------------------------

   function Get_Attribute
     (Nodes     : DOM.Core.Node_List;
      Ref_Attr  : String;
      Ref_Value : String;
      Attr_Name : String)
      return String
   is
      use Ada.Strings.Unbounded;
   begin
      return Get_Attribute
        (Nodes     => Nodes,
         Refs      => (1 => (Name  => To_Unbounded_String (Ref_Attr),
                             Value => To_Unbounded_String (Ref_Value))),
         Attr_Name => Attr_Name);
   end Get_Attribute;

   -------------------------------------------------------------------------

   function Get_Attribute
     (Nodes     : DOM.Core.Node_List;
      Refs      : Ref_Attrs_Type;
      Attr_Name : String)
      return String
   is
      use type DOM.Core.Node;

      Node : constant DOM.Core.Node
        := Get_Element (Nodes => Nodes,
                        Refs  => Refs);
   begin
      if Node /= null then
         return DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => Attr_Name);
      else
         return "";
      end if;
   end Get_Attribute;

   -------------------------------------------------------------------------

   function Get_Element
     (Doc   : DOM.Core.Node;
      XPath : String)
      return DOM.Core.Node
   is
      use type DOM.Core.Node;

      Element : DOM.Core.Node := null;
   begin
      if Doc /= null then
         Element := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Doc,
               XPath => XPath),
            Index => 0);
      end if;

      return Element;
   end Get_Element;

   -------------------------------------------------------------------------

   function Get_Element
     (Nodes     : DOM.Core.Node_List;
      Ref_Attr  : String;
      Ref_Value : String)
      return DOM.Core.Node
   is
      use Ada.Strings.Unbounded;
   begin
      return Get_Element
        (Nodes => Nodes,
         Refs  => (1 => (Name  => To_Unbounded_String (Ref_Attr),
                         Value => To_Unbounded_String (Ref_Value))));
   end Get_Element;

   -------------------------------------------------------------------------

   function Get_Element
     (Nodes : DOM.Core.Node_List;
      Refs  : Ref_Attrs_Type)
      return DOM.Core.Node
   is
      use Ada.Strings.Unbounded;

      Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
   begin
      for I in 0 .. Count - 1 loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Match : Boolean;
         begin
            for Ref of Refs loop
               Match := To_String (Ref.Value) = DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => To_String (Ref.Name));

               exit when not Match;
            end loop;

            if Match then
               return Node;
            end if;
         end;
      end loop;

      return null;
   end Get_Element;

   -------------------------------------------------------------------------

   function Get_Element_Value
     (Doc   : DOM.Core.Node;
      XPath : String)
      return String
   is
      use type DOM.Core.Node;

      Node : constant DOM.Core.Node
        := Get_Element
          (Doc   => Doc,
           XPath => XPath & "/text()");
   begin
      if Node = null then
         return "";
      else
         return DOM.Core.Nodes.Node_Value (N => Node);
      end if;
   end Get_Element_Value;

   -------------------------------------------------------------------------

   function Get_Elements
     (Nodes     : DOM.Core.Node_List;
      Ref_Attr  : String;
      Ref_Value : String)
      return DOM.Core.Node_List
   is
      Count  : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
      Result : DOM.Core.Node_List;
   begin
      for I in 0 .. Count - 1 loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
         begin
            if DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => Ref_Attr) = Ref_Value
            then
               DOM.Core.Append_Node (List => Result,
                                     N    => Node);
            end if;
         end;
      end loop;

      return Result;
   end Get_Elements;

   -------------------------------------------------------------------------

   function Get_Matching
     (XML_Data       : XML_Data_Type;
      Left_XPath     : String;
      Right_XPath    : String;
      Match_Multiple : Boolean := False;
      Match          : not null access function
        (Left, Right : DOM.Core.Node) return Boolean)
      return Matching_Pairs_Type
   is
      Left_Nodes  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => Left_XPath);
      Right_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => Right_XPath);
      Result      : Matching_Pairs_Type;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Left_Nodes) - 1 loop
         declare
            Left_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Left_Nodes,
                 Index => I);
            Right_Node : DOM.Core.Node;
         begin
            Find_Match :
            for J in 0 .. DOM.Core.Nodes.Length (List => Right_Nodes) - 1 loop
               Right_Node := DOM.Core.Nodes.Item
                 (List  => Right_Nodes,
                  Index => J);

               if Match
                 (Left  => Left_Node,
                  Right => Right_Node)
               then
                  DOM.Core.Append_Node (List => Result.Left,
                                        N    => Left_Node);
                  DOM.Core.Append_Node (List => Result.Right,
                                        N    => Right_Node);
                  exit Find_Match when not Match_Multiple;
               end if;
            end loop Find_Match;
         end;
      end loop;

      return Result;
   end Get_Matching;

   -------------------------------------------------------------------------

   procedure Merge
     (Left      : DOM.Core.Node;
      Right     : DOM.Core.Node;
      List_Tags : Tags_Type := No_Tags)
   is
      use type DOM.Core.Node;

      Left_Doc : constant DOM.Core.Document
        := DOM.Core.Nodes.Owner_Document (N => Left);

      --  Returns True if the given name matches a list tag.
      function Is_List_Tag (Name : String) return Boolean;

      ----------------------------------------------------------------------

      function Is_List_Tag (Name : String) return Boolean
      is
      begin
         for Tag of List_Tags loop
            if Name = Ada.Strings.Unbounded.To_String (Source => Tag) then
               return True;
            end if;
         end loop;
         return False;
      end Is_List_Tag;
   begin
      if DOM.Core.Nodes.Node_Name (N => Left)
        /= DOM.Core.Nodes.Node_Name (N => Right)
      then
         return;
      end if;

      declare
         R_Child : DOM.Core.Node := DOM.Core.Nodes.First_Child (N => Right);
      begin
         while R_Child /= null loop
            declare
               L_Child : DOM.Core.Node := DOM.Core.Nodes.First_Child
                 (N => Left);
            begin

               --  Find matching children.

               while L_Child /= null and then
                 DOM.Core.Nodes.Node_Name (N => L_Child)
                 /= DOM.Core.Nodes.Node_Name (N => R_Child)
               loop
                  L_Child := DOM.Core.Nodes.Next_Sibling (N => L_Child);
               end loop;

               if L_Child = null
                 or else Is_List_Tag
                   (Name => DOM.Core.Nodes.Node_Name (N => L_Child))
               then

                  --  No match or list found, attach right child incl. all
                  --  children to left.

                  Append_Child
                    (Node      => Left,
                     New_Child => DOM.Core.Documents.Local.Adopt_Node
                       (Doc    => Left_Doc,
                        Source => DOM.Core.Documents.Local.Clone_Node
                          (N    => R_Child,
                           Deep => True)));
               else
                  Merge (Left      => L_Child,
                         Right     => R_Child,
                         List_Tags => List_Tags);
               end if;
            end;

            R_Child := DOM.Core.Nodes.Next_Sibling (N => R_Child);
         end loop;
      end;

      DOM.Core.Nodes.Set_Node_Value
        (N     => Left,
         Value => DOM.Core.Nodes.Node_Value (N => Right));

      declare
         Attrs : constant DOM.Core.Named_Node_Map
           := DOM.Core.Nodes.Attributes (N => Right);
         Node  : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (Map => Attrs) - 1 loop
            Node := DOM.Core.Nodes.Item (Map   => Attrs,
                                         Index => I);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Left,
               Name  => DOM.Core.Attrs.Name (Att => Node),
               Value => DOM.Core.Attrs.Value (Att => Node));
         end loop;
      end;
   end Merge;

   -------------------------------------------------------------------------

   procedure Remove_Child
     (Node       : DOM.Core.Node;
      Child_Name : String)
   is
      Children : constant DOM.Core.Node_List
        := DOM.Core.Nodes.Child_Nodes (N => Node);
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Children) - 1 loop
         declare
            Child : DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Children,
               Index => I);
         begin
            if DOM.Core.Nodes.Node_Name (N => Child) = Child_Name then
               Child := DOM.Core.Nodes.Remove_Child
                 (N         => Node,
                  Old_Child => Child);
               DOM.Core.Nodes.Free (N => Child);
               return;
            end if;
         end;
      end loop;

      raise XML_Error with "Unable to remove child '" & Child_Name
        & "' from node '" & DOM.Core.Nodes.Node_Name (N => Node) & "'";
   end Remove_Child;

   -------------------------------------------------------------------------

   procedure Set_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String;
      Value : String)
   is
      use type DOM.Core.Node;

      Node : constant DOM.Core.Node := Get_Element
        (Doc   => Doc,
         XPath => XPath);
   begin
      if Node = null then
         raise XML_Error with "Unable to set attribute '" & Name & "' to "
           & "value '" & Value & "' - No element found at XPath '" & XPath
           & "'";
      end if;

      DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                       Name  => Name,
                                       Value => Value);
   end Set_Attribute;

end Muxml.Utils;
