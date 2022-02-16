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
use Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Ordered_Sets;

with Mulog;
with Mutools.Expressions;
with Muxml.Utils;
with McKae.XML.XPath.XIA;

with DOM.Core.Nodes;
with DOM.Core.Attrs;
with DOM.Core.Elements;
with DOM.Core.Documents;
with DOM.Core.Documents.Local;

package body Mutools.XML_Templates
is

   procedure Adopt_All_Children
      (Target             : DOM.Core.Node;
       Parent_Of_Children : DOM.Core.Node;
       Append_Mode        : Boolean := False)
   is
      use type DOM.Core.Node;

      Child     :  DOM.Core.Node
                := DOM.Core.Nodes.First_Child (N => Parent_Of_Children);
      New_Child :  DOM.Core.Node;

   begin
      while Child /= null loop
         New_Child := DOM.Core.Documents.Local.Adopt_Node
            (Doc    => DOM.Core.Nodes.Owner_Document (N => Target),
             Source => DOM.Core.Documents.Local.Clone_Node
                          (N    => Child,
                           Deep => True));
         if Append_Mode then
            New_Child := DOM.Core.Nodes.Append_Child
               (N         => Target,
                New_Child => New_Child);
         else
            New_Child := DOM.Core.Nodes.Insert_Before
               (N         => DOM.Core.Nodes.Parent_Node (N => Target),
                New_Child => New_Child,
                Ref_Child => Target);
         end if;

         Child := DOM.Core.Nodes.Next_Sibling (N => Child);
      end loop;
   end Adopt_All_Children;

   ------------------------------------------------------------------------

   procedure Compile_Template
      (Template       :     DOM.Core.Node;
       Template_Call  :     DOM.Core.Node;
       Running_Number :     Positive;
       Output         : out Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Trimmed_Number : constant String
                     := Ada.Strings.Fixed.Trim
                          (Source => Integer'Image (Running_Number),
                           Side   => Ada.Strings.Left);
      Prefix         : constant String
                     := DOM.Core.Elements.Get_Attribute
                          (Elem => Template_Call,
                           Name => "namePrefix")
                        & "t"
                        & Trimmed_Number
                        & "_";
      Root_Node      : DOM.Core.Node;

      ---------------------------------------------------------------------

      -- assign Root_Node and Config_Node
      -- create Config_Node if necessary
      procedure Assign_Root_And_Config_Node
         (Root_Nodes   :     DOM.Core.Node_List;
          Root_Node    : out DOM.Core.Node;
          Config_Nodes :     DOM.Core.Node_List;
          Config_Node  : out DOM.Core.Node);

      ---------------------------------------------------------------------

      procedure Assign_Root_And_Config_Node
         (Root_Nodes   :     DOM.Core.Node_List;
          Root_Node    : out DOM.Core.Node;
          Config_Nodes :     DOM.Core.Node_List;
          Config_Node  : out DOM.Core.Node)
      is
      begin
         case DOM.Core.Nodes.Length (List => Root_Nodes) is
            when 1 =>
               Root_Node := DOM.Core.Nodes.Item
                               (List  => Root_Nodes,
                                Index => 0);
            when others =>
               raise Muxml.Validation_Error with
                  "No unique template node in new template document.";
         end case;

         case DOM.Core.Nodes.Length (List => Config_Nodes) is
            when 0 =>
               Config_Node := DOM.Core.Documents.Create_Element
                            (Doc      => Output.Doc,
                             Tag_Name => "config");

               Config_Node := DOM.Core.Nodes.Insert_Before
                            (N         => Root_Node,
                             New_Child => Config_Node,
                             Ref_Child => DOM.Core.Nodes.First_Child
                                             (N => Root_Node));
            when 1 =>
               Config_Node := DOM.Core.Nodes.Item (List  => Config_Nodes,
                                               Index => 0);
            when others =>
               raise Muxml.Validation_Error with
                  "Found template with multiple 'config'-nodes.";
         end case;
      end Assign_Root_And_Config_Node;

   begin
      Create_XMLDocument_From_Node (New_Doc  => Output.Doc,
                                    Src_Node => Template);
      declare
         Root_Nodes           : constant DOM.Core.Node_List
                              := McKae.XML.XPath.XIA.XPath_Query
                                    (N     => Output.Doc,
                                     XPath => "/template");
         Config_Nodes        : constant DOM.Core.Node_List
                              := McKae.XML.XPath.XIA.XPath_Query
                                    (N     => Output.Doc,
                                     XPath => "/template/config");
         Call_Parameter_List  : constant DOM.Core.Node_List
                              := McKae.XML.XPath.XIA.XPath_Query
                                    (N     => Template_Call,
                                     XPath => ".//parameter");
         Params               : DOM.Core.Node
                              := Muxml.Utils.Get_Element
                                    (Doc   => Output.Doc,
                                     XPath => "/template/parameters");
         Parameters_Node_List : constant DOM.Core.Node_List
                              := McKae.XML.XPath.XIA.XPath_Query
                                    (N     => Params,
                                     XPath =>     ".//boolean "
                                              & "| .//integer "
                                              & "| .//string ");
         Config               : DOM.Core.Node;
         Matching_Call_Param_Names : Mutools.Expressions.String_Vector.Vector;
      begin

         Assign_Root_And_Config_Node
            (Root_Nodes   => Root_Nodes,
             Root_Node    => Root_Node,
             Config_Nodes => Config_Nodes,
             Config_Node  => Config);

         for I in 0 .. DOM.Core.Nodes.Length
                          (List => Parameters_Node_List) - 1  loop
            declare
               Param         : constant DOM.Core.Node
                             := DOM.Core.Nodes.Item
                                   (List  => Parameters_Node_List,
                                    Index => I);
               Param_Name    : constant String
                             := DOM.Core.Elements.Get_Attribute
                                   (Elem => Param,
                                    Name => "name");
               Default_Value : constant String
                             := DOM.Core.Elements.Get_Attribute
                                   (Elem => Param,
                                    Name => "defaultValue");
               Has_Default      : constant Boolean
                  := Muxml.Utils.Has_Attribute
                       (Node      => Param,
                        Attr_Name => "defaultValue");

               Matching_Call_Params : constant DOM.Core.Node_List
                  := Muxml.Utils.Get_Elements
                       (Nodes     =>  Call_Parameter_List,
                        Ref_Attr  => "name",
                        Ref_Value => Param_Name);

               New_Config_Node : DOM.Core.Node;
               Call_Value      : Unbounded_String;
            begin
               if DOM.Core.Nodes.Length (List => Matching_Call_Params) > 1
                  or (not Has_Default and DOM.Core.Nodes.Length
                         (List => Matching_Call_Params) = 0)
               then
                  raise Muxml.Validation_Error with
                     "Parameter '"
                     & Param_Name
                     & "' in template '"
                     & DOM.Core.Elements.Get_Attribute
                     (Elem => Template,
                      Name => "name")
                     & "' has not been assigned by call.";
               elsif DOM.Core.Nodes.Length (List => Matching_Call_Params) = 0 then
                  Call_Value := To_Unbounded_String (Default_Value);
               else
                  declare
                     Call_Node : constant DOM.Core.Node
                        := DOM.Core.Nodes.Item
                        (List  => Matching_Call_Params,
                         Index => 0);
                  begin
                     if not Muxml.Utils.Has_Attribute
                        (Node      => Call_Node,
                         Attr_Name => "value")
                     then
                        raise Muxml.Validation_Error with
                           "Parameter '"
                           & Param_Name
                           & "' in call of template '"
                           & DOM.Core.Elements.Get_Attribute
                           (Elem => Template,
                            Name => "name")
                           & "' has no value attribute.";
                     else
                        Call_Value := To_Unbounded_String
                           (DOM.Core.Elements.Get_Attribute
                               (Elem => Call_Node,
                                Name => "value"));

                        Mutools.Expressions.String_Vector.Append
                           (Container => Matching_Call_Param_Names,
                            New_Item => Param_Name);
                     end if;
                  end;
               end if;

               New_Config_Node := DOM.Core.Nodes.Append_Child
                  (N         => Config,
                   New_Child => DOM.Core.Nodes.Clone_Node
                                   (N    => Param,
                                    Deep => True));
               DOM.Core.Elements.Set_Attribute
                  (Elem  => New_Config_Node,
                   Name  => "value",
                   Value => To_String (Call_Value));
               DOM.Core.Elements.Remove_Attribute
                  (Elem  => New_Config_Node,
                   Name  => "defaultValue");
            end;
         end loop;

         -- check if all set parameters were used
         for I in 0 .. DOM.Core.Nodes.Length
            (List => Call_Parameter_List) - 1  loop
            declare
               Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                         (List  => Call_Parameter_List,
                          Index => I);
               Node_Name  : constant String
                          := DOM.Core.Elements.Get_Attribute
                                   (Elem => Node,
                                    Name => "name");
            begin
               if not Mutools.Expressions.String_Vector.Contains
                  (Container => Matching_Call_Param_Names,
                   Item      => Node_Name)
               then
                  raise Muxml.Validation_Error with
                     "Parameter '"
                     & Node_Name
                     & "' in call of template '"
                     & DOM.Core.Elements.Get_Attribute
                     (Elem => Template,
                      Name => "name")
                     & "' is unused";
               end if;
            end;
         end loop;

         Params := DOM.Core.Nodes.Remove_Child (N         => Root_Node,
                                                Old_Child => Params);
         DOM.Core.Nodes.Free (N => Params);

         Prefix_Variables (Root_Node   => Root_Node,
                           Config_Node => Config,
                           Prefix      => Prefix);
      end;
   end Compile_Template;

   -------------------------------------------------------------------------

   procedure Create_XMLDocument_From_Node
      (New_Doc  : out DOM.Core.Document;
       Src_Node :     DOM.Core.Node)
   is
      Implementation : DOM.Core.DOM_Implementation;
      New_Child      : DOM.Core.Node;
   begin
      New_Doc   := DOM.Core.Create_Document (Implementation);
      New_Child := DOM.Core.Documents.Local.Adopt_Node
                 (Doc    => New_Doc,
                  Source => DOM.Core.Documents.Local.Clone_Node
                               (N    => Src_Node,
                                Deep => True));
      New_Child := DOM.Core.Nodes.Append_Child (N         => New_Doc,
                                                New_Child => New_Child);
      pragma Unreferenced (New_Child);
   end Create_XMLDocument_From_Node;

   -------------------------------------------------------------------------

   procedure Expand (XML_Data : in out Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Max_Rounds     : constant Positive := 100;
      Running_Number :          Positive := 1;
      Rounds         :          Positive := 1;
      Templates      : constant DOM.Core.Node_List
                     := McKae.XML.XPath.XIA.XPath_Query
                           (N     => XML_Data.Doc,
                            XPath => "//template");

      ---------------------------------------------------------------------

      -- given the processed template, substitute the call-node with the
      -- call contents
      procedure Substitute_Template_Call
         (Compiled_Template :        Muxml.XML_Data_Type;
          XML_Data          : in out Muxml.XML_Data_Type;
          Template_Call     : in out DOM.Core.Node);

      ---------------------------------------------------------------------

      procedure Substitute_Template_Call
         (Compiled_Template :        Muxml.XML_Data_Type;
          XML_Data          : in out Muxml.XML_Data_Type;
          Template_Call     : in out DOM.Core.Node)
      is
         Template_Config  : constant DOM.Core.Node
            := Muxml.Utils.Get_Element
            (Doc   => Compiled_Template.Doc,
             XPath => "/template/config");
         Template_Expressions : constant DOM.Core.Node
            := Muxml.Utils.Get_Element
            (Doc   => Compiled_Template.Doc,
             XPath => "/template/expressions");
         Template_Body : constant DOM.Core.Node
            := Muxml.Utils.Get_Element
            (Doc   => Compiled_Template.Doc,
             XPath => "/template/body");

         System_Config : constant DOM.Core.Node
            := Muxml.Utils.Get_Element
            (Doc   => XML_Data.Doc,
             XPath => "/system/config");
         System_Expressions : DOM.Core.Node
            := Muxml.Utils.Get_Element
            (Doc   => XML_Data.Doc,
             XPath => "/system/expressions");
         System_Root : constant DOM.Core.Node
            := Muxml.Utils.Get_Element
            (Doc   => XML_Data.Doc,
             XPath => "/system");
      begin
         Adopt_All_Children
            (Target             => System_Config,
             Parent_Of_Children => Template_Config,
             Append_Mode        => True);

         if  Template_Expressions /= null then
            if System_Expressions = null then
               System_Expressions := DOM.Core.Documents.Create_Element
                  (Doc      => XML_Data.Doc,
                   Tag_Name => "expressions");
               if System_Config = null then
                  System_Expressions := DOM.Core.Nodes.Insert_Before
                     (N         => System_Root,
                      New_Child => System_Expressions,
                      Ref_Child => DOM.Core.Nodes.First_Child
                      (N => System_Root));
               else
                  System_Expressions := DOM.Core.Nodes.Insert_Before
                     (N         => System_Root,
                      New_Child => System_Expressions,
                      Ref_Child => DOM.Core.Nodes.Next_Sibling
                      (N => System_Config));
               end if;
            end if;

            Adopt_All_Children
               (Target => System_Expressions,
                Parent_Of_Children => Template_Expressions,
                Append_Mode => True);
         end if;

         Adopt_All_Children
            (Target             => Template_Call,
             Parent_Of_Children => Template_Body,
             Append_Mode        => False);

         -- remove the Template_Call from main document
         Template_Call := DOM.Core.Nodes.Remove_Child
            (N         => DOM.Core.Nodes.Parent_Node
             (N => Template_Call),
             Old_Child => Template_Call);
         DOM.Core.Nodes.Free (N => Template_Call);
      end Substitute_Template_Call;

   begin
      if DOM.Core.Nodes.Length (List => Templates) = 0
      then
         Mulog.Log (Msg => "No template definition found.");
      else
         Mulog.Log (Msg => "Found " &
            Integer'Image (DOM.Core.Nodes.Length (List => Templates)) &
            " template definition(s).");
      end if;

      -- remove templates from the tree but do not free the nodes
      for I in 0 .. DOM.Core.Nodes.Length (List => Templates) - 1 loop
         declare
            Node : DOM.Core.Node
                 := DOM.Core.Nodes.Item
                      (List  => Templates,
                       Index => I);
         begin
            Node := DOM.Core.Nodes.Remove_Child
                      (N         => DOM.Core.Nodes.Parent_Node (N => Node),
                       Old_Child => Node);
            pragma Unreferenced (Node);
         end;
      end loop;

      for Round in 1 .. Max_Rounds loop
         -- store value for use outside of loop
         Rounds := Round;

         declare
            Template_Calls : constant DOM.Core.Node_List
                           := McKae.XML.XPath.XIA.XPath_Query
                                 (N     => XML_Data.Doc,
                                  XPath => "//useTemplate");
         begin
            if DOM.Core.Nodes.Length (List => Template_Calls) = 0 then
               exit;
            end if;

            for I in 0 .. DOM.Core.Nodes.Length
                             (List => Template_Calls) - 1 loop
               declare
                  Compiled_Template : Muxml.XML_Data_Type;
                  Template_Call     : DOM.Core.Node
                                    := DOM.Core.Nodes.Item
                                          (List  => Template_Calls,
                                           Index => I);
                  Called_Template_Name : constant String
                                       := DOM.Core.Elements.Get_Attribute
                                             (Elem => Template_Call,
                                              Name => "name");
                  Template : constant DOM.Core.Node
                           :=  Muxml.Utils.Get_Element
                                  (Nodes     => Templates,
                                   Ref_Attr  => "name",
                                   Ref_Value => Called_Template_Name);
               begin
                  if Template = null then
                     raise Muxml.Validation_Error with
                        "Could not find a template with name: '"
                        & Called_Template_Name
                        & "'";
                  end if;

                  Compile_Template
                     (Template       => Template,
                      Template_Call  => Template_Call,
                      Running_Number => Running_Number,
                      Output         => Compiled_Template);
                  Running_Number := Running_Number + 1;

                  Substitute_Template_Call
                     (Compiled_Template => Compiled_Template,
                      XML_Data          => XML_Data,
                      Template_Call     => Template_Call);
               end;
            end loop;
         end;
      end loop;

      -- free memory of template definitions
      declare
         Dummy : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Templates) - 1 loop
            Dummy := DOM.Core.Nodes.Item (List  => Templates,
                                          Index => I);
            DOM.Core.Nodes.Free (N => Dummy);
         end loop;
      end;

      if Rounds > Max_Rounds then
         raise Muxml.Validation_Error with
            "Nesting-depth of templates greater than"
            & Integer'Image (Max_Rounds)
            & ". This may be due to cyclic template-inclusions.";
      end if;
   end Expand;

   ------------------------------------------------------------------------

   procedure Prefix_Variables
      (Root_Node   : DOM.Core.Node;
       Config_Node : DOM.Core.Node;
       Prefix      : String)
   is
      use type DOM.Core.Node;
      use all type DOM.Core.Node_Types;

      -- used to store defined names
      package String_Set_Type is new Ada.Containers.Indefinite_Ordered_Sets
         (Element_Type => String);

      Expressions_Node : constant DOM.Core.Node
         := Muxml.Utils.Get_Unique_Element_Child
         (Parent    => Root_Node,
          Child_Name => "expressions");

      Current_Node  : DOM.Core.Node;
      Defined_Names : String_Set_Type.Set;

      ---------------------------------------------------------------------
      -- go through the child-nodes of Parent and put the value of their
      -- "name" attribute in the set of known names ("Names")
      -- furthermore, these values are changed in the children
      -- by prefixing them with the given string
      procedure Gather_Names_And_Rename
         (Parent :     DOM.Core.Node;
          Prefix :     String;
          Names  : out String_Set_Type.Set);

      ---------------------------------------------------------------------

      procedure Gather_Names_And_Rename
         (Parent :     DOM.Core.Node;
          Prefix :     String;
          Names  : out String_Set_Type.Set)
      is
         Node : DOM.Core.Node
            := DOM.Core.Nodes.First_Child (N => Parent);
         Name_Attribute : constant String := "name";
      begin
         while Node /= null loop
            if DOM.Core.Nodes.Node_Type (N => Node) =  Element_Node
               and then Muxml.Utils.Has_Attribute
               (Node      => Node,
                Attr_Name => Name_Attribute)
            then
               Names.Include (DOM.Core.Elements.Get_Attribute
                                 (Elem => Node,
                                  Name => Name_Attribute));
               DOM.Core.Elements.Set_Attribute
                  (Elem => Node,
                   Name => Name_Attribute,
                   Value => Prefix
                      & DOM.Core.Elements.Get_Attribute
                      (Elem => Node,
                       Name => Name_Attribute));
            end if;
            Node := DOM.Core.Nodes.Next_Sibling (N => Node);
         end loop;
      end Gather_Names_And_Rename;

      ---------------------------------------------------------------------

      -- traverses the xml-tree depth-first
      -- null is returned when there is no next node
      function Next_Node (Current_Node : DOM.Core.Node) return DOM.Core.Node;

      ---------------------------------------------------------------------

      function Next_Node (Current_Node : DOM.Core.Node) return DOM.Core.Node
      is
         ------------------------------------------------------------------

         -- recursively go to the parent until some parent has a next sibling
         -- and return that sibling
         -- returns null if there is no such node
         function Backtrack_Until_Sibling
            (Node : DOM.Core.Node)
            return DOM.Core.Node;

         ------------------------------------------------------------------

         function Backtrack_Until_Sibling
            (Node : DOM.Core.Node)
            return DOM.Core.Node
         is
         begin
            if DOM.Core.Nodes.Next_Sibling (N => Node) /= null then
               return DOM.Core.Nodes.Next_Sibling (N => Node);
            elsif DOM.Core.Nodes.Parent_Node (N => Node) /= null then
               return Backtrack_Until_Sibling
                  (Node =>  DOM.Core.Nodes.Parent_Node (N => Node));
            else
               return null;
            end if;
         end Backtrack_Until_Sibling;

      begin
         if Current_Node /= null then
            if DOM.Core.Nodes.Has_Child_Nodes (N => Current_Node) then
               return DOM.Core.Nodes.First_Child (N => Current_Node);
            elsif DOM.Core.Nodes.Next_Sibling (N => Current_Node) /= null then
               return DOM.Core.Nodes.Next_Sibling (N => Current_Node);
            elsif DOM.Core.Nodes.Parent_Node (N => Current_Node) /= null then
               return Backtrack_Until_Sibling
                  (Node =>  DOM.Core.Nodes.Parent_Node
                      (N => Current_Node));
            else
               return null;
            end if;
         else
            return Current_Node;
         end if;
      end Next_Node;

      ---------------------------------------------------------------------

      -- rename references of the form "...='$Old_Name'"
      procedure Prefix_Dollar_Refs
         (Node        : DOM.Core.Node;
          Prefix      : String;
          Known_Names : String_Set_Type.Set);

      ---------------------------------------------------------------------

      procedure Prefix_Dollar_Refs
         (Node        : DOM.Core.Node;
          Prefix      : String;
          Known_Names : String_Set_Type.Set)
      is
         Attr_Nodes : constant DOM.Core.Named_Node_Map
            := DOM.Core.Nodes.Attributes (N => Node);

      begin
         for I in 0 .. DOM.Core.Nodes.Length (Map => Attr_Nodes) - 1 loop
            declare
               Attr : constant DOM.Core.Node
                  :=  DOM.Core.Nodes.Item (Map => Attr_Nodes, Index => I);
               Attr_Value : constant String
                  := DOM.Core.Attrs.Value (Att => Attr);
            begin
               if Attr_Value'Length > 0 and then
                  Attr_Value (Attr_Value'First) = '$' and then
                  Known_Names.Contains (Item => Attr_Value
                                           (Attr_Value'First + 1 .. Attr_Value'Last))
               then
                  DOM.Core.Attrs.Set_Value
                     (Att => Attr,
                      Value => "$" & Prefix & Attr_Value
                         (Attr_Value'First + 1 .. Attr_Value'Last));
               end if;
            end;
         end loop;
      end  Prefix_Dollar_Refs;

      ---------------------------------------------------------------------

      -- prefix references within the value attribute of "evalString" nodes
      -- these are of the form "foo${var_name}bar${var2}${var3}"
      procedure Prefix_EvalString
         (Node       : DOM.Core.Node;
          Prefix     : String;
          Known_Names : String_Set_Type.Set);

      ---------------------------------------------------------------------

      procedure Prefix_EvalString
         (Node       : DOM.Core.Node;
          Prefix     : String;
          Known_Names : String_Set_Type.Set)
      is
         package ASU renames Ada.Strings.Unbounded;
         use all type Mutools.Expressions.Fragment_Type;

         Input_String : constant String
            := DOM.Core.Elements.Get_Attribute
            (Elem => Node,
             Name => "value");
         Parsed_Fragments : constant Mutools.Expressions.Fragment_Vector.Vector
            := Mutools.Expressions.Parse_Dollar_Braced_References
            (Input_String => Input_String);
         New_Value : ASU.Unbounded_String;
      begin
         for Fragment of Parsed_Fragments loop
            if Fragment.Value_Type = Mutools.Expressions.Text_Type then
               ASU.Append (Source => New_Value,
                           New_Item => Fragment.Value.Element);
            elsif Known_Names.Contains (Item => Fragment.Value.Element) then
               ASU.Append (Source => New_Value,
                           New_Item => "${" & Prefix & Fragment.Value.Element & "}");
            else
               ASU.Append (Source => New_Value,
                           New_Item => "${" & Fragment.Value.Element & "}");
            end if;
         end loop;

         DOM.Core.Elements.Set_Attribute
            (Elem  => Node,
             Name  => "value",
             Value => ASU.To_String (New_Value));
      end Prefix_EvalString;

      ---------------------------------------------------------------------

      -- add the given Prefix to all "namePrefix" attributes in useTemplate
      procedure Prefix_NamePrefix
         (Node        : DOM.Core.Node;
          Prefix      : String);

      ---------------------------------------------------------------------

      procedure Prefix_NamePrefix
         (Node        : DOM.Core.Node;
          Prefix      : String)
      is
         Old_Name : constant String
            := DOM.Core.Elements.Get_Attribute (Elem => Node, Name => "namePrefix");
      begin
         DOM.Core.Elements.Set_Attribute
            (Elem => Node,
             Name => "namePrefix",
             Value => Prefix & Old_Name);
      end Prefix_NamePrefix;

      ----------------------------------------------------------------------

      -- prefix the value if the attribute with the given name
      procedure Prefix_NonDollar_Reference
         (Node        : DOM.Core.Node;
          Attr_Name   : String;
          Prefix      : String;
          Known_Names : String_Set_Type.Set);

      ---------------------------------------------------------------------

      procedure Prefix_NonDollar_Reference
         (Node        : DOM.Core.Node;
          Attr_Name   : String;
          Prefix      : String;
          Known_Names : String_Set_Type.Set)

      is
         Attr_Value : constant String
            := DOM.Core.Elements.Get_Attribute
            (Elem => Node,
             Name => Attr_Name);
      begin
         if Known_Names.Contains (Item => Attr_Value) then
            DOM.Core.Elements.Set_Attribute
               (Elem  => Node,
                Name  => Attr_Name,
                Value => Prefix & Attr_Value);
         end if;
      end Prefix_NonDollar_Reference;

      ---------------------------------------------------------------------

      -- rename references of the form "...='$Some_Name'"
      procedure Prefix_Text_Node
         (Node        : DOM.Core.Node;
          Prefix      : String;
          Known_Names : String_Set_Type.Set);

      ---------------------------------------------------------------------

      procedure Prefix_Text_Node
         (Node        : DOM.Core.Node;
          Prefix      : String;
          Known_Names : String_Set_Type.Set)
      is
         Text_Value : constant String
            := DOM.Core.Nodes.Node_Value (N => Node);

      begin
         if Text_Value'Length > 0 and then
            Text_Value (Text_Value'First) = '$' and then
            Known_Names.Contains (Item =>  Text_Value
                                     (Text_Value'First + 1 .. Text_Value'Last))
         then
            DOM.Core.Nodes.Set_Node_Value
               (N     => Node,
                Value => "$"
                   & Prefix
                   & Text_Value (Text_Value'First + 1 .. Text_Value'Last));
         end if;
      end Prefix_Text_Node;

   begin
      -- fill Defined_Names and rename the visited definitions
      Gather_Names_And_Rename
         (Parent => Config_Node,
          Prefix => Prefix,
          Names  => Defined_Names);

      if Expressions_Node /= null then
         Gather_Names_And_Rename
            (Parent => Expressions_Node,
             Prefix => Prefix,
             Names  => Defined_Names);
      end if;

      Current_Node := Root_Node;
      while Current_Node /= null loop
         declare
            Node_Name : constant String
               := DOM.Core.Nodes.Node_Name (N => Current_Node);
         begin
            if Node_Name = "if" or
               Node_Name = "case"
            then
               Prefix_NonDollar_Reference
                  (Node        => Current_Node,
                   Attr_Name   => "variable",
                   Prefix      => Prefix,
                   Known_Names => Defined_Names);

            elsif Node_Name = "variable" then
               Prefix_NonDollar_Reference
                  (Node        => Current_Node,
                   Attr_Name   => "name",
                   Prefix      => Prefix,
                   Known_Names => Defined_Names);

            elsif Node_Name = "evalString" then
               Prefix_EvalString
                  (Node        => Current_Node,
                   Prefix      => Prefix,
                   Known_Names => Defined_Names);

            elsif Node_Name = "useTemplate" then
               Prefix_NamePrefix
                  (Node        => Current_Node,
                   Prefix      => Prefix);

            else
               if DOM.Core.Nodes.Node_Type (N => Current_Node) =  Text_Node then
                  Prefix_Text_Node
                     (Node        => Current_Node,
                      Prefix      => Prefix,
                      Known_Names => Defined_Names);
               else
                  Prefix_Dollar_Refs
                     (Node        => Current_Node,
                      Prefix      => Prefix,
                      Known_Names => Defined_Names);
               end if;
            end if;
         end;
         Current_Node := Next_Node (Current_Node => Current_Node);
      end loop;
   end Prefix_Variables;
end Mutools.XML_Templates;
