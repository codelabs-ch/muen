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

with Mulog;
with Mutools.Mergers;
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
         Parameters_Node_List : DOM.Core.Node_List;
         Config               : DOM.Core.Node;
      begin

         Assign_Root_And_Config_Node
            (Root_Nodes   => Root_Nodes,
             Root_Node    => Root_Node,
             Config_Nodes => Config_Nodes,
             Config_Node  => Config);

         Parameters_Node_List := McKae.XML.XPath.XIA.XPath_Query
                                    (N     => Params,
                                     XPath =>     ".//boolean "
                                              & "| .//integer "
                                     & "| .//string ");

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
                  := null /= DOM.Core.Nodes.Get_Named_Item
                             (Map  => DOM.Core.Nodes.Attributes (N => Param),
                              Name => "defaultValue");
               Has_Call_Value   : constant Boolean
                  := Param_Name = Muxml.Utils.Get_Attribute
                       (Nodes     => Call_Parameter_List,
                        Ref_Attr  => "name",
                        Ref_Value => Param_Name,
                        Attr_Name => "name");

               New_Config_Node  : DOM.Core.Node;
               Call_Value       : Unbounded_String;

            begin
               if (not Has_Call_Value) and (not Has_Default) then
                  raise Muxml.Validation_Error with
                     "Parameter '"
                     & Param_Name
                     & "' in template '"
                     & DOM.Core.Elements.Get_Attribute
                         (Elem => Template,
                          Name => "name")
                     & "' has not been assigned by call.";
               end if;
               if Has_Call_Value then
                  Call_Value := To_Unbounded_String
                     (Muxml.Utils.Get_Attribute
                      (Nodes     => Call_Parameter_List,
                       Ref_Attr  => "name",
                       Ref_Value => Param_Name,
                       Attr_Name => "value"));
               else
                  Call_Value := To_Unbounded_String (Default_Value);
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

         Params := DOM.Core.Nodes.Remove_Child (N         => Root_Node,
                                                Old_Child => Params);
         DOM.Core.Nodes.Free (N => Params);
      end;

      Prefix_Variables (Root_Node => Root_Node,
                        Prefix    => Prefix);
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
         Mutools.Mergers.Merge_Config_Section
            (Policy     => XML_Data,
             New_Config => Template_Config,
             Clone      => True);

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
      (Root_Node : DOM.Core.Node;
       Prefix    : String)
   is
      use type DOM.Core.Node;

      Defining_Nodes : constant DOM.Core.Node_List
                     := McKae.XML.XPath.XIA.XPath_Query
                          (N     => Root_Node,
                           XPath =>     ".//boolean "
                                    & "| .//integer "
                                    & "| .//string "
                                    & "| .//expression");

      ---------------------------------------------------------------------

      -- add the given Prefix to all "namePrefix" attributes in useTemplate
      -- statements in the subtree with root Parent
      procedure Prefix_NamePrefix
                   (Parent : DOM.Core.Node;
                    Prefix : String);

      ---------------------------------------------------------------------

      procedure Prefix_NamePrefix
                   (Parent : DOM.Core.Node;
                    Prefix : String)
      is
         Use_Nodes : constant DOM.Core.Node_List
                   := McKae.XML.XPath.XIA.XPath_Query
                        (N     => Parent,
                         XPath => ".//useTemplate");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Use_Nodes) - 1 loop
            declare
               Use_Node : constant DOM.Core.Node
                        := DOM.Core.Nodes.Item
                              (List  => Use_Nodes,
                               Index => I);
               Old_Prefix : constant String
                          := DOM.Core.Elements.Get_Attribute
                                (Elem => Use_Node,
                                 Name => "namePrefix");
            begin
               DOM.Core.Elements.Set_Attribute
                  (Elem  => Use_Node,
                   Name  => "namePrefix",
                   Value => Prefix & Old_Prefix);
            end;
         end loop;
      end Prefix_NamePrefix;

      ---------------------------------------------------------------------

      -- rename references of the form "...='$Old_Name'" to "...='$New_Name'"
      procedure Rename_Dollar_Refs
                   (Parent   : DOM.Core.Node;
                    Old_Name : String;
                    New_Name : String);

      ---------------------------------------------------------------------

      procedure Rename_Dollar_Refs
                   (Parent   : DOM.Core.Node;
                    Old_Name : String;
                    New_Name : String)
      is
         Attr_List : constant DOM.Core.Node_List
                   := McKae.XML.XPath.XIA.XPath_Query
                         (N     => Parent,
                          XPath => ".//@*[.='$" & Old_Name & "']");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Attr_List) - 1 loop
            declare
               Attr_Node  : constant DOM.Core.Node
                          := DOM.Core.Nodes.Item
                                (List  => Attr_List,
                                 Index => I);
            begin
               DOM.Core.Attrs.Set_Value
                  (Att   => Attr_Node,
                   Value => "$" & New_Name);
            end;
         end loop;
      end Rename_Dollar_Refs;

      ---------------------------------------------------------------------

      -- rename references within the value of Attr_Name of the nodes
      -- matching XPath which contain values of form
      -- "text_${var1}_text_${var2}_text"
      procedure  Rename_Dollar_Refs_With_Braces
                   (Parent    : DOM.Core.Node;
                    XPath     : String;
                    Attr_Name : String;
                    Old_Name  : String;
                    New_Name  : String);

      ---------------------------------------------------------------------

      procedure  Rename_Dollar_Refs_With_Braces
                   (Parent    : DOM.Core.Node;
                    XPath     : String;
                    Attr_Name : String;
                    Old_Name  : String;
                    New_Name  : String)

      is
         package ASU renames Ada.Strings.Unbounded;
         use all type Mutools.Expressions.Fragment_Type;

         Referencing_Nodes : constant DOM.Core.Node_List
            := McKae.XML.XPath.XIA.XPath_Query
            (N     => Parent,
             XPath => XPath);
      begin
         for I in 0 .. DOM.Core.Nodes.Length
            (List => Referencing_Nodes) - 1 loop
            declare
               Ref_Node : constant DOM.Core.Node
                  := DOM.Core.Nodes.Item
                  (List  => Referencing_Nodes,
                   Index => I);
               Input_String : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem => Ref_Node,
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
                  elsif Fragment.Value.Element = Old_Name then
                     ASU.Append (Source => New_Value,
                                 New_Item => "${" & New_Name & "}");
                  else
                     ASU.Append (Source => New_Value,
                                 New_Item => "${" & Fragment.Value.Element & "}");
                  end if;
               end loop;

               DOM.Core.Elements.Set_Attribute
                  (Elem  => Ref_Node,
                   Name  => Attr_Name,
                   Value => ASU.To_String (New_Value));
            end;
         end loop;
      end Rename_Dollar_Refs_With_Braces;

      ----------------------------------------------------------------------

      -- for each node matching XPath relative to Parent and with attribute
      -- Attr_Name of value Old_Name: set that value to New_Name
      procedure Rename_NonDollar_Refs
                   (Parent    : DOM.Core.Node;
                    XPath     : String;
                    Attr_Name : String;
                    Old_Name  : String;
                    New_Name  : String);

      ---------------------------------------------------------------------

      procedure Rename_NonDollar_Refs
                   (Parent    : DOM.Core.Node;
                    XPath     : String;
                    Attr_Name : String;
                    Old_Name  : String;
                    New_Name  : String)
      is
         Referencing_Nodes : constant DOM.Core.Node_List
             := McKae.XML.XPath.XIA.XPath_Query
                  (N     => Parent,
                   XPath => XPath);
         Ref_Node          : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length
                       (List => Referencing_Nodes) - 1 loop
            Ref_Node := DOM.Core.Nodes.Item
                           (List  => Referencing_Nodes,
                            Index => I);
            if Old_Name = DOM.Core.Elements.Get_Attribute
                             (Elem => Ref_Node,
                              Name => Attr_Name)
            then
               DOM.Core.Elements.Set_Attribute
                  (Elem  => Ref_Node,
                   Name  => Attr_Name,
                   Value => New_Name);
            end if;
         end loop;
      end Rename_NonDollar_Refs;

      ---------------------------------------------------------------------

   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Defining_Nodes) - 1 loop
         declare
            Def_Node  : constant DOM.Core.Node
                      := DOM.Core.Nodes.Item
                            (List  => Defining_Nodes,
                             Index => I);
            Name_Attr : constant DOM.Core.Node
                      := DOM.Core.Nodes.Get_Named_Item
                            (Map  => DOM.Core.Nodes.Attributes (N => Def_Node),
                              Name => "name");

         begin
            -- Get_Named_Item returns NULL if no node exists
            --    (not an empty string) and can therefore distinguish
            --    between ' name="" ' and no name attribute

            -- if Name_Attr = null then this node does not define a variable
            if Name_Attr /= null then
               declare
                  Old_Name : constant String
                           := DOM.Core.Elements.Get_Attribute
                                 (Elem => Def_Node,
                                  Name => "name");
                  New_Name : constant String := Prefix & Old_Name;
               begin
                  DOM.Core.Elements.Set_Attribute
                     (Elem      => Def_Node,
                      Name      => "name",
                      Value     => New_Name);

                  Rename_NonDollar_Refs
                     (Parent    => Root_Node,
                      XPath     => ".//variable",
                      Attr_Name => "name",
                      Old_Name  => Old_Name,
                      New_Name  => New_Name);

                  Rename_NonDollar_Refs
                     (Parent    => Root_Node,
                      XPath     => ".//if",
                      Attr_Name => "variable",
                      Old_Name  => Old_Name,
                      New_Name  => New_Name);

                  Rename_NonDollar_Refs
                     (Parent    => Root_Node,
                      XPath     => ".//case",
                      Attr_Name => "variable",
                      Old_Name  => Old_Name,
                      New_Name  => New_Name);

                  Rename_Dollar_Refs_With_Braces
                     (Parent    => Root_Node,
                      XPath     => ".//evalString",
                      Attr_Name => "value",
                      Old_Name  => Old_Name,
                      New_Name  => New_Name);

                  Rename_Dollar_Refs
                     (Parent    => Root_Node,
                      Old_Name  => Old_Name,
                      New_Name  => New_Name);
               end;
            end if;
         end;
      end loop;

      Prefix_NamePrefix
         (Parent => Root_Node,
          Prefix => Prefix);
   end Prefix_Variables;

end Mutools.XML_Templates;
