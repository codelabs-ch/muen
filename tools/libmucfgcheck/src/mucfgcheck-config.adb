--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
--with Muxml.Utils;
with Mutools.Utils;
with Mutools.System_Config;

with Mucfgcheck.Validation_Errors;
with Ada.Containers.Indefinite_Ordered_Sets;

package body Mucfgcheck.Config
is

   --  Returns the name of the config variable given by the node.
   function Config_Var_Name (Node : DOM.Core.Node) return String;

   --  Returns the name of the expression of which the given node is a part of.
   function Expression_Name (Node : DOM.Core.Node) return String;

   generic
      type Value_Type is (<>);
      Typename   : String;
      XPath      : String;
      Value_Kind : String;
      with function Name (Node : DOM.Core.Node) return String;
   procedure Check_Type_Values (Policy : Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   procedure Check_Type_Values (Policy : Muxml.XML_Data_Type)
   is
      Values : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => XPath & Typename);
   begin
      for I in Natural range 0 .. DOM.Core.Nodes.Length (List => Values) - 1
      loop
         declare
            Val_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Values,
                 Index => I);
            Val_Str  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Val_Node,
                 Name => "value");
         begin
            if Val_Str'Length = 0 then
               Validation_Errors.Insert
                 (Msg => Mutools.Utils.Capitalize (Typename)
                  & " without value attribute in " & Value_Kind
                  & " '" & Name (Node => Val_Node) & "'");
            end if;

            declare
               Dummy : Value_Type;
            begin
               Dummy := Value_Type'Value (Val_Str);
            exception
               when Constraint_Error =>
                  Validation_Errors.Insert
                    (Msg => Mutools.Utils.Capitalize
                       (Typename) & " with invalid value '" & Val_Str
                     & "' in " & Value_Kind &  " '"
                     & Name (Node => Val_Node) & "'");
            end;
         end;
      end loop;
   end Check_Type_Values;

   -------------------------------------------------------------------------

   procedure Conditional_Config_Var_Refs (XML_Data : Muxml.XML_Data_Type)
   is
      Refs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/*//if");
   begin
      for I in Natural range 0 .. DOM.Core.Nodes.Length (List => Refs) - 1 loop
         declare
            Ref : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Refs,
                 Index => I);
            Ref_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Ref,
                 Name => "variable");
         begin
            if Ref_Name'Length = 0 then
               Validation_Errors.Insert
                 (Msg => "Conditional without variable attribute");
            end if;

            if not Mutools.System_Config.Has_Value
              (Data => XML_Data,
               Name => Ref_Name)
            then
               Validation_Errors.Insert
                 (Msg => "Config variable '" & Ref_Name
                  & "' referenced by conditional not defined");
            end if;
         end;
      end loop;
   end Conditional_Config_Var_Refs;

   -------------------------------------------------------------------------

   function Config_Var_Name (Node : DOM.Core.Node) return String
   is
   begin
      return DOM.Core.Elements.Get_Attribute (Elem => Node,
                                              Name => "name");
   end Config_Var_Name;

   -------------------------------------------------------------------------

   procedure Check_Config_Boolean_Values is new Check_Type_Values
     (Value_Type => Boolean,
      Typename   => "boolean",
      XPath      => "/*/config/",
      Value_Kind => "config variable",
      Name       => Config_Var_Name);

   procedure Config_Boolean_Values
     (XML_Data : Muxml.XML_Data_Type) renames Check_Config_Boolean_Values;

   -------------------------------------------------------------------------

   procedure Check_Config_Integer_Values is new Check_Type_Values
     (Value_Type => Integer,
      Typename   => "integer",
      XPath      => "/*/config/",
      Value_Kind => "config variable",
      Name       => Config_Var_Name);

   procedure Config_Integer_Values
     (XML_Data : Muxml.XML_Data_Type) renames Check_Config_Integer_Values;

   -------------------------------------------------------------------------

   procedure Check_Boolean_Values is new Check_Type_Values
     (Value_Type => Boolean,
      Typename   => "boolean",
      XPath      => "/*/expressions//",
      Value_Kind => "expression",
      Name       => Expression_Name);

   procedure Expression_Boolean_Values
     (XML_Data : Muxml.XML_Data_Type) renames Check_Boolean_Values;

   -------------------------------------------------------------------------

   procedure Check_Integer_Values is new Check_Type_Values
     (Value_Type => Integer,
      Typename   => "integer",
      XPath      => "/*/expressions//",
      Value_Kind => "expression",
      Name       => Expression_Name);

   procedure Expression_Integer_Values
     (XML_Data : Muxml.XML_Data_Type) renames Check_Integer_Values;

   -------------------------------------------------------------------------

   procedure Expression_Config_Var_Refs (XML_Data : Muxml.XML_Data_Type)
   is
      package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets
         (Element_Type => String);
      use String_Sets;

      Config_Vars : constant DOM.Core.Node_List
                  := McKae.XML.XPath.XIA.XPath_Query
                        (N     => XML_Data.Doc,
                         XPath => "/*/config/*");
      Exprs       : constant DOM.Core.Node_List
                  := McKae.XML.XPath.XIA.XPath_Query
                        (N     => XML_Data.Doc,
                         XPath => "/*/expressions/expression");
      Known_Names : Set;

   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Config_Vars) - 1 loop
         declare
            Config_Var : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                       (List  => Config_Vars,
                        Index => I);
         begin
            -- duplicate names will be detected in the uniquess-tests
            -- but may be present at this stage.
            -- Include avoids exceptions in these cases
            Known_Names.Include (DOM.Core.Elements.Get_Attribute
                                   (Elem => Config_Var,
                                    Name => "name"));
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Exprs) - 1 loop
         declare
            use type DOM.Core.Node;

            Expr      : constant DOM.Core.Node
                      := DOM.Core.Nodes.Item
                            (List  => Exprs,
                             Index => I);
            Expr_Name  : constant String
                      := DOM.Core.Elements.Get_Attribute
                            (Elem => Expr,
                             Name => "name");
            Expr_Vars : constant DOM.Core.Node_List
                      := McKae.XML.XPath.XIA.XPath_Query
                        (N     => Expr,
                         XPath => ".//variable");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Expr_Vars) - 1 loop
               declare
                  Var      : constant DOM.Core.Node
                           := DOM.Core.Nodes.Item
                                 (List  => Expr_Vars,
                                  Index => J);
                  Var_Name : constant String
                           := DOM.Core.Elements.Get_Attribute
                                 (Elem => Var,
                                  Name => "name");
               begin
                  if Var_Name'Length = 0 then
                     Validation_Errors.Insert
                        (Msg => "Variable without name attribute in expression '"
                         & Expr_Name
                         & "'");
                  elsif not Known_Names.Contains (Var_Name) then
                     Validation_Errors.Insert
                        (Msg => "Variable '"
                         & Var_Name
                         & "' referenced in expression '"
                         & Expr_Name
                         & "' not defined");
                  end if;
               end;
            end loop;
            Known_Names.Include (Expr_Name);
         end;
      end loop;
   end Expression_Config_Var_Refs;

   -------------------------------------------------------------------------

   function Expression_Name (Node : DOM.Core.Node) return String
   is
      use type DOM.Core.Node;

      Cur_Node : DOM.Core.Node;
   begin
      Cur_Node := Node;

      loop
         if DOM.Core.Nodes.Node_Name (N => Cur_Node) = "expression" then
            return DOM.Core.Elements.Get_Attribute
              (Elem => Cur_Node,
               Name => "name");
         end if;

         Cur_Node := DOM.Core.Nodes.Parent_Node (N => Cur_Node);
         exit when Cur_Node = null;
      end loop;

      raise Validation_Errors.Validation_Error with "Unable to get expression "
        & "name for '" & DOM.Core.Nodes.Node_Name (N => Node) & "'";
   end Expression_Name;

   -------------------------------------------------------------------------

   procedure Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Cfg_Expr_Values : constant DOM.Core.Node_List
         := McKae.XML.XPath.XIA.XPath_Query
               (N     => XML_Data.Doc,
                XPath => "/*/config/* | /*/expressions/expression");
   begin
      Mulog.Log (Msg => "Checking uniqueness of"
                 & DOM.Core.Nodes.Length (List =>  Cfg_Expr_Values)'Img
                 & " config and expression name(s)");
      Mucfgcheck.Attr_Uniqueness
         (Nodes     => Cfg_Expr_Values,
          Attr_Name => "name",
          Error_Msg => "The names given to config variables and expressions "
                        & "are not unique.");
   end Name_Uniqueness;

end Mucfgcheck.Config;
