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
with Muxml.Utils;
with Mutools.Utils;
with Mutools.System_Config;

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
               raise Validation_Error with Mutools.Utils.Capitalize (Typename)
                 & " without value attribute in " & Value_Kind
                 & " '" & Name (Node => Val_Node) & "'";
            end if;

            declare
               Dummy : Value_Type;
            begin
               Dummy := Value_Type'Value (Val_Str);
            exception
               when Constraint_Error =>
                  raise Validation_Error with Mutools.Utils.Capitalize
                    (Typename) & " with invalid value '" & Val_Str
                    & "' in " & Value_Kind &  " '"
                    & Name (Node => Val_Node) & "'";
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
               raise Validation_Error with "Conditional without variable "
                 & "attribute";
            end if;

            if not Mutools.System_Config.Has_Value
              (Data => XML_Data,
               Name => Ref_Name)
            then
               raise Validation_Error with "Config variable '" & Ref_Name
                 & "' referenced by conditional not defined";
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
      Config_Vars : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/*/config/*");
      Refs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/*/expressions//variable");
   begin
      for I in Natural range 0 .. DOM.Core.Nodes.Length (List => Refs) - 1 loop
         declare
            use type DOM.Core.Node;

            Ref : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Refs,
                 Index => I);
            Ref_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Ref,
                 Name => "name");
         begin
            if Ref_Name'Length = 0 then
               raise Validation_Error with "Config variable without name "
                 & "attribute in expression '" & Expression_Name (Node => Ref)
                 & "'";
            end if;

            if Muxml.Utils.Get_Element
              (Nodes     => Config_Vars,
               Ref_Attr  => "name",
               Ref_Value => Ref_Name) = null
            then
               raise Validation_Error with "Config variable '" & Ref_Name
                 & "' referenced in expression '"
                 & Expression_Name (Node => Ref) & "' not defined";
            end if;
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

      raise Validation_Error with "Unable to get expression name for '"
        & DOM.Core.Nodes.Node_Name (N => Node) & "'";
   end Expression_Name;

   -------------------------------------------------------------------------

   procedure Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Cfg_Values : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/*/config/*");

      --  Check that names of Left and Right differ.
      procedure Check_Name_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Name_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "name");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         if Left_Name = Right_Name then
            raise Validation_Error with "Multiple config variables with name '"
              & Left_Name & "'";
         end if;
      end Check_Name_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Cfg_Values)'Img & " config variable name(s)");

      Compare_All (Nodes      => Cfg_Values,
                   Comparator => Check_Name_Inequality'Access);
   end Name_Uniqueness;

end Mucfgcheck.Config;
