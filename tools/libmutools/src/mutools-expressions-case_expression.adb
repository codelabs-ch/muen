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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;
with Mulog;
with Mutools.System_Config;
with Muxml.Utils;

package body Mutools.Expressions.Case_Expression
is
   function "=" (L, R : Value_Type_Tuple) return Boolean
   is
   begin
      if L.Value_Type /= R.Value_Type then
         return False;
      end if;
      case L.Value_Type is
         when Boolean_Type =>
            return L.Bool_Value = R.Bool_Value;
         when Integer_Type =>
            return L.Int_Value = R.Int_Value;
         when String_Type =>
            return L.String_Value.Element = R.String_Value.Element;
      end case;
   end "=";

   -------------------------------------------------------------------------

   procedure Case_Expression_Evaluation
      (Policy        :        Muxml.XML_Data_Type;
       Expr_Node     :        DOM.Core.Node;
       Value_Of_Case :    out Value_Type_Tuple;
       Backtrace     : in out String_Vector.Vector)
   is
      Children  : constant DOM.Core.Node_List
         := McKae.XML.XPath.XIA.XPath_Query
              (N     => Expr_Node,
               XPath => "./case");
      Node_Name : constant String
         := DOM.Core.Elements.Get_Attribute
              (Elem => Expr_Node,
               Name => "name");

      Case_Node : DOM.Core.Node;
   begin
      if DOM.Core.Nodes.Length (List => Children) /= 1 then
         raise Invalid_Expression with
            "Case-expression '"
            & DOM.Core.Elements.Get_Attribute
            (Elem => Expr_Node,
             Name => "name")
            & "' has "
            & DOM.Core.Nodes.Length (List => Children)'Image
            & " case-child nodes. Should have exactly one.";
      end if;
      Case_Node := DOM.Core.Nodes.Item (List  => Children,
                                        Index => 0);

      Evaluate_Case_Node
         (Policy        => Policy,
          Case_Node     => Case_Node,
          Value_Of_Case => Value_Of_Case,
          Backtrace     => Backtrace);

      case Value_Of_Case.Value_Type is
         when Boolean_Type =>
            Mulog.Log (Msg => "Expanding expression '" & Node_Name
                       & "' with value '" & Value_Of_Case.Bool_Value'Image & "'");
            System_Config.Set_Value (Data  => Policy,
                                     Name  => Node_Name,
                                     Value => Value_Of_Case.Bool_Value);
         when Integer_Type =>
            Mulog.Log (Msg => "Expanding expression '" & Node_Name
                       & "' with value '" & Value_Of_Case.Int_Value'Image & "'");
            System_Config.Set_Value (Data  => Policy,
                                     Name  => Node_Name,
                                     Value => Value_Of_Case.Int_Value);
         when String_Type =>
            Mulog.Log (Msg => "Expanding expression '" & Node_Name
                       & "' with value '" & Value_Of_Case.String_Value.Element & "'");
            System_Config.Set_Value (Data  => Policy,
                                     Name  => Node_Name,
                                     Value => Value_Of_Case.String_Value.Element);
      end case;

   end Case_Expression_Evaluation;

   -------------------------------------------------------------------------

   procedure Evaluate_Case_Node
      (Policy        :        Muxml.XML_Data_Type;
       Case_Node     :        DOM.Core.Node;
       Value_Of_Case :    out Value_Type_Tuple;
       Backtrace     : in out String_Vector.Vector)
   is
      use all type DOM.Core.Node;
      Children  : constant DOM.Core.Node_List
         := McKae.XML.XPath.XIA.XPath_Query
         (N     => Case_Node,
          XPath => "./when | ./others");
      Child_Type : Variable_Type;
      Return_Node : DOM.Core.Node;

      ----------------------------------------------------------------------

      -- assign the value of Child to Child_Value
      procedure Evaluate_When_Child
         (Policy      :        Muxml.XML_Data_Type;
          Child       :        DOM.Core.Node;
          Child_Value :    out Value_Type_Tuple;
          Backtrace   : in out String_Vector.Vector);

      ----------------------------------------------------------------------

      procedure Evaluate_When_Child
         (Policy      :        Muxml.XML_Data_Type;
          Child       :        DOM.Core.Node;
          Child_Value :    out Value_Type_Tuple;
          Backtrace   : in out String_Vector.Vector)
      is
         Child_Name : constant String
            := DOM.Core.Nodes.Node_Name (N => Child);
         Def_Node   : DOM.Core.Node;
      begin
         if Child_Name = "case" then
            Evaluate_Case_Node
               (Policy        => Policy,
                Case_Node     => Child,
                Value_Of_Case => Child_Value,
                Backtrace     => Backtrace);

         elsif Child_Name = "variable" then
            Def_Node := Get_Defining_Node
               (Policy   => Policy,
                Var_Name => DOM.Core.Elements.Get_Attribute
                   (Elem => Child,
                    Name => "name"));
            Expand_Single_Node
               (Policy    => Policy,
                Node      => Def_Node,
                Backtrace => Backtrace);

            -- now we know that the defining node is not an expression
            Def_Node := Get_Defining_Node
               (Policy   => Policy,
                Var_Name => DOM.Core.Elements.Get_Attribute
                   (Elem => Child,
                    Name => "name"));
            Get_Type_And_Value
               (Node => Def_Node,
                Type_And_Value => Child_Value);

         elsif Child_Name = "boolean"
            or Child_Name = "integer"
            or Child_Name = "string"
         then
            Get_Type_And_Value
               (Node => Child,
                Type_And_Value => Child_Value);
         else
            raise Invalid_Expression with
               "When-Node inside of Case contains illegal node with name '"
               &  DOM.Core.Nodes.Node_Name (N => Child)
               & "'";
         end if;
      end Evaluate_When_Child;

   begin
      -- assign the Return_Node to the when-child that matches
      Evaluate_Case_Node_Frame (Policy      => Policy,
                                Case_Node   => Case_Node,
                                Return_Node => Return_Node,
                                Backtrace   => Backtrace);
      if Return_Node = null then
         raise Invalid_Expression with
            "Found case-node in expression where none of the actuals "
            & "matches. Case-variable has name '"
            & DOM.Core.Elements.Get_Attribute (Elem => Case_Node,
                                               Name => "variable")
            & "'";
      end if;

      for I in 0 .. DOM.Core.Nodes.Length (List => Children) - 1 loop
         declare
            Child : constant DOM.Core.Node
               := DOM.Core.Nodes.Item (List  => Children,
                                       Index => I);
            Child_Children : constant DOM.Core.Node_List
               := McKae.XML.XPath.XIA.XPath_Query
               (N     => Child,
                XPath => "./*");
            Child_Child : DOM.Core.Node;
            Child_Value : Value_Type_Tuple;
         begin
            if DOM.Core.Nodes.Length (List => Child_Children) /= 1 then
               raise Invalid_Expression with
                  "When-Node inside of Case has "
                  & DOM.Core.Nodes.Length (List => Child_Children)'Image
                  & " child nodes. Should have one.";
            end if;
            Child_Child := DOM.Core.Nodes.Item (List  => Child_Children,
                                                Index => 0);
            Evaluate_When_Child
               (Policy      => Policy,
                Child       => Child_Child,
                Child_Value => Child_Value,
                Backtrace   => Backtrace);

            if Child = Return_Node then
               Value_Of_Case :=  Child_Value;
            end if;

            -- check that all options are of the same type
            if I = 0 then
               Child_Type := Child_Value.Value_Type;
            elsif Child_Type /= Child_Value.Value_Type then
               raise Invalid_Expression with
                  "Case expression contains values of multiple types: '"
                  & Child_Type'Image
                  & "' and '"
                  & Child_Value.Value_Type'Image
                  & "'";
            end if;
         end;
      end loop;
   end Evaluate_Case_Node;

   -------------------------------------------------------------------------

   procedure Evaluate_Case_Node_Frame
      (Policy              :        Muxml.XML_Data_Type;
       Case_Node           :        DOM.Core.Node;
       Return_Node         :    out DOM.Core.Node;
       Backtrace           : in out String_Vector.Vector)
   is
      use all type DOM.Core.Node;
      Case_Variable_Name : constant String
         := DOM.Core.Elements.Get_Attribute (Elem => Case_Node,
                                             Name => "variable");

      Case_Variable_Value : Value_Type_Tuple;
      Node : DOM.Core.Node;
      Case_Children : constant DOM.Core.Node_List
         := McKae.XML.XPath.XIA.XPath_Query
         (N     => Case_Node,
          XPath => "./when | ./others");

      ---------------------------------------------------------------------

      -- Evaluate a when-option which is not a reference and write result to
      -- When_Variable_Value
      procedure  Evaluate_Explicit_When_Option
         (Policy              :        Muxml.XML_Data_Type;
          When_Node_RawValue  :        String;
          Case_Variable_Value :        Value_Type_Tuple;
          When_Variable_Value :    out Value_Type_Tuple;
          Backtrace           : in out String_Vector.Vector);

      ---------------------------------------------------------------------

      procedure  Evaluate_Explicit_When_Option
         (Policy              :        Muxml.XML_Data_Type;
          When_Node_RawValue  :        String;
          Case_Variable_Value :        Value_Type_Tuple;
          When_Variable_Value :    out Value_Type_Tuple;
          Backtrace           : in out String_Vector.Vector)
      is
         Node : DOM.Core.Node;
      begin
         -- start evaluation of the given when-value
         if When_Node_RawValue'Length > 0
            and then When_Node_RawValue (When_Node_RawValue'First) = '$'
         then
            -- make sure the node has been evaluated
            Node := Get_Defining_Node
               (Policy   => Policy,
                Var_Name => When_Node_RawValue
                   (When_Node_RawValue'First + 1 .. When_Node_RawValue'Last));
            Expand_Single_Node (Policy    => Policy,
                                Node      => Node,
                                Backtrace => Backtrace);
            Node := Get_Defining_Node
               (Policy   => Policy,
                Var_Name => When_Node_RawValue
                   (When_Node_RawValue'First + 1 .. When_Node_RawValue'Last));

            Get_Type_And_Value
               (Node => Node,
                Type_And_Value => When_Variable_Value);
            if When_Variable_Value.Value_Type /= Case_Variable_Value.Value_Type then
               raise  Invalid_Expression with
                  "Found case node where variable types do not match. "
                  & "Case-variable type is '"
                  & Case_Variable_Value.Value_Type'Image
                  & "' when-variable type is '"
                  & When_Variable_Value.Value_Type'Image
                  & "'";
            end if;
         else
            -- in this case we have a 'constant' without type
            When_Variable_Value.Value_Type
               := Case_Variable_Value.Value_Type;

            begin
               if Case_Variable_Value.Value_Type = Boolean_Type then
                  When_Variable_Value.Bool_Value
                     := Boolean'Value (When_Node_RawValue);
               elsif Case_Variable_Value.Value_Type = Integer_Type then
                  When_Variable_Value.Int_Value
                     := Integer'Value (When_Node_RawValue);
               else
                  When_Variable_Value.String_Value
                     := String_Holder_Type.To_Holder (When_Node_RawValue);
               end if;
            exception
               when Constraint_Error =>
                  raise Invalid_Expression with
                     "Found when-node with value '"
                     & When_Node_RawValue
                     & "' which cannot be cast to "
                     & Case_Variable_Value.Value_Type'Image;
            end;
         end if;
      end Evaluate_Explicit_When_Option;

   begin
      Return_Node := null;

      -- get type and value of case-variable
      if not Muxml.Utils.Has_Attribute
         (Node => Case_Node,
          Attr_Name => "variable")
      then
         raise  Invalid_Expression with
            "Found case-node without 'variable' attribute";
      end if;

      -- guarantee that Node has been evaluated
      Node := Get_Defining_Node
         (Policy   => Policy,
          Var_Name => Case_Variable_Name);
      Expand_Single_Node (Policy    => Policy,
                          Node      => Node,
                          Backtrace => Backtrace);
      Node := Get_Defining_Node
         (Policy   => Policy,
          Var_Name => Case_Variable_Name);

      Get_Type_And_Value
         (Node => Node,
          Type_And_Value => Case_Variable_Value);

      -- get type and value of when-variables
      if DOM.Core.Nodes.Length (List => Case_Children) < 1 then
         raise  Invalid_Expression with
            "Found case-node without when-children";
      end if;

      for I in 0 .. DOM.Core.Nodes.Length (List => Case_Children) - 1 loop
         declare
            When_Node : constant DOM.Core.Node
               := DOM.Core.Nodes.Item (List  => Case_Children,
                                       Index => I);
            When_Node_RawValue : constant String
               := DOM.Core.Elements.Get_Attribute (Elem => When_Node,
                                                   Name => "value");
            When_Variable_Value : Value_Type_Tuple;
         begin
            if DOM.Core.Nodes.Node_Name (N => When_Node) = "others" then
               if I < DOM.Core.Nodes.Length (List => Case_Children) - 1 then
                  raise  Invalid_Expression with
                     "Found 'others'-node which is not the last child of 'case'";
               end if;
               if Return_Node = null then
                  Return_Node := When_Node;
               end if;
            elsif not Muxml.Utils.Has_Attribute
               (Node => When_Node,
                Attr_Name => "value")
            then
               raise  Invalid_Expression with
                  "Found when-node without 'value' attribute";
            else
               Evaluate_Explicit_When_Option
                  (Policy              => Policy,
                   When_Node_RawValue  => When_Node_RawValue,
                   Case_Variable_Value => Case_Variable_Value,
                   When_Variable_Value => When_Variable_Value,
                   Backtrace           => Backtrace);

               if "=" (L => When_Variable_Value, R => Case_Variable_Value) then
                  if Return_Node = null then
                     Return_Node := When_Node;
                  else
                     raise Invalid_Expression with
                        "Found case node where multiple values match. "
                        & "Case-variable value is '"
                        & To_String (VTT => Case_Variable_Value)
                        & "'";
                  end if;
               end if;
            end if;
         end;
      end loop;

   end Evaluate_Case_Node_Frame;

   -------------------------------------------------------------------------

   procedure Get_Type_And_Value
      (Node           :     DOM.Core.Node;
       Type_And_Value : out Value_Type_Tuple)
   is
      Node_Name : constant String
         := DOM.Core.Nodes.Node_Name (N => Node);
      Node_Value : constant String
         := DOM.Core.Elements.Get_Attribute (Elem => Node,
                                             Name => "value");
   begin
      if not Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "value") then
         raise Invalid_Expression with
            "Get_Type_And_Value called on node with name '"
            & Node_Name
            & "' which has no value";
      elsif Node_Value'Length > 0
         and then Node_Value (Node_Value'First) = '$'
      then
         raise Invalid_Expression with
            "Get_Type_And_Value called on node with name '"
            & Node_Name
            & "' which has value starting with '$'";
      end if;

      if Node_Name = "boolean" then
         Type_And_Value.Value_Type := Boolean_Type;
         Type_And_Value.Bool_Value := Boolean'Value (Node_Value);
      elsif Node_Name = "integer" then
         Type_And_Value.Value_Type := Integer_Type;
         Type_And_Value.Int_Value := Integer'Value (Node_Value);
      elsif Node_Name = "string" then
         Type_And_Value.Value_Type := String_Type;
         Type_And_Value.String_Value
            := String_Holder_Type.To_Holder (Node_Value);
      else
         raise Invalid_Expression with
            "Get_Type_And_Value called on node with name '"
            & Node_Name
            & "'";
      end if;
   end Get_Type_And_Value;

   -------------------------------------------------------------------------

   function To_String (VTT : Value_Type_Tuple) return String
   is
      package ASU renames Ada.Strings.Unbounded;
      Output : ASU.Unbounded_String;
   begin
      Output := ASU.To_Unbounded_String (VTT.Value_Type'Image & " ");
      case VTT.Value_Type is
         when Boolean_Type =>
            ASU.Append (Source => Output,
                        New_Item => VTT.Bool_Value'Image);
         when Integer_Type =>
            ASU.Append (Source => Output,
                        New_Item => VTT.Int_Value'Image);
         when String_Type =>
            ASU.Append (Source => Output,
                        New_Item => VTT.String_Value.Element);
      end case;
      return ASU.To_String (Output);
   end To_String;

end Mutools.Expressions.Case_Expression;
