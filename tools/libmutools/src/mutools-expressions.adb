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

with Ada.Exceptions;
with Ada.Strings.Unbounded;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Mutools.System_Config;

with Mutools.Expressions.Case_Expression;
package body Mutools.Expressions
is
   -------------------------------------------------------------------------

   procedure Add_To_Backtrace
      (Backtrace : in out String_Vector.Vector;
       Name      :        String)
   is
   begin
      if String_Vector.Contains (Container => Backtrace,
                                 Item      => Name)
      then
         declare
            Path : Ada.Strings.Unbounded.Unbounded_String;
         begin
            for I in String_Vector.Find_Index (Container => Backtrace,
                                               Item      => Name)
                     .. Backtrace.Last_Index loop
               Ada.Strings.Unbounded.Append
                  (Source   => Path,
                   New_Item => Backtrace (I));
               Ada.Strings.Unbounded.Append
                  (Source   => Path,
                   New_Item => " > ");
            end loop;
            Ada.Strings.Unbounded.Append
               (Source   => Path,
                New_Item => Name);

            raise Invalid_Expression with
               "Resolving the value of node with name '"
               & Name
               & "' lead to cyclic dependency: "
               & Ada.Strings.Unbounded.To_String (Path);
         end;
      end if;
      String_Vector.Append (Container => Backtrace,
                            New_Item  => Name);
   end Add_To_Backtrace;

   -------------------------------------------------------------------------

   function Bool_Value
     (Policy    :        Muxml.XML_Data_Type;
      Node      :        DOM.Core.Node;
      Backtrace : in out String_Vector.Vector)
      return Boolean
   is
      type Bool_Kind is (Bool_Boolean, Bool_Variable);

      Bool_Type : Bool_Kind;
      Result    : Boolean;
   begin
      begin
         Bool_Type := Bool_Kind'Value
           ("Bool_" & DOM.Core.Nodes.Node_Name (N => Node));
      exception
         when Constraint_Error =>
            raise Invalid_Expression with "Invalid boolean type '"
              & DOM.Core.Nodes.Node_Name (N => Node) & "'";
      end;

      case Bool_Type is
         when Bool_Boolean  =>
            Result := Boolean'Value
              (DOM.Core.Elements.Get_Attribute
                                     (Elem => Node,
                                      Name => "value"));
         when Bool_Variable =>
            declare
               Var_Name : constant String
                        := DOM.Core.Elements.Get_Attribute
                             (Elem => Node,
                              Name => "name");
            begin
               Result := Evaluate_Boolean
                  (Policy    => Policy,
                   Node      => Get_Defining_Node (Policy   => Policy,
                                                   Var_Name => Var_Name),
                   Backtrace => Backtrace);
            end;
      end case;

      return Result;
   end Bool_Value;

   -------------------------------------------------------------------------

   function Boolean_Expression
     (Policy    :        Muxml.XML_Data_Type;
      Node      :        DOM.Core.Node;
      Backtrace : in out String_Vector.Vector)
      return Boolean
   is
      use type DOM.Core.Node;

      function C
        (List  : DOM.Core.Node_List;
         Index : Natural)
         return DOM.Core.Node renames DOM.Core.Nodes.Item;

      type Expression_Kind is
        (Expr_And, Expr_Boolean, Expr_Expression, Expr_Eq, Expr_Gt, Expr_Lt,
         Expr_Ne, Expr_Not, Expr_Or, Expr_Variable);

      Children  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Node,
           XPath => "*");
      Expr      : Expression_Kind;
      Result    : Boolean;

      generic
         with function Op (X, Y : Integer) return Boolean;
         Op_Name : String;
      function Eval_Integers return Boolean;

      generic
         with function Op (X, Y : Boolean) return Boolean;
         Op_Name : String;
      function Eval_Booleans return Boolean;

      --  Evaluate expression.
      function Eval_Expr return Boolean;

      --  Evaluate not operation.
      function Eval_Not return Boolean;

      ----------------------------------------------------------------------

      function Eval_Booleans return Boolean
      is
      begin
         if C (Children, 0) = null or else C (Children, 1) = null then
            raise Invalid_Expression with "Operator '" & Op_Name
              & "' requires two child elements";
         end if;
         return Op (X => Boolean_Expression
                           (Policy => Policy,
                            Node   => C (Children, 0),
                            Backtrace => Backtrace),
                    Y => Boolean_Expression
                           (Policy => Policy,
                            Node   => C (Children, 1),
                            Backtrace => Backtrace));
      end Eval_Booleans;

      ----------------------------------------------------------------------

      function Eval_Expr return Boolean
      is
         Expr_Name : constant String
           := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "name");
      begin
         if C (Children, 0) = null then
            raise Invalid_Expression with "Expression '" & Expr_Name
              & "': Missing operator";
         end if;

         begin
            return Boolean_Expression
                     (Policy => Policy,
                      Node   => C (Children, 0),
                      Backtrace => Backtrace);
         exception
            when E : Invalid_Expression =>
               raise Invalid_Expression with "Expression '" & Expr_Name
                 & "': " & Ada.Exceptions.Exception_Message (X => E);
         end;
      end Eval_Expr;

      ----------------------------------------------------------------------

      function Eval_Integers return Boolean
      is
      begin
         if C (Children, 0) = null or else C (Children, 1) = null then
            raise Invalid_Expression with "Operator '" & Op_Name
              & "' requires two child elements";
         end if;
         return Op (X => Int_Value
                           (Policy    => Policy,
                            Node      => C (Children, 0),
                            Backtrace => Backtrace),
                    Y => Int_Value
                           (Policy    => Policy,
                            Node      => C (Children, 1),
                            Backtrace => Backtrace));
      end Eval_Integers;

      ----------------------------------------------------------------------

      function Eval_Not return Boolean
      is
      begin
         if C (Children, 0) = null then
            raise Invalid_Expression with "Operator 'not' requires one child"
              & " element";
         end if;

         return not Boolean_Expression
                       (Policy => Policy,
                        Node   => C (Children, 0),
                        Backtrace => Backtrace);
      end Eval_Not;

      ----------------------------------------------------------------------

      --  Evaluate Eq operation.
      function Eval_Eq is new Eval_Integers
        (Op      => "=",
         Op_Name => "eq");

      --  Evaluate Gt operation.
      function Eval_Gt is new Eval_Integers
        (Op      => ">",
         Op_Name => "gt");

      --  Evaluate Lt operation.
      function Eval_Lt is new Eval_Integers
        (Op      => "<",
         Op_Name => "lt");

      --  Evaluate Ne operation.
      function Eval_Ne is new Eval_Integers
        (Op      => "/=",
         Op_Name => "ne");

      --  Evaluate and operation.
      function Eval_And is new Eval_Booleans
        (Op      => "and",
         Op_Name => "and");

      --  Evaluate and operation.
      function Eval_Or is new Eval_Booleans
        (Op      => "or",
         Op_Name => "or");

      ----------------------------------------------------------------------

   begin
      begin
         Expr := Expression_Kind'Value
           ("Expr_" &  DOM.Core.Nodes.Node_Name (N => Node));
      exception
         when Constraint_Error =>
            raise Invalid_Expression with "Invalid expression term '"
              & DOM.Core.Nodes.Node_Name (N => Node) & "'";
      end;

      case Expr is
         when Expr_Eq         => Result := Eval_Eq;
         when Expr_Gt         => Result := Eval_Gt;
         when Expr_Lt         => Result := Eval_Lt;
         when Expr_Ne         => Result := Eval_Ne;
         when Expr_Not        => Result := Eval_Not;
         when Expr_And        => Result := Eval_And;
         when Expr_Or         => Result := Eval_Or;
         when Expr_Expression => Result := Eval_Expr;
         when Expr_Boolean
            | Expr_Variable   => Result := Bool_Value (Policy    => Policy,
                                                       Node      => Node,
                                                       Backtrace => Backtrace);
      end case;

      return Result;
   end Boolean_Expression;

   -------------------------------------------------------------------------

   function Evaluate_Boolean
      (Policy    :        Muxml.XML_Data_Type;
       Node      :        DOM.Core.Node;
       Backtrace : in out String_Vector.Vector)
      return Boolean
   is
      use all type Mutools.Expressions.Case_Expression.Variable_Type;
      Node_Type : constant String
         := DOM.Core.Nodes.Node_Name (N => Node);
      Node_Name : constant String
         := DOM.Core.Elements.Get_Attribute
                             (Elem => Node,
                              Name => "name");
      Result : Boolean;

   begin
      Add_To_Backtrace (Backtrace => Backtrace,
                        Name      => Node_Name);
      if Node_Type = "boolean" then
         declare
            Node_Value : constant String
                       :=  DOM.Core.Elements.Get_Attribute
                             (Elem => Node,
                              Name => "value");
            Next_Node : DOM.Core.Node;
         begin
            if Node_Value (Node_Value'First) /= '$' then
               Result := System_Config.Get_Value
                  (Data  => Policy,
                   Name  => Node_Name);
            else
               Next_Node := Get_Defining_Node
                  (Policy   => Policy,
                   Var_Name => Node_Value
                   (Node_Value'First + 1 .. Node_Value'Last));
               Result := Evaluate_Boolean
                  (Policy    => Policy,
                   Node      => Next_Node,
                   Backtrace => Backtrace);
               Mulog.Log (Msg => "Expanding config-variable '" & Node_Name
                    & "' with value '" & Result'Image & "'");
               System_Config.Set_Value (Data  => Policy,
                                        Name  => Node_Name,
                                        Value => Result);
            end if;
         end;

      elsif Node_Type = "expression" then
         if Get_Expr_Type (Expr => Node) = "case" then
            declare
               Result_Case : Mutools.Expressions.Case_Expression.Value_Type_Tuple;
            begin
               Mutools.Expressions.Case_Expression.Case_Expression_Evaluation
                  (Policy        => Policy,
                   Expr_Node     => Node,
                   Value_Of_Case => Result_Case,
                   Backtrace     => Backtrace);
               if Result_Case.Value_Type /=
                  Mutools.Expressions.Case_Expression.Boolean_Type
               then
                  raise Muxml.Validation_Error with
                     "A Boolean variable or expression points to expression with name '"
                     & Node_Name
                     & "' which is not Boolean valued";
               end if;
               Result := Result_Case.Bool_Value;
            end;

         elsif Get_Expr_Type (Expr => Node) = "boolean" then
            Result :=  Boolean_Expression (Policy => Policy,
                                           Node => Node,
                                           Backtrace => Backtrace);
            Mulog.Log (Msg => "Expanding expression '" & Node_Name
                       & "' with value '" & Result'Image & "'");
            System_Config.Set_Value (Data  => Policy,
                                     Name  => Node_Name,
                                     Value => Result);
         else
            raise Muxml.Validation_Error with
               "A Boolean variable or expression points to expression with name '"
               & Node_Name
               & "' which is not Boolean valued";
         end if;

      else
         raise Muxml.Validation_Error with
            "A Boolean variable or expression points to node with type '"
            & Node_Type
            & "' which is not Boolean valued";
      end if;

      String_Vector.Delete_Last (Container => Backtrace);
      return Result;

   end Evaluate_Boolean;

   -------------------------------------------------------------------------

   function Evaluate_Integer
      (Policy    :        Muxml.XML_Data_Type;
       Node      :        DOM.Core.Node;
       Backtrace : in out String_Vector.Vector)
      return Integer
   is
      use all type Mutools.Expressions.Case_Expression.Variable_Type;

      Node_Type : constant String
         := DOM.Core.Nodes.Node_Name (N => Node);
      Node_Name : constant String
         := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "name");
      Result : Integer;

   begin
      Add_To_Backtrace (Backtrace => Backtrace,
                        Name      => Node_Name);
      if Node_Type = "integer" then
         declare
            Node_Value : constant String
                       :=  DOM.Core.Elements.Get_Attribute
                             (Elem => Node,
                              Name => "value");
            Next_Node : DOM.Core.Node;
         begin
            if Node_Value (Node_Value'First) /= '$' then
               Result := System_Config.Get_Value
                  (Data  => Policy,
                   Name  => Node_Name);
            else
               Next_Node := Get_Defining_Node
                  (Policy   => Policy,
                   Var_Name => Node_Value
                   (Node_Value'First + 1 .. Node_Value'Last));
               Result := Evaluate_Integer
                  (Policy    => Policy,
                   Node      => Next_Node,
                   Backtrace => Backtrace);
               Mulog.Log (Msg => "Expanding config-variable '" & Node_Name
                    & "' with value '" & Result'Image & "'");
               System_Config.Set_Value (Data  => Policy,
                                        Name  => Node_Name,
                                        Value => Result);
            end if;
         end;

      elsif Node_Type = "expression" then
         if Get_Expr_Type (Expr => Node) = "case" then
            declare
               Result_Case : Mutools.Expressions.Case_Expression.Value_Type_Tuple;
            begin
               Mutools.Expressions.Case_Expression.Case_Expression_Evaluation
                  (Policy        => Policy,
                   Expr_Node     => Node,
                   Value_Of_Case => Result_Case,
                   Backtrace     => Backtrace);
               if Result_Case.Value_Type /=
                  Mutools.Expressions.Case_Expression.Integer_Type
               then
                  raise Muxml.Validation_Error with
                     "An Integer variable or expression points to expression with name '"
                     & Node_Name
                     & "' which is not integer valued";
               end if;
               Result := Result_Case.Int_Value;
            end;
         else
            raise Muxml.Validation_Error with
               "An integer variable or expression points to expression with name '"
               & Node_Name
               & "' which is not integer valued";
         end if;
      else
         raise Muxml.Validation_Error with
            "An integer variable or expression points to node with type '"
            & Node_Type
            & "' which is not integer valued";
      end if;

      String_Vector.Delete_Last (Container => Backtrace);
      return Result;

   end Evaluate_Integer;

   -------------------------------------------------------------------------

   function Evaluate_String
      (Policy    :        Muxml.XML_Data_Type;
       Node      :        DOM.Core.Node;
       Backtrace : in out String_Vector.Vector)
      return String
   is
      package ASU renames Ada.Strings.Unbounded;
      use all type Mutools.Expressions.Case_Expression.Variable_Type;

      Node_Type : constant String
         := DOM.Core.Nodes.Node_Name (N => Node);
      Node_Name : constant String
         := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "name");
      Result : ASU.Unbounded_String;

   begin
      Add_To_Backtrace (Backtrace => Backtrace,
                        Name      => Node_Name);

      if Node_Type = "string" then
         declare
            Node_Value : constant String
                       :=  DOM.Core.Elements.Get_Attribute
                             (Elem => Node,
                              Name => "value");
            Next_Node : DOM.Core.Node;
         begin
            if Node_Value'Length > 0 then
               if Node_Value (Node_Value'First) = '$' then
                  Next_Node := Get_Defining_Node
                     (Policy   => Policy,
                      Var_Name => Node_Value
                      (Node_Value'First + 1 .. Node_Value'Last));
                  Result := ASU.To_Unbounded_String (Evaluate_String
                                                     (Policy    => Policy,
                                                      Node      => Next_Node,
                                                      Backtrace => Backtrace));
                  Mulog.Log (Msg => "Expanding config-variable '" & Node_Name
                             & "' with value '" & ASU.To_String (Result) & "'");
                  System_Config.Set_Value (Data  => Policy,
                                           Name  => Node_Name,
                                           Value => ASU.To_String (Result));
               else
                  Result := ASU.To_Unbounded_String (Node_Value);
               end if;
            else
               Result := ASU.To_Unbounded_String (Node_Value);
            end if;
         end;

      elsif Node_Type = "expression" then
         if Get_Expr_Type (Expr => Node) = "case" then
            declare
               Result_Case : Mutools.Expressions.Case_Expression.Value_Type_Tuple;
            begin
               Mutools.Expressions.Case_Expression.Case_Expression_Evaluation
                  (Policy        => Policy,
                   Expr_Node     => Node,
                   Value_Of_Case => Result_Case,
                   Backtrace     => Backtrace);

               if Result_Case.Value_Type /=
                  Mutools.Expressions.Case_Expression.String_Type
               then
                  raise Muxml.Validation_Error with
                     "A String variable or expression points to expression "
                     & "with name '"
                     & Node_Name
                     & "' which is not String valued";
               end if;
               Result :=  ASU.To_Unbounded_String
                  (Result_Case.String_Value.Element);
            end;

         elsif Get_Expr_Type (Expr => Node) = "string" then
            Result :=  ASU.To_Unbounded_String
               (String_Expression (Policy => Policy,
                                   Node => Node,
                                   Backtrace => Backtrace));
            Mulog.Log (Msg => "Expanding expression '" & Node_Name
                       & "' with value '" & ASU.To_String (Result) & "'");
            System_Config.Set_Value (Data  => Policy,
                                     Name  => Node_Name,
                                     Value => ASU.To_String (Result));
         else
            raise Muxml.Validation_Error with
               "A string variable or expression points to expression with name '"
               & Node_Name
               & "' which is not string valued";
         end if;
      else
         raise Muxml.Validation_Error with
            "A string variable or expression points to node with type '"
            & Node_Type
            & "' which is not string valued";
      end if;

      String_Vector.Delete_Last (Container => Backtrace);
      return ASU.To_String (Result);

   end Evaluate_String;

   -------------------------------------------------------------------------

   procedure Expand (Policy : Muxml.XML_Data_Type)
   is
      Backtrace  : String_Vector.Vector;

      Vars_Exprs : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                      (N     => Policy.Doc,
                       XPath =>  "/*/config/boolean | "
                               & "/*/config/integer | "
                               & "/*/config/string | "
                               & "/*/expressions/expression");

   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Vars_Exprs) - 1 loop
         declare
            Var      : constant DOM.Core.Node
                     := DOM.Core.Nodes.Item (List  => Vars_Exprs,
                                             Index => I);
         begin
            Expand_Single_Node
               (Policy    => Policy,
                Node      => Var,
                Backtrace => Backtrace);
         end;
      end loop;
   end Expand;

   -------------------------------------------------------------------------

   procedure Expand_Single_Node
      (Policy    :        Muxml.XML_Data_Type;
       Node      :        DOM.Core.Node;
       Backtrace : in out String_Vector.Vector)
   is
      Node_Type : constant String
         :=  DOM.Core.Nodes.Node_Name (N => Node);

      ----------------------------------------------------------------------

      -- do nothing (used to discard function return value)
      procedure Discard (I : Boolean);
      procedure Discard (I : Integer);
      procedure Discard (I : String);

      ----------------------------------------------------------------------

      procedure Discard (I : Boolean)
      is
         pragma Unreferenced (I);
      begin
         null;
      end Discard;

      procedure Discard (I : Integer)
      is
         pragma Unreferenced (I);
      begin
         null;
      end Discard;

      procedure Discard (I : String)
      is
         pragma Unreferenced (I);
      begin
         null;
      end Discard;

   begin
      if    Node_Type = "boolean" then
         Discard (Evaluate_Boolean (Policy => Policy,
                                    Node   => Node,
                                    Backtrace => Backtrace));
      elsif Node_Type = "integer" then
         Discard (Evaluate_Integer (Policy => Policy,
                                    Node   => Node,
                                    Backtrace => Backtrace));
      elsif Node_Type = "string" then
         Discard (Evaluate_String (Policy => Policy,
                                   Node   => Node,
                                   Backtrace => Backtrace));
      elsif Node_Type = "expression" then
         if Get_Expr_Type (Expr => Node) = "boolean" then
            Discard (Evaluate_Boolean (Policy => Policy,
                                       Node   => Node,
                                       Backtrace => Backtrace));
         elsif Get_Expr_Type (Expr => Node) = "case" then
            declare
               Dummy : Mutools.Expressions.Case_Expression.Value_Type_Tuple;
               Node_Name : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem => Node,
                   Name => "name");
            begin
               Add_To_Backtrace (Backtrace => Backtrace, Name => Node_Name);
               Mutools.Expressions.Case_Expression.Case_Expression_Evaluation
                  (Policy        => Policy,
                   Expr_Node     => Node,
                   Value_Of_Case => Dummy,
                   Backtrace     => Backtrace);
               pragma Unreferenced (Dummy);
               String_Vector.Delete_Last (Container => Backtrace);
            end;
         else
            Discard (Evaluate_String (Policy => Policy,
                                      Node   => Node,
                                      Backtrace => Backtrace));
         end if;
      else
         raise Invalid_Expression
            with "Invalid config-variable or expression with type '"
            & Node_Type & "'";
      end if;
   end Expand_Single_Node;

   -------------------------------------------------------------------------

   function Get_Defining_Node
      (Policy   : Muxml.XML_Data_Type;
       Var_Name : String)
      return DOM.Core.Node
   is
      Config_Vars : constant DOM.Core.Node_List
                  := McKae.XML.XPath.XIA.XPath_Query
                     (N     => Policy.Doc,
                      XPath =>  "/*/config/boolean[@name='" & Var_Name & "'] | "
                              & "/*/config/integer[@name='" & Var_Name & "'] | "
                              & "/*/config/string[@name='"  & Var_Name & "']");
      Exprs_Vars : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Policy.Doc,
                    XPath =>  "/*/expressions/expression[@name='"
                             & Var_Name & "']");
   begin
      if DOM.Core.Nodes.Length (List => Config_Vars) > 0 then
         return DOM.Core.Nodes.Item (List  => Config_Vars,
                                     Index => 0);
      elsif DOM.Core.Nodes.Length (List => Exprs_Vars) > 0 then
         return DOM.Core.Nodes.Item (List  => Exprs_Vars,
                                     Index => 0);
      else
         raise Muxml.Validation_Error with
            "A variable or expression points to '" & Var_Name
            & "' which does not exit";
      end if;
   end Get_Defining_Node;

   ----------------------------------------------------------------------

   function Get_Expr_Type (Expr : DOM.Core.Node) return String
   is
      use type DOM.Core.Node;
      use type DOM.Core.Node_Types;

      First_Child : DOM.Core.Node
         := DOM.Core.Nodes.First_Child (N => Expr);
   begin
      while DOM.Core.Nodes.Node_Type (N => First_Child)
         /= DOM.Core.Element_Node loop
         First_Child := DOM.Core.Nodes.Next_Sibling (N => First_Child);
         exit when  First_Child = null;
      end loop;

      if First_Child = null then
         raise Invalid_Expression with "Expression with name "
            & DOM.Core.Elements.Get_Attribute
            (Elem => Expr,
             Name => "name")
            & " is empty.";
      end if;

      if DOM.Core.Nodes.Node_Name (N => First_Child) = "concatenation" then
         return "string";
      elsif DOM.Core.Nodes.Node_Name (N => First_Child) = "case" then
         return "case";
      else
         return "boolean";
      end if;
   end  Get_Expr_Type;

   -------------------------------------------------------------------------

   function Int_Value
     (Policy    :        Muxml.XML_Data_Type;
      Node      :        DOM.Core.Node;
      Backtrace : in out String_Vector.Vector)
      return Integer
   is
      type Int_Kind is (Int_Integer, Int_Variable);

      Int_Type : Int_Kind;
      Result   : Integer;
   begin
      begin
         Int_Type := Int_Kind'Value
           ("Int_" & DOM.Core.Nodes.Node_Name (N => Node));
      exception
         when Constraint_Error =>
            raise Invalid_Expression with "Invalid integer type '"
              & DOM.Core.Nodes.Node_Name (N => Node) & "'";
      end;

      case Int_Type is
         when Int_Integer  =>
            Result := Integer'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "value"));
         when Int_Variable =>
            declare
               Var_Name : constant String
                        := DOM.Core.Elements.Get_Attribute
                             (Elem => Node,
                              Name => "name");
            begin
               Result := Evaluate_Integer
                  (Policy    => Policy,
                   Node      => Get_Defining_Node (Policy   => Policy,
                                                   Var_Name => Var_Name),
                   Backtrace => Backtrace);
            end;
      end case;

      return Result;
   end Int_Value;

   -------------------------------------------------------------------------

   function String_Expression
     (Policy    :        Muxml.XML_Data_Type;
      Node      :        DOM.Core.Node;
      Backtrace : in out String_Vector.Vector)
     return String
   is
      package ASU renames Ada.Strings.Unbounded;

      -- we can assume that the first child is <concatenation>
      -- as there is no other programm-path where String_Expression is called
      Children  : constant DOM.Core.Node_List
                := McKae.XML.XPath.XIA.XPath_Query
                      (N     => Node,
                       XPath => "./concatenation/*");
      Result    : ASU.Unbounded_String;
   begin
      if DOM.Core.Nodes.Length (List => Children) < 2 then
         raise Invalid_Expression with
            "Concatenation-expression '"
            & DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "name")
            & "' has less than two children";
      end if;
      for I in 0 .. DOM.Core.Nodes.Length (List => Children) - 1 loop
         declare
            Child  : constant DOM.Core.Node
                   := DOM.Core.Nodes.Item (List  => Children,
                                           Index => I);
         begin
            Ada.Strings.Unbounded.Append
               (Source   => Result,
                New_Item => ASU.To_Unbounded_String
                                 (String_Value (Policy    => Policy,
                                                Node      => Child,
                                                Backtrace => Backtrace)));
         end;
      end loop;
      return ASU.To_String (Result);

   end String_Expression;

   -------------------------------------------------------------------------

   function String_Value
     (Policy    :        Muxml.XML_Data_Type;
      Node      :        DOM.Core.Node;
      Backtrace : in out String_Vector.Vector)
      return String
   is
      Node_Name : constant String
                := DOM.Core.Nodes.Node_Name (N => Node);
   begin
      if Node_Name = "string" then
         return DOM.Core.Elements.Get_Attribute (Elem => Node,
                                                 Name => "value");
      elsif Node_Name = "variable" then
         declare
            Var_Name : constant String
               := DOM.Core.Elements.Get_Attribute
               (Elem => Node,
                Name => "name");
         begin
            return Evaluate_String
               (Policy    => Policy,
                Node      => Get_Defining_Node (Policy   => Policy,
                                                Var_Name => Var_Name),
                Backtrace => Backtrace);
         end;
      else
         raise Invalid_Expression with "Invalid string type '"
               & DOM.Core.Nodes.Node_Name (N => Node) & "'";
      end if;
   end String_Value;

end Mutools.Expressions;
