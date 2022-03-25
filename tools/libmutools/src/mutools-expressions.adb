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

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Mutools.System_Config;

package body Mutools.Expressions
is

   -------------------------------------------------------------------------

   function Bool_Value
     (Policy : Muxml.XML_Data_Type;
      Node   : DOM.Core.Node)
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
            Result := System_Config.Get_Value
              (Data => Policy,
               Name => DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "name"));
      end case;

      return Result;
   end Bool_Value;

   -------------------------------------------------------------------------

   procedure Expand (Policy : Muxml.XML_Data_Type)
   is
      Exprs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/*/expressions/expression");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Exprs) - 1 loop
         declare
            Expr : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Exprs,
                                      Index => I);
            Expr_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Expr,
                 Name => "name");
            Expr_Value : constant Boolean
              := Expression (Policy => Policy,
                             Node   => Expr);
         begin
            Mulog.Log (Msg => "Expanding expression '" & Expr_Name
                       & "' with value " & Expr_Value'Img);
            System_Config.Set_Value
              (Data  => Policy,
               Name  => Expr_Name,
               Value => Expr_Value);
         end;
      end loop;
   end Expand;

   -------------------------------------------------------------------------

   function Expression
     (Policy : Muxml.XML_Data_Type;
      Node   : DOM.Core.Node)
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
         return Op (X => Expression (Policy => Policy,
                                     Node   => C (Children, 0)),
                    Y => Expression (Policy => Policy,
                                     Node   => C (Children, 1)));
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
            return Expression (Policy => Policy,
                               Node   => C (Children, 0));
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
                    (Policy => Policy,
                     Node   => C (Children, 0)),
                    Y => Int_Value
                      (Policy => Policy,
                       Node   => C (Children, 1)));
      end Eval_Integers;

      ----------------------------------------------------------------------

      function Eval_Not return Boolean
      is
      begin
         if C (Children, 0) = null then
            raise Invalid_Expression with "Operator 'not' requires one child"
              & " element";
         end if;

         return not Expression (Policy => Policy,
                                Node   => C (Children, 0));
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
            | Expr_Variable   => Result := Bool_Value (Policy => Policy,
                                                       Node   => Node);
      end case;

      return Result;
   end Expression;

   -------------------------------------------------------------------------

   function Int_Value
     (Policy : Muxml.XML_Data_Type;
      Node   : DOM.Core.Node)
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
            Result := System_Config.Get_Value
              (Data => Policy,
               Name => DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "name"));
      end case;

      return Result;
   end Int_Value;

end Mutools.Expressions;
