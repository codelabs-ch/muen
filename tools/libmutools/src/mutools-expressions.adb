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
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Mutools.Expressions.Case_Expression;
with Mutools.Xmldebuglog;
with Muxml.Utils;

package body Mutools.Expressions
is
   Expr_Debug_Active : Boolean := False;

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
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
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
               if Node_Access.Output_Boolean.Contains (Var_Name) then
                  Result := Node_Access.Output_Boolean (Var_Name);
               else
                  Result := Evaluate_Boolean
                     (Node        => Get_Defining_Node (Var_Name    => Var_Name,
                                                        Node_Access => Node_Access),
                      Backtrace   => Backtrace,
                      Node_Access => Node_Access);
               end if;
            end;
      end case;

      return Result;
   end Bool_Value;

   -------------------------------------------------------------------------

   function Boolean_Expression
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
      return Boolean
   is
      type Expression_Kind is
        (Expr_And, Expr_Boolean, Expr_Expression, Expr_Eq, Expr_Gt, Expr_Lt,
         Expr_Ne, Expr_Not, Expr_Or, Expr_Variable);

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
         if Get_Nth_Child_Node (Parent => Node, N => 1) = null
            or else Get_Nth_Child_Node (Parent => Node, N => 2) = null
         then
            raise Invalid_Expression with "Operator '" & Op_Name
              & "' requires two child elements";
         end if;
         return Op (X => Boolean_Expression
                       (Node        => Get_Nth_Child_Node
                                          (Parent => Node,
                                           N      => 1),
                        Backtrace   => Backtrace,
                        Node_Access => Node_Access),
                    Y => Boolean_Expression
                       (Node        => Get_Nth_Child_Node
                                          (Parent => Node,
                                           N      => 2),
                        Backtrace   => Backtrace,
                        Node_Access => Node_Access));
      end Eval_Booleans;

      ----------------------------------------------------------------------

      function Eval_Expr return Boolean
      is
         Expr_Name : constant String
                   := DOM.Core.Elements.Get_Attribute
                        (Elem => Node,
                         Name => "name");
      begin
         if Get_Nth_Child_Node (Parent => Node, N => 1) = null then
            raise Invalid_Expression with
               "Expression '"
               & Expr_Name
               & "': Missing operator";
         end if;

         begin
            return Boolean_Expression
                     (Node        => Get_Nth_Child_Node (Parent => Node, N => 1),
                      Backtrace   => Backtrace,
                      Node_Access => Node_Access);
         exception
            when E : Invalid_Expression =>
               raise Invalid_Expression with
                  "Expression '"
                  & Expr_Name
                  & "': "
                  & Ada.Exceptions.Exception_Message (X => E);
         end;
      end Eval_Expr;

      ----------------------------------------------------------------------

      function Eval_Integers return Boolean
      is
      begin
         if Get_Nth_Child_Node (Parent => Node, N => 1) = null
            or else Get_Nth_Child_Node (Parent => Node, N => 2) = null
         then
            raise Invalid_Expression with
               "Operator '"
               & Op_Name
               & "' requires two child elements";
         end if;
         return Op (X => Int_Value
                       (Node        => Get_Nth_Child_Node
                                         (Parent => Node,
                                          N      => 1),
                            Backtrace   => Backtrace,
                            Node_Access => Node_Access),
                    Y => Int_Value
                       (Node        => Get_Nth_Child_Node
                                         (Parent => Node,
                                          N      => 2),
                            Backtrace   => Backtrace,
                            Node_Access => Node_Access));
      end Eval_Integers;

      ----------------------------------------------------------------------

      function Eval_Not return Boolean
      is
      begin
         if Get_Nth_Child_Node (Parent => Node, N => 1) = null then
            raise Invalid_Expression with
               "Operator 'not' requires one child element";
         end if;

         return not Boolean_Expression
                       (Node        => Get_Nth_Child_Node (Parent => Node, N => 1),
                        Backtrace   => Backtrace,
                        Node_Access => Node_Access);
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
            raise Invalid_Expression with
               "Invalid expression term '"
               & DOM.Core.Nodes.Node_Name (N => Node)
               & "'";
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
            | Expr_Variable   => Result := Bool_Value (Node        => Node,
                                                       Backtrace   => Backtrace,
                                                       Node_Access => Node_Access);
      end case;

      return Result;
   end Boolean_Expression;

   -------------------------------------------------------------------------

   function Evaluate_Boolean
      (Node        :        DOM.Core.Node;
       Backtrace   : in out String_Vector.Vector;
       Node_Access : in out Access_Hashmaps_Type)
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
               Result := Boolean'Value (Node_Value);
               Node_Access.Output_Boolean.Insert (Key      => Node_Name,
                                                  New_Item => Result);
            else
               declare
                  Reference : constant String
                     := Node_Value (Node_Value'First + 1 .. Node_Value'Last);
               begin
                  if Node_Access.Output_Boolean.Contains (Reference) then
                     Result := Node_Access.Output_Boolean (Reference);
                  else
                     Next_Node := Get_Defining_Node
                        (Var_Name    => Reference,
                         Node_Access => Node_Access);
                     Result := Evaluate_Boolean
                        (Node        => Next_Node,
                         Backtrace   => Backtrace,
                         Node_Access => Node_Access);
                  end if;
                  if Log_Expansion_Values then
                     Mulog.Log (Msg => "Expanding config-variable '" & Node_Name
                                   & "' with value '" & Result'Image & "'");
                  end if;
                  Node_Access.Output_Boolean.Insert (Key      => Node_Name,
                                                     New_Item => Result);
               end;
            end if;
         end;

      elsif Node_Type = "expression" then
         if Get_Expr_Type (Expr => Node) = Case_Expr_Type then
            declare
               Result_Case : Mutools.Expressions.Case_Expression.Value_Type_Tuple;
            begin
               Mutools.Expressions.Case_Expression.Case_Expression_Evaluation
                  (Expr_Node     => Node,
                   Value_Of_Case => Result_Case,
                   Backtrace     => Backtrace,
                   Node_Access   => Node_Access);
               if Result_Case.Value_Type /=
                  Mutools.Expressions.Case_Expression.Boolean_Type
               then
                  raise Muxml.Validation_Error with
                     "A Boolean variable or expression points to expression"
                     & " with name '"
                     & Node_Name
                     & "' which is not Boolean valued";
               end if;
               Result := Result_Case.Bool_Value;
            end;

         elsif Get_Expr_Type (Expr => Node) = Boolean_Expr_Type then
            Result :=  Boolean_Expression (Node        => Node,
                                           Backtrace   => Backtrace,
                                           Node_Access => Node_Access);
            if Log_Expansion_Values then
               Mulog.Log (Msg => "Expanding expression '"
                             & Node_Name
                             & "' with value '"
                             & Result'Image
                             & "'");
            end if;
            Node_Access.Output_Boolean.Insert (Key      => Node_Name,
                                               New_Item => Result);
         elsif Get_Expr_Type (Expr => Node) = Variable_Expr_Type then
            declare
               Reference : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem =>  Get_Nth_Child_Node (Parent => Node, N => 1),
                   Name => "name");
               Next_Node : DOM.Core.Node;
            begin
               if Node_Access.Output_Boolean.Contains (Reference) then
                     Result := Node_Access.Output_Boolean (Reference);
               else
                  Next_Node := Get_Defining_Node
                     (Var_Name    => Reference,
                      Node_Access => Node_Access);
                  Result := Evaluate_Boolean
                     (Node        => Next_Node,
                      Backtrace   => Backtrace,
                      Node_Access => Node_Access);
               end if;
               if Log_Expansion_Values then
                  Mulog.Log (Msg => "Expanding expression '"
                                & Node_Name
                                & "' with value '"
                                & Result'Image
                                & "'");
               end if;
               Node_Access.Output_Boolean.Insert (Key      => Node_Name,
                                                  New_Item => Result);
            end;
         else
            raise Muxml.Validation_Error with
               "A Boolean variable or expression points to expression"
               & " with name '"
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
      (Node       :        DOM.Core.Node;
       Backtrace  : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
      return Integer
   is
      use all type Mutools.Expressions.Case_Expression.Variable_Type;

      Node_Type : constant String
                := DOM.Core.Nodes.Node_Name (N => Node);
      Node_Name : constant String
                := DOM.Core.Elements.Get_Attribute
                     (Elem => Node,
                      Name => "name");
      Result    : Integer;

   begin
      Add_To_Backtrace (Backtrace => Backtrace,
                        Name      => Node_Name);
      if Node_Type = "integer" then
         declare
            Node_Value : constant String
                       :=  DOM.Core.Elements.Get_Attribute
                             (Elem => Node,
                              Name => "value");
            Next_Node  : DOM.Core.Node;
         begin
            if Node_Value (Node_Value'First) /= '$' then
               Result := Integer'Value (Node_Value);
               Node_Access.Output_Integer.Insert (Key      => Node_Name,
                                                  New_Item => Result);
            else
               declare
                  Reference : constant String
                     := Node_Value (Node_Value'First + 1 .. Node_Value'Last);
               begin
                  if Node_Access.Output_Integer.Contains (Reference) then
                     Result := Node_Access.Output_Integer (Reference);
                  else
                     Next_Node := Get_Defining_Node
                        (Var_Name    => Reference,
                         Node_Access => Node_Access);
                     Result := Evaluate_Integer
                        (Node        => Next_Node,
                         Backtrace   => Backtrace,
                         Node_Access => Node_Access);
                  end if;
                  if Log_Expansion_Values then
                     Mulog.Log (Msg => "Expanding config-variable '"
                                   & Node_Name
                                   & "' with value '"
                                   & Result'Image
                                   & "'");
                  end if;
                  Node_Access.Output_Integer.Insert (Key      => Node_Name,
                                                     New_Item => Result);
               end;
            end if;
         end;

      elsif Node_Type = "expression" then
         if Get_Expr_Type (Expr => Node) = Case_Expr_Type then
            declare
               Result_Case : Mutools.Expressions.Case_Expression.Value_Type_Tuple;
            begin
               Mutools.Expressions.Case_Expression.Case_Expression_Evaluation
                  (Expr_Node     => Node,
                   Value_Of_Case => Result_Case,
                   Backtrace     => Backtrace,
                   Node_Access   => Node_Access);
               if Result_Case.Value_Type /=
                  Mutools.Expressions.Case_Expression.Integer_Type
               then
                  raise Muxml.Validation_Error with
                     "An integer variable or expression points to expression"
                     & " with name '"
                     & Node_Name
                     & "' which is not integer valued";
               end if;
               Result := Result_Case.Int_Value;
            end;
         elsif Get_Expr_Type (Expr => Node) = Variable_Expr_Type then
            declare
               Reference : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem =>  Get_Nth_Child_Node (Parent => Node, N => 1),
                   Name => "name");
               Next_Node : DOM.Core.Node;
            begin
               if Node_Access.Output_Integer.Contains (Reference) then
                     Result := Node_Access.Output_Integer (Reference);
               else
                  Next_Node := Get_Defining_Node
                     (Var_Name    => Reference,
                      Node_Access => Node_Access);
                  Result := Evaluate_Integer
                     (Node        => Next_Node,
                      Backtrace   => Backtrace,
                      Node_Access => Node_Access);
               end if;
               if Log_Expansion_Values then
                  Mulog.Log (Msg => "Expanding expression '"
                                & Node_Name
                                & "' with value '"
                                & Result'Image
                                & "'");
               end if;
               Node_Access.Output_Integer.Insert (Key      => Node_Name,
                                                  New_Item => Result);
            end;
         else
            raise Muxml.Validation_Error with
               "An integer variable or expression points to expression"
               & " with name '"
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
      (Node        :        DOM.Core.Node;
       Backtrace   : in out String_Vector.Vector;
       Node_Access : in out Access_Hashmaps_Type)
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
      Result    : ASU.Unbounded_String;

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
                  declare
                     Reference : constant String
                        := Node_Value (Node_Value'First + 1 .. Node_Value'Last);
                  begin
                     if Node_Access.Output_String.Contains (Reference) then
                        Result := ASU.To_Unbounded_String
                           (Node_Access.Output_String (Reference));
                     else
                        Next_Node := Get_Defining_Node
                           (Var_Name    => Reference,
                            Node_Access => Node_Access);
                        Result := ASU.To_Unbounded_String
                           (Evaluate_String
                               (Node        => Next_Node,
                                Backtrace   => Backtrace,
                                Node_Access => Node_Access));
                     end if;
                     if Log_Expansion_Values then
                        Mulog.Log (Msg => "Expanding config-variable '"
                                      & Node_Name
                                      & "' with value '"
                                      & ASU.To_String (Result)
                                      & "'");
                     end if;
                     Node_Access.Output_String.Insert
                        (Key => Node_Name, New_Item => ASU.To_String (Result));
                  end;
               else
                  Result := ASU.To_Unbounded_String (Node_Value);
                  Node_Access.Output_String.Insert
                     (Key => Node_Name, New_Item => ASU.To_String (Result));
               end if;
            else
               Result := ASU.To_Unbounded_String (Node_Value);
               Node_Access.Output_String.Insert
                  (Key => Node_Name, New_Item => ASU.To_String (Result));
            end if;
         end;

      elsif Node_Type = "expression" then
         if Get_Expr_Type (Expr => Node) = Case_Expr_Type then
            declare
               Result_Case : Mutools.Expressions.Case_Expression.Value_Type_Tuple;
            begin
               Mutools.Expressions.Case_Expression.Case_Expression_Evaluation
                  (Expr_Node     => Node,
                   Value_Of_Case => Result_Case,
                   Backtrace     => Backtrace,
                   Node_Access   => Node_Access);

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
         elsif Get_Expr_Type (Expr => Node) = Variable_Expr_Type then
            declare
               Reference : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem =>  Get_Nth_Child_Node (Parent => Node, N => 1),
                   Name => "name");
               Next_Node : DOM.Core.Node;
            begin
               if Node_Access.Output_String.Contains (Reference) then
                  Result := ASU.To_Unbounded_String
                     (Node_Access.Output_String (Reference));
               else
                  Next_Node := Get_Defining_Node
                     (Var_Name    => Reference,
                      Node_Access => Node_Access);
                  Result :=  ASU.To_Unbounded_String
                     (Evaluate_String
                         (Node        => Next_Node,
                          Backtrace   => Backtrace,
                          Node_Access => Node_Access));
               end if;
               if Log_Expansion_Values then
                  Mulog.Log (Msg => "Expanding expression '"
                                & Node_Name
                                & "' with value '"
                                & ASU.To_String (Result)
                                & "'");
               end if;
               Node_Access.Output_String.Insert (Key      => Node_Name,
                                                 New_Item => ASU.To_String (Result));
            end;
         elsif Get_Expr_Type (Expr => Node) = String_Expr_Type then
            Result :=  ASU.To_Unbounded_String
               (String_Expression (Node        => Node,
                                   Backtrace   => Backtrace,
                                   Node_Access => Node_Access));
            if Log_Expansion_Values then
               Mulog.Log (Msg => "Expanding expression '"
                             & Node_Name
                             & "' with value '"
                             & ASU.To_String (Result)
                             & "'");
            end if;
            Node_Access.Output_String.Insert
                        (Key => Node_Name, New_Item => ASU.To_String (Result));
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

   procedure Expand (Policy       : Muxml.XML_Data_Type;
                     Debug_Active : Boolean := False)
   is
      Backtrace  : String_Vector.Vector;
      Vars_Exprs : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                      (N     => Policy.Doc,
                       XPath =>  "/*/config/boolean | "
                               & "/*/config/integer | "
                               & "/*/config/string | "
                               & "/*/expressions/expression");
      Node_Access : Access_Hashmaps_Type;

      ----------------------------------------------------------------------

      -- delete the current content of /*/config and replace it with
      -- the entries in Node_Access.Output_...
      procedure Substitute_Config_Section
         (Policy      :        Muxml.XML_Data_Type;
          Node_Access : in out Access_Hashmaps_Type);

      ----------------------------------------------------------------------

      procedure Substitute_Config_Section
         (Policy      :        Muxml.XML_Data_Type;
          Node_Access : in out Access_Hashmaps_Type)
      is
         Config_Node_List : constant DOM.Core.Node_List
            := McKae.XML.XPath.XIA.XPath_Query
            (N     => Policy.Doc,
             XPath => "/*/config");
         Insert_Position, Config_Entry, Config_Node, System_Node, Dummy : DOM.Core.Node;
         pragma Unreferenced (Dummy);
      begin
         if Node_Access.Output_Boolean.Is_Empty and
            Node_Access.Output_Integer.Is_Empty and
            Node_Access.Output_String.Is_Empty
         then
            return;
         end if;

         if DOM.Core.Nodes.Length (List => Config_Node_List) /= 0 then
            Config_Node := DOM.Core.Nodes.Item
               (List  => Config_Node_List,
                Index => 0);
            System_Node := DOM.Core.Nodes.Parent_Node (N => Config_Node);
            Insert_Position := DOM.Core.Nodes.Next_Sibling (N => Config_Node);

            if Expr_Debug_Active then
               Mutools.Xmldebuglog.Remove_Log_Of_Subtree (Node  => Config_Node);
            end if;
            Config_Node := DOM.Core.Nodes.Remove_Child
               (N          => System_Node,
                Old_Child  => Config_Node);
            DOM.Core.Nodes.Free (N => Config_Node);

         else
            System_Node := DOM.Core.Nodes.Item
               (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Policy.Doc,
                    XPath => "/*"),
                Index => 0);

            if DOM.Core.Nodes.Has_Child_Nodes (N => System_Node) then
               Insert_Position := DOM.Core.Nodes.First_Child (N => System_Node);
            end if;

         end if;

         Config_Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "config");

         if Insert_Position /= null then
            Config_Node := DOM.Core.Nodes.Insert_Before
               (N         => DOM.Core.Nodes.Parent_Node (N => Insert_Position),
                New_Child => Config_Node,
                Ref_Child => Insert_Position);
         else
            Config_Node := DOM.Core.Nodes.Append_Child
               (N         => DOM.Core.Nodes.Parent_Node (N => System_Node),
                New_Child => Config_Node);
         end if;

         for C in Node_Access.Output_Boolean.Iterate loop
            Config_Entry := DOM.Core.Documents.Create_Element
               (Doc      => Policy.Doc,
                Tag_Name => "boolean");
            DOM.Core.Elements.Set_Attribute
               (Elem  => Config_Entry,
                Name  => "name",
                Value => Name_To_Boolean_Hashed_Map.Key (C));
            DOM.Core.Elements.Set_Attribute
               (Elem  => Config_Entry,
                Name  => "value",
                Value => Ada.Characters.Handling.To_Lower
                   (Boolean'Image (Node_Access.Output_Boolean (C))));
            Dummy := DOM.Core.Nodes.Append_Child
               (N         => Config_Node,
                New_Child => Config_Entry);
         end loop;

         for C in Node_Access.Output_Integer.Iterate loop
            Config_Entry := DOM.Core.Documents.Create_Element
               (Doc      => Policy.Doc,
                Tag_Name => "integer");
            DOM.Core.Elements.Set_Attribute
               (Elem  => Config_Entry,
                Name  => "name",
                Value => Name_To_Integer_Hashed_Map.Key (C));
            DOM.Core.Elements.Set_Attribute
               (Elem  => Config_Entry,
                Name  => "value",
                Value => Ada.Strings.Fixed.Trim
                   (Integer'Image (Node_Access.Output_Integer (C)),
                    Ada.Strings.Both));
            Dummy := DOM.Core.Nodes.Append_Child
               (N         => Config_Node,
                New_Child => Config_Entry);
         end loop;

         for C in Node_Access.Output_String.Iterate loop
            Config_Entry := DOM.Core.Documents.Create_Element
               (Doc      => Policy.Doc,
                Tag_Name => "string");
            DOM.Core.Elements.Set_Attribute
               (Elem  => Config_Entry,
                Name  => "name",
                Value => Name_To_String_Hashed_Map.Key (C));
            DOM.Core.Elements.Set_Attribute
               (Elem  => Config_Entry,
                Name  => "value",
                Value => Node_Access.Output_String (C));
            Dummy := DOM.Core.Nodes.Append_Child
               (N         => Config_Node,
                New_Child => Config_Entry);
         end loop;

      end Substitute_Config_Section;

   begin
      Expr_Debug_Active := Debug_Active;

      for I in 0 .. DOM.Core.Nodes.Length (List => Vars_Exprs) - 1 loop
         declare
            Node : constant DOM.Core.Node
               := DOM.Core.Nodes.Item (List  => Vars_Exprs, Index => I);
            Node_Name_Attr : constant String
               := DOM.Core.Elements.Get_Attribute
                  (Elem => Node,
                   Name => "name");
         begin
            Node_Access.Input.Insert (Key => Node_Name_Attr, New_Item => Node);
         end;
      end loop;

      for C in Node_Access.Input.Iterate loop
         declare
            Key_Value : constant String
               := Name_To_Node_Hashed_Map.Key (C);
         begin
            if     not Node_Access.Output_Boolean.Contains (Key_Value)
               and not Node_Access.Output_Integer.Contains (Key_Value)
               and not Node_Access.Output_String.Contains (Key_Value)
            then
               Expand_Single_Node
                  (Node        => Node_Access.Input (C),
                   Backtrace   => Backtrace,
                   Node_Access => Node_Access);
            end if;
         end;
      end loop;

      Substitute_Config_Section (Policy      => Policy,
                                 Node_Access => Node_Access);

   end Expand;

   -------------------------------------------------------------------------

   procedure Expand_Single_Node
      (Node        :        DOM.Core.Node;
       Backtrace   : in out String_Vector.Vector;
       Node_Access : in out Access_Hashmaps_Type)
   is
      Node_Type : constant String
         :=  DOM.Core.Nodes.Node_Name (N => Node);

      ----------------------------------------------------------------------

      -- do nothing (used to discard function return value)
      procedure Discard (I : Boolean);
      procedure Discard (I : Integer);
      procedure Discard (I : String);

      ----------------------------------------------------------------------

      -- If Reference is in Node_Access.Output, then Node_Name is added
      -- to Node_Access.Output with the same value and Success is true.
      -- Otherwise Node_Access is unchanged and Success is false.
      procedure Expand_If_Known
         (Node_Name :     String;
          Reference :     String;
          Success   : out Boolean);

      ----------------------------------------------------------------------

      procedure Discard (I : Boolean)
      is
         pragma Unreferenced (I);
      begin
         null;
      end Discard;

      ----------------------------------------------------------------------

      procedure Discard (I : Integer)
      is
         pragma Unreferenced (I);
      begin
         null;
      end Discard;

      ----------------------------------------------------------------------

      procedure Discard (I : String)
      is
         pragma Unreferenced (I);
      begin
         null;
      end Discard;

      ----------------------------------------------------------------------

      procedure Expand_If_Known
         (Node_Name :     String;
          Reference :     String;
          Success   : out Boolean)
      is
      begin
         Success := False;
         if Node_Access.Output_Boolean.Contains (Reference) then
            declare
               Result : constant Boolean
                  := Node_Access.Output_Boolean (Reference);
            begin
               if Log_Expansion_Values then
                  Mulog.Log (Msg => "Expanding expression '"
                                & Node_Name
                                & "' with value '"
                                & Result'Image
                                & "'");
               end if;
               Node_Access.Output_Boolean.Insert (Key      => Node_Name,
                                                  New_Item => Result);
               Success := True;
            end;
         elsif Node_Access.Output_Integer.Contains (Reference) then
            declare
               Result : constant Integer
                  := Node_Access.Output_Integer (Reference);
            begin
               if Log_Expansion_Values then
                  Mulog.Log (Msg => "Expanding expression '"
                                & Node_Name
                                & "' with value '"
                                & Result'Image
                                & "'");
               end if;
               Node_Access.Output_Integer.Insert (Key      => Node_Name,
                                                  New_Item => Result);
               Success := True;
            end;

         elsif Node_Access.Output_String.Contains (Reference) then
            declare
               Result : constant String
                  := Node_Access.Output_String (Reference);
            begin
               if Log_Expansion_Values then
                  Mulog.Log (Msg => "Expanding expression '"
                                & Node_Name
                                & "' with value '"
                                & Result
                                & "'");
               end if;
               Node_Access.Output_String.Insert (Key      => Node_Name,
                                                 New_Item => Result);
               Success := True;
            end;
         end if;
      end Expand_If_Known;

   begin
      if    Node_Type = "boolean" then
         Discard (Evaluate_Boolean (Node        => Node,
                                    Backtrace   => Backtrace,
                                    Node_Access => Node_Access));
      elsif Node_Type = "integer" then
         Discard (Evaluate_Integer (Node        => Node,
                                    Backtrace   => Backtrace,
                                    Node_Access => Node_Access));
      elsif Node_Type = "string" then
         Discard (Evaluate_String (Node        => Node,
                                   Backtrace   => Backtrace,
                                   Node_Access => Node_Access));
      elsif Node_Type = "expression" then
         if Get_Expr_Type (Expr => Node) = Boolean_Expr_Type then
            Discard (Evaluate_Boolean (Node        => Node,
                                       Backtrace   => Backtrace,
                                       Node_Access => Node_Access));
         elsif Get_Expr_Type (Expr => Node) = Case_Expr_Type then
            declare
               Dummy : Mutools.Expressions.Case_Expression.Value_Type_Tuple;
               Node_Name : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem => Node,
                   Name => "name");
            begin
               Add_To_Backtrace (Backtrace => Backtrace, Name => Node_Name);
               Mutools.Expressions.Case_Expression.Case_Expression_Evaluation
                  (Expr_Node     => Node,
                   Value_Of_Case => Dummy,
                   Backtrace     => Backtrace,
                   Node_Access   => Node_Access);
               pragma Unreferenced (Dummy);
               String_Vector.Delete_Last (Container => Backtrace);
            end;
         elsif Get_Expr_Type (Expr => Node) = Variable_Expr_Type then
            declare
               Reference : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem =>  Get_Nth_Child_Node (Parent => Node, N => 1),
                   Name => "name");
               Node_Name : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem => Node,
                   Name => "name");
               Success : Boolean;
               Next_Node : DOM.Core.Node;
            begin
               Expand_If_Known
                  (Node_Name => Node_Name,
                   Reference => Reference,
                   Success => Success);
               if not Success then
                  Next_Node := Get_Defining_Node
                     (Var_Name    => Reference,
                      Node_Access => Node_Access);
                  Add_To_Backtrace (Backtrace => Backtrace, Name => Node_Name);
                  Expand_Single_Node
                     (Node        => Next_Node,
                      Backtrace   => Backtrace,
                      Node_Access => Node_Access);
                  String_Vector.Delete_Last (Container => Backtrace);

                  Expand_If_Known
                     (Node_Name => Node_Name,
                      Reference => Reference,
                      Success => Success);
                  if not Success then
                     raise Program_Error with
                        "Unexpected exception when expanding expression "
                        & "with name '"
                        & Node_Name
                        & "'";
                  end if;
               end if;
            end;

         else
            Discard (Evaluate_String (Node        => Node,
                                      Backtrace   => Backtrace,
                                      Node_Access => Node_Access));
         end if;
      else
         raise Invalid_Expression with
            "Invalid config-variable or expression with type '"
            & Node_Type
            & "'";
      end if;
   exception
      when E : others =>
         if Expr_Debug_Active then
            -- check if the exception got amended by a child-call already
            declare
               Debug_Info_Header : constant String
                  := ". See above message for details.";
               Has_Been_Amended : constant Boolean
                  := (Ada.Strings.Fixed.Count
                         (Source  => Ada.Exceptions.Exception_Message (X => E),
                          Pattern => Debug_Info_Header) > 0);
            begin
               if Has_Been_Amended then
                  raise;
               else
                  Mulog.Log (Msg => Mutools.Xmldebuglog.Get_Log_For_Error_Message (Node => Node));
                  Ada.Exceptions.Raise_Exception
                     (E       => Ada.Exceptions.Exception_Identity (X => E),
                      Message => Ada.Exceptions.Exception_Message (X => E)
                         & Debug_Info_Header);
               end if;
            end;
         else
            raise;
         end if;

   end Expand_Single_Node;

   -------------------------------------------------------------------------

   function Get_Defining_Node
      (Var_Name    :        String;
       Node_Access : in out Access_Hashmaps_Type)
      return DOM.Core.Node
   is
   begin
      if Node_Access.Input.Contains (Var_Name) then
         return Node_Access.Input (Var_Name);
      else
         raise Muxml.Validation_Error with
            "A variable or expression points to '"
            & Var_Name
            & "' which does not exit";
      end if;
   end Get_Defining_Node;

   ----------------------------------------------------------------------

   function Get_Expr_Type (Expr : DOM.Core.Node) return Expression_Toplevel_Type
   is
      First_Child : constant DOM.Core.Node
                  :=  Get_Nth_Child_Node (Parent => Expr, N => 1);
   begin
      if First_Child = null then
         raise Invalid_Expression with
            "Expression with name "
            & DOM.Core.Elements.Get_Attribute
                (Elem => Expr,
                 Name => "name")
            & " is empty.";
      end if;
      declare
         Child_Name : constant String := DOM.Core.Nodes.Node_Name (N => First_Child);
      begin
         if Child_Name = "concatenation" or
            Child_Name = "evalString"
         then
            return String_Expr_Type;
         elsif Child_Name = "eq" or
            Child_Name = "ne" or
            Child_Name = "gt" or
            Child_Name = "lt" or
            Child_Name = "and" or
            Child_Name = "or" or
            Child_Name = "not"
         then
            return Boolean_Expr_Type;
         elsif Child_Name = "case" then
            return Case_Expr_Type;
         elsif Child_Name = "variable" then
            return Variable_Expr_Type;
         else
            raise Invalid_Expression with
               "Expression with name "
               & DOM.Core.Elements.Get_Attribute
               (Elem => Expr,
                Name => "name")
               & " begins with illegal operator "
               & Child_Name;
         end if;
      end;

   end  Get_Expr_Type;

   -------------------------------------------------------------------------

   function Get_Nth_Child_Node
      (Parent : DOM.Core.Node;
       N      : Positive)
      return DOM.Core.Node
   is
      use type DOM.Core.Node_Types;

      Child : DOM.Core.Node
         := DOM.Core.Nodes.First_Child (N => Parent);

      -- go to next sibling until an element-node is reached
      -- does not change node if starting node is an element node
      procedure Loop_Until_Element (N : in out DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Loop_Until_Element (N : in out DOM.Core.Node)
      is
      begin
         while DOM.Core.Nodes.Node_Type (N => N)
            /= DOM.Core.Element_Node
         loop
            N := DOM.Core.Nodes.Next_Sibling (N => N);
            exit when  N = null;
         end loop;
      end Loop_Until_Element;

   begin
      Loop_Until_Element (N => Child);

      for I in 2 .. N loop
         Child := DOM.Core.Nodes.Next_Sibling (N => Child);
         Loop_Until_Element (N => Child);
      end loop;

      return Child;
   end Get_Nth_Child_Node;

   -------------------------------------------------------------------------

   function Int_Value
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
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
            raise Invalid_Expression with
               "Invalid integer type '"
               & DOM.Core.Nodes.Node_Name (N => Node)
               & "'";
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
               if Node_Access.Output_Integer.Contains (Var_Name) then
                  Result := Node_Access.Output_Integer (Var_Name);
               else
                  Result := Evaluate_Integer
                     (Node        => Get_Defining_Node
                         (Var_Name    => Var_Name,
                          Node_Access => Node_Access),
                      Backtrace   => Backtrace,
                      Node_Access => Node_Access);
               end if;
            end;
      end case;

      return Result;
   end Int_Value;

   -------------------------------------------------------------------------

   function Parse_Dollar_Braced_References (Input_String : String)
                                           return Fragment_Vector.Vector
   is
      Left_Index, Right_Index : Natural := Input_String'First;
      Result : Fragment_Vector.Vector;

      -----------------------------------------------------------------

      -- check if S contains $ or { or }
      function Has_Dollar_Or_Braces (Input : String) return Boolean;

      -----------------------------------------------------------------

      function Has_Dollar_Or_Braces (Input : String) return Boolean
      is
      begin
         return (0 /= Ada.Strings.Fixed.Index
                    (Source  => Input,
                     Pattern => "$",
                     From    => Input'First)
                    + Ada.Strings.Fixed.Index
                    (Source  => Input,
                     Pattern => "{",
                     From    => Input'First)
                    + Ada.Strings.Fixed.Index
                    (Source  => Input,
                     Pattern => "}",
                     From    => Input'First));
      end Has_Dollar_Or_Braces;

   begin
      Right_Index := Ada.Strings.Fixed.Index
         (Source  => Input_String,
          Pattern => "${",
          From    => Left_Index);

      while Right_Index /= 0 loop
         declare
            String_Fragment : constant String
               := Input_String (Left_Index .. Right_Index - 1);
         begin
            if Has_Dollar_Or_Braces (Input => String_Fragment) then
               raise Invalid_Expression with
                  "EvalString got invaild value '" & Input_String & "'";
            elsif String_Fragment'Length > 0 then
               Fragment_Vector.Append
                  (Container => Result,
                   New_Item  => Fragment_Entry'
                      (Value => String_Holder_Type.To_Holder (String_Fragment),
                       Value_Type => Text_Type));
            end if;
         end;

         Left_Index := Right_Index + String'("${")'Length;
         Right_Index := Ada.Strings.Fixed.Index
            (Source  => Input_String,
             Pattern => "}",
             From    => Left_Index);

         if Right_Index = 0 then
            raise Invalid_Expression with
               "EvalString got invaild value '" & Input_String & "'";
         end if;

         declare
            String_Fragment : constant String
               := Input_String (Left_Index .. Right_Index - 1);
         begin
            if Has_Dollar_Or_Braces (Input => String_Fragment) then
               raise Invalid_Expression with
                  "EvalString got invaild value '" & Input_String & "'";
            elsif String_Fragment'Length > 0 then
               Fragment_Vector.Append
                  (Container => Result,
                   New_Item  => Fragment_Entry'
                      (Value => String_Holder_Type.To_Holder (String_Fragment),
                       Value_Type => Reference_Type));
            end if;
         end;

         Left_Index  := Right_Index + String'("}")'Length;
         Right_Index := Ada.Strings.Fixed.Index
            (Source  => Input_String,
             Pattern => "${",
             From    => Left_Index);
      end loop;

      declare
         String_Fragment : constant String
            := Input_String (Left_Index .. Input_String'Last);
      begin
         if Has_Dollar_Or_Braces (Input => String_Fragment) then
            raise Invalid_Expression with
               "EvalString got invaild value '" & Input_String & "'";
         elsif String_Fragment'Length > 0 then
            Fragment_Vector.Append
               (Container => Result,
                New_Item  => Fragment_Entry'
                   (Value => String_Holder_Type.To_Holder (String_Fragment),
                    Value_Type => Text_Type));
         end if;
      end;

      return Result;
   end Parse_Dollar_Braced_References;

   -------------------------------------------------------------------------

   function String_Expression
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
     return String
   is
      package ASU renames Ada.Strings.Unbounded;

      Children  : constant DOM.Core.Node_List
                := McKae.XML.XPath.XIA.XPath_Query
                      (N     => Node,
                       XPath => "./*");

      ---------------------------------------------------------------------

      -- evaluate a <concatenation>-node within the expression
      function Evaluate_Concatenation
         (Node      :        DOM.Core.Node;
          Backtrace : in out String_Vector.Vector)
         return String;

      ---------------------------------------------------------------------

      function Evaluate_Concatenation
         (Node      :        DOM.Core.Node;
          Backtrace : in out String_Vector.Vector)
         return String
      is
         Children  : constant DOM.Core.Node_List
            := McKae.XML.XPath.XIA.XPath_Query
            (N     => Node,
             XPath => "./*");
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
                      (String_Value (Node        => Child,
                                     Backtrace   => Backtrace,
                                     Node_Access => Node_Access)));
            end;
         end loop;
         return ASU.To_String (Result);
      end Evaluate_Concatenation;

      ---------------------------------------------------------------------

      -- evaluate an <evalString>-node within the expression
      function Evaluate_Eval_String
         (Node      :        DOM.Core.Node;
          Backtrace : in out String_Vector.Vector)
         return String;

      ---------------------------------------------------------------------

      function Evaluate_Eval_String
         (Node      :        DOM.Core.Node;
          Backtrace : in out String_Vector.Vector)
         return String
      is
         Result    : ASU.Unbounded_String;
         Input_String : constant String
            := DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "value");
         Parsed_Fragments : constant Fragment_Vector.Vector
            := Parse_Dollar_Braced_References (Input_String => Input_String);

      begin
         if not Muxml.Utils.Has_Attribute
                  (Node      => Node,
                   Attr_Name => "value")
         then
            raise Invalid_Expression with
               "EvalString-expression '"
               & DOM.Core.Elements.Get_Attribute
               (Elem   => Node,
                Name   => "name")
               & "' has evalString child without value attribute.";
         end if;

         for Fragment of Parsed_Fragments loop
            if Fragment.Value_Type = Text_Type then
               ASU.Append (Source   => Result,
                           New_Item => Fragment.Value.Element);
            else
               declare
                  Ref_Value : ASU.Unbounded_String;
               begin
                  if Node_Access.Output_String.Contains (Fragment.Value.Element)
                  then
                     Ref_Value := ASU.To_Unbounded_String
                        (Node_Access.Output_String (Fragment.Value.Element));
                  else
                     Ref_Value :=  ASU.To_Unbounded_String
                     (Evaluate_String
                        (Node        => Get_Defining_Node
                            (Var_Name    => Fragment.Value.Element,
                             Node_Access => Node_Access),
                         Backtrace   => Backtrace,
                         Node_Access => Node_Access));
                  end if;
                  ASU.Append (Source   => Result,
                              New_Item => Ref_Value);
               end;
            end if;
         end loop;

         return ASU.To_String (Result);
      end  Evaluate_Eval_String;

   begin
      if DOM.Core.Nodes.Length (List => Children) /= 1 then
         raise Invalid_Expression with
            "String-expression '"
            & DOM.Core.Elements.Get_Attribute
            (Elem => Node,
             Name => "name")
            & "' does not have one unique child";
      end if;

      declare
         Child  : constant DOM.Core.Node
            := DOM.Core.Nodes.Item (List  => Children,
                                    Index => 0);
         Child_Name : constant String
            := DOM.Core.Nodes.Node_Name (N => Child);
      begin
         if Child_Name = "concatenation" then
            return Evaluate_Concatenation
               (Node      => Child,
                Backtrace => Backtrace);
         elsif Child_Name = "evalString" then
            return Evaluate_Eval_String
               (Node      => Child,
                Backtrace => Backtrace);
         else
            raise Invalid_Expression with
               "String-expression '"
               & DOM.Core.Elements.Get_Attribute
               (Elem => Node,
                Name => "name")
               & "' has an unknown child operation with name '"
               & Child_Name
               & "'";
         end if;
      end;
   end String_Expression;

   -------------------------------------------------------------------------

   function String_Value
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
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
            if Node_Access.Output_String.Contains (Var_Name) then
               return Node_Access.Output_String (Var_Name);
            else
               return Evaluate_String
                  (Node        => Get_Defining_Node (Var_Name    => Var_Name,
                                                     Node_Access => Node_Access),
                   Backtrace   => Backtrace,
                   Node_Access => Node_Access);
            end if;
         end;
      else
         raise Invalid_Expression with
            "Invalid string type '"
            & DOM.Core.Nodes.Node_Name (N => Node)
            & "'";
      end if;
   end String_Value;

end Mutools.Expressions;
