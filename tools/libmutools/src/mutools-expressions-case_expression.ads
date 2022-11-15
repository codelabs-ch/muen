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
with DOM.Core;
with Ada.Containers.Indefinite_Holders;

package Mutools.Expressions.Case_Expression
is
   type Variable_Type is (Boolean_Type, Integer_Type, String_Type);

   package String_Holder_Type is new Ada.Containers.Indefinite_Holders
      (Element_Type => String);

   --  Container for values of case-expressions
   --  as their type can only be determined during evaluation.
   type Value_Type_Tuple is record
      Bool_Value   : Boolean;
      Int_Value    : Integer;
      String_Value : String_Holder_Type.Holder;
      Value_Type   : Variable_Type;
   end record;

   --  Check if L and R have the same Value_Type and if their value in
   --  that type are equal.
   function "=" (L, R : Value_Type_Tuple) return Boolean;

   --  Return a string representation of VTT, including Type and its value.
   --  When No_Type is true, the type is omitted in the output.
   function To_String
       (VTT     : Value_Type_Tuple;
        No_Type : Boolean := False)
       return String;

   --  Evaluate 'case' and 'when'-nodes, but not the children of 'when'-nodes.
   --  Return_Node is the matching 'when'-node and null if no child matches.
   --  This function is used for <case>-statements inside
   --  and outside of expressions.
   procedure Evaluate_Case_Node_Frame
      (Case_Node   :        DOM.Core.Node;
       Return_Node :    out DOM.Core.Node;
       Backtrace   : in out String_Vector.Vector;
       Node_Access : in out Access_Hashmaps_Type);

   --  Evaluate an Expression of type "Case", i.e., an expression containing
   --  <case> as its child.
   --  Adds the resulting value to Node_Access.Output.
   procedure Case_Expression_Evaluation
      (Expr_Node     :        DOM.Core.Node;
       Value_Of_Case :    out Value_Type_Tuple;
       Backtrace     : in out String_Vector.Vector;
       Node_Access   : in out Access_Hashmaps_Type);

   --  This function is an interface for the debug-functionality.
   --  Return the value of Ref_Name as a string.
   --  If Ref_Name is not a valid key in Hashmap then the empty string
   --  is returned (which is also a legal value for existing variables of type
   --  string).
   function Get_Value_Of_Reference_Debug
      (Ref_Name    : String;
       Node_Access : Access_Hashmaps_Type)
      return String;

end Mutools.Expressions.Case_Expression;
