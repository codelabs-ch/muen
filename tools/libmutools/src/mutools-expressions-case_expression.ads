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

   type Value_Type_Tuple is record
      Bool_Value     : Boolean;
      Int_Value      : Integer;
      String_Value   : String_Holder_Type.Holder;
      Value_Type     : Variable_Type;
   end record;

   -- check if L and R have the same Value_Type and if their entries
   -- corresponding to that value are equal
   function "=" (L, R : Value_Type_Tuple) return Boolean;

   -- return a string representation of VTT, including Type and its value
   function To_String (VTT : Value_Type_Tuple) return String;

   -- Check some assumptions for the given <case>-Node
   --   and return the 'when'-child that matches.
   -- This function is used for <case>-statements inside
   --   and outside of expressions.
   -- Return_Node is Null if no child matches.
   -- If Guarantee_Match is True, formal requirements to guarantee a match are
   --   checked (e.g. existence of 'when others')
   --   and an exception is raised if these are not met.
   -- The following always raise an exception:
   --  (0) there is no when statement
   --  (1) the case variable is Boolean and the options are
   --     "true", "false", "others"  (should not contain "others")
   --  (2) 'when others' is not the last option
   --  (3) there are two options (excluding 'others') which have the same
   --      actual value.
   procedure Evaluate_Case_Node_Frame
      (Policy          :        Muxml.XML_Data_Type;
       Case_Node       :        DOM.Core.Node;
       Guarantee_Match :        Boolean;
       Return_Node     :    out DOM.Core.Node;
       Backtrace       : in out String_Vector.Vector);

   -- Evaluate a Case-Statement within an expression recursively.
   procedure Evaluate_Case_Node
      (Policy        :        Muxml.XML_Data_Type;
       Case_Node     :        DOM.Core.Node;
       Value_Of_Case :    out Value_Type_Tuple;
       Backtrace     : in out String_Vector.Vector);

   -- Evaluate an Expression of type "Case", i.e., an expression containing
   -- <case> as its child.
   -- Adds resulting value to config-section of Policy.
   procedure Case_Expression_Evaluation
      (Policy        :        Muxml.XML_Data_Type;
       Expr_Node     :        DOM.Core.Node;
       Value_Of_Case :    out Value_Type_Tuple;
       Backtrace     : in out String_Vector.Vector);

private
      -- can be called on nodes like <boolean value="foo"/> as well as
      --   config-variable entries like <boolean name="varname" value="foo"/>
      --   independet of the type of the variable.
      -- fills Type_And_Value with the respective type-value tuple.
      -- If value begins with '$', an error will be reported.
      procedure Get_Type_And_Value
         (Node           :     DOM.Core.Node;
          Type_And_Value : out Value_Type_Tuple);

end Mutools.Expressions.Case_Expression;
