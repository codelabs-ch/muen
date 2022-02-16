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

with DOM.Core;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Muxml;

use all type DOM.Core.Node;

package Mutools.Expressions
is

   --  Expand all expressions in the specified policy to config values.
   --  This includes the substitutions of $-referenced variables within
   --  expressions and within the config section
   procedure Expand (Policy : Muxml.XML_Data_Type);

   Invalid_Expression : exception;

   --  A string-vector is used to store a backtrace of the recursive path
   --  used for evaluation. The backtrace is used for cycle detection
   --  and to report a useful error in case of a cycle.
   package String_Vector is new Ada.Containers.Indefinite_Vectors
      (Element_Type => String,
       Index_Type   => Natural);

   -- Fragment Vectors hold parts ("framents") of a string,
   -- where each part is annotated with their type,
   -- i.e., whether it is a reference to some variable or just text
   package String_Holder_Type is new Ada.Containers.Indefinite_Holders
      (Element_Type => String);
   type Fragment_Type is (Text_Type, Reference_Type);
   type Fragment_Entry is record
      Value      : String_Holder_Type.Holder;
      Value_Type : Fragment_Type;
   end record;
   package Fragment_Vector is new Ada.Containers.Indefinite_Vectors
      (Element_Type => Fragment_Entry,
       Index_Type   => Natural);

   -- Parse strings of the form "text1_${ref1}_text2_${ref2}" into a vector
   -- of the form (("text1_", Text_Type), ("ref1", Reference_Type), ...)
   function Parse_Dollar_Braced_References (Input_String : String)
                                           return Fragment_Vector.Vector;

   -- hashmap for fast access to config variables and expressions by 'name'
   -- attribute
   package Name_To_Node_Hashed_Map is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => DOM.Core.Node,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");

   -- hashmaps for fast access to the evaluation results of expressions and
   -- config variables (one map per type to avoid sorting lateron)
   package Name_To_Boolean_Hashed_Map is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Boolean,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");
   package Name_To_Integer_Hashed_Map is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Integer,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");
   package Name_To_String_Hashed_Map is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => String,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");

   -- wrapper for hash-maps
   type Access_Hashmaps_Type is record
      Input          : Name_To_Node_Hashed_Map.Map;
      Output_Boolean : Name_To_Boolean_Hashed_Map.Map;
      Output_Integer : Name_To_Integer_Hashed_Map.Map;
      Output_String  : Name_To_String_Hashed_Map.Map;
   end record;

private
   use all type String_Vector.Vector;

   --  Returns the value of the config variable reference or <boolean>-element
   --  given as node.
   function Bool_Value
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
      return Boolean;

   --  Returns the value of the config variable reference or <integer>-element
   --  given as node.
   function Int_Value
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
     return Integer;

   --  Returns the value of the config variable reference or <string>-element
   --  given as node.
   function String_Value
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
      return String;

   --  Returns the Boolean value of a Boolean expression or config variable
   --  given as node and adds name-value entry to config section.
   --  Resolves dependencies recusively.
   function Evaluate_Boolean
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
      return Boolean;

   --  Returns the integer value of an integer expression or config variable
   --  given as node and adds name-value entry to config section.
   --  Resolves dependencies recusively.
   function Evaluate_Integer
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
     return Integer;

   --  Returns the string value of a string expression or config variable
   --  given as node and adds name-value entry to config section.
   --  Resolves dependencies recusively.
   function Evaluate_String
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
     return String;

   -- Evaluates Boolean expression recursively.
   function Boolean_Expression
     (Node        :        DOM.Core.Node;
      Backtrace   : in out String_Vector.Vector;
      Node_Access : in out Access_Hashmaps_Type)
     return Boolean;

   -- Evaluates string expression recursively.
   function String_Expression
      (Node        :        DOM.Core.Node;
       Backtrace   : in out String_Vector.Vector;
       Node_Access : in out Access_Hashmaps_Type)
      return String;

   -- Returns a config-variable or expression node with the given name attribute.
   -- If there exists a matching config variable, than that is returned.
   -- Otherwise, expressions are considered.
   -- Raises an exception if no match is found.
   function Get_Defining_Node
      (Var_Name    :        String;
       Node_Access : in out Access_Hashmaps_Type)
      return DOM.Core.Node;

   -- define types that determine how an expression is handled.
   -- Note: There are no expressions with integer type.
   type Expression_Toplevel_Type is
      (Boolean_Expr_Type, String_Expr_Type, Case_Expr_Type, Variable_Expr_Type);

   -- determine whether the given expression-node defines a Boolean value, a
   -- string value, or is of unknown type (in case of a case statement)
   function Get_Expr_Type
      (Expr :  DOM.Core.Node)
      return Expression_Toplevel_Type;

   -- return the Nth child of node Parent which is of type Element_Node
   -- and null if no such node exists.
   -- Use N => 1 for the first child.
   function Get_Nth_Child_Node
      (Parent : DOM.Core.Node;
       N      : Positive)
      return DOM.Core.Node;

   -- provides all functionality for 'Expand', except for the outer loop,
   -- i.e., it checks which type the given Node has and calls the
   -- responsible evaluation function on that node.
   procedure Expand_Single_Node
      (Node        :        DOM.Core.Node;
       Backtrace   : in out String_Vector.Vector;
       Node_Access : in out Access_Hashmaps_Type);

   -- handles the addition of a string to the backtrace and handles errors
   procedure Add_To_Backtrace
      (Backtrace : in out String_Vector.Vector;
       Name      :        String);

end Mutools.Expressions;
