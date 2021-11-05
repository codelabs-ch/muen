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

private with DOM.Core;
with Ada.Containers.Indefinite_Vectors;
with Muxml;

package Mutools.Expressions
is

   --  Expand all expressions in the specified policy to boolean or string
   --  config values.
   --  This includes the substitutions of $-referenced variables.
   procedure Expand (Policy : Muxml.XML_Data_Type);

   Invalid_Expression : exception;

private
   --  A string-vector is used to store a backtrace of the recursive path
   --  used for evaluation. The backtrace is used for cycle detection
   --  and to report a useful error in case of a cycle.
   package String_Vector is new Ada.Containers.Indefinite_Vectors
      (Element_Type => String,
       Index_Type   => Natural);
   use all type String_Vector.Vector;

   --  Returns the value of the config variable reference or boolean element
   --  given as node.
   function Bool_Value
     (Policy    :        Muxml.XML_Data_Type;
      Node      :        DOM.Core.Node;
      Backtrace : in out String_Vector.Vector)
      return Boolean;

   --  Returns the value of the config variable reference or integer element
   --  given as node.
   function Int_Value
     (Policy    :        Muxml.XML_Data_Type;
      Node      :        DOM.Core.Node;
      Backtrace : in out String_Vector.Vector)
     return Integer;

   --  Returns the value of the config variable reference or integer element
   --  given as node.
   function String_Value
     (Policy    :        Muxml.XML_Data_Type;
      Node      :        DOM.Core.Node;
      Backtrace : in out String_Vector.Vector)
      return String;

   --  Returns the Boolean value of a Boolean expression or config variable
   --  given as node and adds value to config section.
   --  Resolves dependencies recusively.
   function Evaluate_Boolean
     (Policy    :        Muxml.XML_Data_Type;
      Node      :        DOM.Core.Node;
      Backtrace : in out String_Vector.Vector)
      return Boolean;

   --  Returns the integer value of an integer expression or config variable
   --  given as node and adds value to config section.
   --  Resolves dependencies recusively.
   function Evaluate_Integer
     (Policy    :        Muxml.XML_Data_Type;
      Node      :        DOM.Core.Node;
      Backtrace : in out String_Vector.Vector)
     return Integer;

   --  Returns the string value of a string expression or config variable
   --  given as node and adds value to config section.
   --  Resolves dependencies recusively.
   function Evaluate_String
     (Policy    :        Muxml.XML_Data_Type;
      Node      :        DOM.Core.Node;
      Backtrace : in out String_Vector.Vector)
     return String;

   -- Evaluates Boolean expression recursively.
   function Boolean_Expression
      (Policy    :        Muxml.XML_Data_Type;
       Node      :        DOM.Core.Node;
       Backtrace : in out String_Vector.Vector)
      return Boolean;

   -- Evaluates string expression recursively.
   function String_Expression
      (Policy    :        Muxml.XML_Data_Type;
       Node      :        DOM.Core.Node;
       Backtrace : in out String_Vector.Vector)
      return String;

   -- Returns a config-variable or expression node with the given name attribute.
   -- If there exists a matching config variable, than that is returned.
   -- Otherwise, expressions are considered.
   -- Raises an exception if no match is found.
   function Get_Defining_Node
      (Policy   : Muxml.XML_Data_Type;
       Var_Name : String)
      return DOM.Core.Node;

   -- determine whether the given expression defines a Boolean or a
   -- string value
   function  Get_Expr_Type
      (Expr :  DOM.Core.Node)
      return String;

   -- handles the addition of a string to the backtrace and handles errors
   procedure Add_To_Backtrace
      (Backtrace : in out String_Vector.Vector;
       Name      :        String);

end Mutools.Expressions;
