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

with Muxml;

package Merge.Expressions
is

   --  Expand all expressions in the specified policy to boolean config values.
   procedure Expand (Policy : Muxml.XML_Data_Type);

   --  Returns the value of the config variable reference or integer element
   --  given as node.
   function Int_Value
     (Policy : Muxml.XML_Data_Type;
      Node   : DOM.Core.Node)
      return Integer;

   --  Returns the boolean value of the expression given as node.
   function Expression
     (Policy : Muxml.XML_Data_Type;
      Node   : DOM.Core.Node)
      return Boolean;

   Invalid_Expression : exception;

end Merge.Expressions;
