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
private with Mutools.Expressions;

with Muxml;

package Mutools.Conditionals
is

   --  Expand all conditionals in the specified policy.
   procedure Expand
      (Policy       : Muxml.XML_Data_Type;
       Debug_Active : Boolean := False);

private

   --  Recursively evaluate all conditionals of given parent node.
   procedure Evaluate
      (Policy       :        Muxml.XML_Data_Type;
       Config       :        DOM.Core.Node_List;
       Parent       :        DOM.Core.Node;
       Node_Access  : in out Mutools.Expressions.Access_Hashmaps_Type;
       Debug_Active :        Boolean);

end Mutools.Conditionals;
