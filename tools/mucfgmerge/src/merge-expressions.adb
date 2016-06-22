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

with DOM.Core.Elements;
with DOM.Core.Nodes;

with Mutools.System_Config;

package body Merge.Expressions
is

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
            Result := Mutools.System_Config.Get_Value
              (Data => Policy,
               Name => DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "name"));
      end case;

      return Result;
   end Int_Value;

end Merge.Expressions;
