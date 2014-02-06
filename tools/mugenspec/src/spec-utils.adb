--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Nodes;

package body Spec.Utils
is

   -------------------------------------------------------------------------

   function To_Number
     (Fields  : DOM.Core.Node_List;
      Default : Interfaces.Unsigned_64 := 0)
      return Interfaces.Unsigned_64
   is
      Result : Interfaces.Unsigned_64 := Default;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Fields) - 1 loop
         declare
            Flag_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Fields,
               Index => I);
            Flag_Name : constant Bitfield_Type := Bitfield_Type'Value
              (DOM.Core.Nodes.Node_Name (N => Flag_Node));
            Flag_Value : constant String       := DOM.Core.Nodes.Node_Value
              (N => DOM.Core.Nodes.First_Child (N => Flag_Node));
         begin
            if Flag_Value = "1" then
               Result := Mutools.Utils.Bit_Set
                 (Value => Result,
                  Pos   => Map (Flag_Name));
            else
               Result := Mutools.Utils.Bit_Clear
                 (Value => Result,
                  Pos   => Map (Flag_Name));
            end if;
         end;
      end loop;

      return Result;
   end To_Number;

end Spec.Utils;
