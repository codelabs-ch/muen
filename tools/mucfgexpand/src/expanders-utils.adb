--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with DOM.Core.Elements;

package body Expanders.Utils
is

   -------------------------------------------------------------------------

   procedure Allocate
     (Allocator : in out Number_Allocator_Type;
      Number    :    out Natural)
   is
   begin
      for I in Allocator.Numbers'Range loop
         if Allocator.Numbers (I) then
            Allocator.Numbers (I) := False;
            Number := I;
            return;
         end if;
      end loop;

      raise No_Free_Number;
   end Allocate;

   -------------------------------------------------------------------------

   procedure Allocate_Range
     (Allocator   : in out Number_Allocator_Type;
      Range_Size  :        Positive;
      Range_Start :    out Natural;
      Range_End   :    out Natural)
   is
   begin
      Range_End := Allocator.Range_End;

      for I in Allocator.Range_Start .. Allocator.Range_End - Range_Size + 1
      loop
         Range_Start := I;

         Check_Free_Range :
         for J in I .. I + Range_Size - 1 loop
            exit Check_Free_Range when not Allocator.Numbers (J);
            Range_End := J;
         end loop Check_Free_Range;

         if Range_End = Range_Start + Range_Size - 1 then

            --  Found range that has the requested size.

            for J in Range_Start .. Range_End loop
               Allocator.Numbers (J) := False;
            end loop;

            return;
         end if;
      end loop;

      raise No_Free_Number;
   end Allocate_Range;

   -------------------------------------------------------------------------

   procedure Reserve_Number
     (Allocator : in out Number_Allocator_Type;
      Number    :        Natural)
   is
   begin
      if Number in Allocator.Numbers'Range then
         Allocator.Numbers (Number) := False;
      end if;
   end Reserve_Number;

   -------------------------------------------------------------------------

   procedure Reserve_Numbers
     (Allocator : in out Number_Allocator_Type;
      Nodes     :        DOM.Core.Node_List;
      Attribute :        String)
   is
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Nodes) loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I - 1);
            Number_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => Attribute);
            Number : Natural;
         begin
            Number := Natural'Value (Number_Str);

            Reserve_Number (Allocator => Allocator,
                            Number    => Number);

         exception
            when others =>
               raise Invalid_Attribute with "Node '" & DOM.Core.Nodes.Node_Name
                 (N => Node) & "' has no valid number attribute '"
                 & Attribute &  "'";
         end;
      end loop;
   end Reserve_Numbers;

end Expanders.Utils;
