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

with DOM.Core;

package Expanders.Utils
is

   type Number_Allocator_Type (Range_Start, Range_End : Natural) is private
   with Type_Invariant =>
      Number_Allocator_Type.Range_Start < Number_Allocator_Type.Range_End;

   --  Return next free number from given allocator and mark it as allocated.
   --  Raises an exception if no free number is available.
   procedure Allocate
     (Allocator : in out Number_Allocator_Type;
      Number    :    out Natural);

   --  Return the start and end index of a free range with the specified size
   --  from the given allocator. All numbers of the range are marked as
   --  allocated. Raises an exception if no free range of the given size is
   --  available.
   procedure Allocate_Range
     (Allocator   : in out Number_Allocator_Type;
      Range_Size  :        Positive;
      Range_Start :    out Natural;
      Range_End   :    out Natural);

   --  Marks the given number as reserved.
   procedure Reserve_Number
     (Allocator : in out Number_Allocator_Type;
      Number    :        Natural);

   --  The procedure marks numbers as reserved. The numbers to reserve are
   --  specified by a common attribute of the nodes in the specified list.
   --  Raises an exception if the given attribute does not exists or does not
   --  designate a number.
   procedure Reserve_Numbers
     (Allocator : in out Number_Allocator_Type;
      Nodes     :        DOM.Core.Node_List;
      Attribute :        String);

   No_Free_Number    : exception;
   Invalid_Attribute : exception;

private

   type Numbers_Array is array (Natural range <>) of Boolean;

   type Number_Allocator_Type (Range_Start, Range_End : Natural) is record
      Numbers : Numbers_Array (Range_Start .. Range_End) := (others => True);
   end record;

end Expanders.Utils;
