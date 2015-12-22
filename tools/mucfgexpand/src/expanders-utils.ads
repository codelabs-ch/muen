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

package Expanders.Utils
is

   type Number_Allocator_Type (Range_Start, Range_End : Natural) is private
   with Type_Invariant =>
      Number_Allocator_Type.Range_Start < Number_Allocator_Type.Range_End;

   --  Return next free number from given allocator and mark it as allocated.
   --  Raises an  exception of no free number is available.
   procedure Allocate
     (Allocator : in out Number_Allocator_Type;
      Number    :    out Natural);

   No_Free_Number : exception;

private

   type Numbers_Array is array (Natural range <>) of Boolean;

   type Number_Allocator_Type (Range_Start, Range_End : Natural) is record
      Numbers : Numbers_Array (Range_Start .. Range_End) := (others => True);
   end record;

end Expanders.Utils;
