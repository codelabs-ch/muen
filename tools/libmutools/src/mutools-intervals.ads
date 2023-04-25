--
--  Copyright (C) 2023 secunet Security Networks AG
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

with Ada.Containers.Doubly_Linked_Lists;

with Interfaces;
use type Interfaces.Unsigned_64;

package Mutools.Intervals
is
   --  Models an interval (endpoints included).
   --  If First_Element > Last_Element the interval is empty.
   type Interval_Type is
   record
      First_Element : Interfaces.Unsigned_64;
      Last_Element  : Interfaces.Unsigned_64;
   end record;

   --  Models a list of pairwise disjoint nonadjacent intervals.
   --  All intervals on the list have positive size.
   type Interval_List_Type is tagged private;

   --  Include the given interval in the set covered by all intervals in List.
   --  This may include extensions of intervals, fusing multiple intervals into
   --  one or simply adding this interval to the list.
   procedure Add_Interval
     (List     : in out Interval_List_Type;
      Interval :        Interval_Type);

   --  Exclude all elements of the given interval from the set covered by all
   --  intervals in List. Elements of List are shortened or deleted.
   procedure Subtract_Interval
     (List     : in out Interval_List_Type;
      Interval :        Interval_Type);

   --  Wrapper for Subtract_Interval with parameters List and Interval.
   procedure Subtract_Interval
     (List          : in out Interval_List_Type;
      First_Element :        Interfaces.Unsigned_64;
      Size          :        Interfaces.Unsigned_64);

   --  Return an element A such that the interval [A, A+size-1]
   --  is included in some interval in List. The interval [A,A+size-1] is then
   --  subtracted from List, "reserving" that space.
   --  Raises an exception if the interval cannot be reserved.
   function Reserve_Interval
     (List : in out Interval_List_Type;
      Size :        Interfaces.Unsigned_64)
     return Interfaces.Unsigned_64 with
     Pre => Size > 0;

   --  Delete all entries of List.
   procedure Clear (List : in out Interval_List_Type);

   Out_Of_Space     : exception;
   Invalid_Interval : exception;

   --  Return a string representation of List with the numbers represented
   --  in Hex.
   function Interval_List_To_String_Hex
     (List : in out Interval_List_Type)
     return String;

private

   package Intervals_List_Package is new
     Ada.Containers.Doubly_Linked_Lists (Element_Type => Interval_Type);

   type Interval_List_Type is tagged
   record
      Data : Intervals_List_Package.List;
   end record;

end Mutools.Intervals;
