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

with Ada.Strings.Unbounded;

with Mutools.Utils;

package body Mutools.Intervals
is
   --  Check if I is empty or not.
   function Is_Empty (I : Interval_Type) return Boolean;

   -------------------------------------------------------------------------

   procedure Add_Interval
     (List     : in out Interval_List_Type;
      Interval :        Interval_Type)
   is
      use Intervals_List_Package;

      --  Insert Interval in List.
      procedure Insert_Interval
        (List     : in out Interval_List_Type;
         Interval :        Interval_Type);

      --  Delete all elements of List which are contained in Interval.
      procedure Delete_Contained_Intervals
        (List     : in out Interval_List_Type;
         Interval :        Interval_Type);

      --  Set Output_Ival to the union of Input_Ival and all intervals
      --  in List which have distance <=1 from the Input_Ival
      --  (i.e. they are contained or directly adjacent).
      procedure Extend_Interval
        (List        :     Interval_List_Type;
         Input_Ival  :     Interval_Type;
         Output_Ival : out Interval_Type);

      ----------------------------------------------------------------------

      procedure Delete_Contained_Intervals
        (List     : in out Interval_List_Type;
         Interval :        Interval_Type)
      is
         Curr          : Intervals_List_Package.Cursor
           := First (Container => List.Data);
         Next_Position : Intervals_List_Package.Cursor
           := Curr;
      begin
         while Curr /= No_Element loop
            if Element (Curr).Last_Element < Interval.First_Element then
               Next (Curr);
            elsif
              Element (Curr).First_Element >= Interval.First_Element
              and Element (Curr).Last_Element <= Interval.Last_Element
            then
               Next_Position := Next (Curr);
               Delete (Container => List.Data,
                       Position  => Curr);
               Curr := Next_Position;
            elsif Element (Curr).First_Element > Interval.Last_Element then
               return;
            end if;
         end loop;
      end Delete_Contained_Intervals;

      ----------------------------------------------------------------------

      procedure Extend_Interval
        (List        :     Interval_List_Type;
         Input_Ival  :     Interval_Type;
         Output_Ival : out Interval_Type)
      is
         Curr : Intervals_List_Package.Cursor;
      begin
         Output_Ival := Input_Ival;
         Curr := First (Container => List.Data);
         while Curr /= No_Element loop
            exit when
              Input_Ival.Last_Element < Interfaces.Unsigned_64'Last
                 and Element (Curr).First_Element > Input_Ival.Last_Element + 1;

            if Element (Curr).First_Element < Input_Ival.First_Element
              and then
              Element (Curr).Last_Element >= Input_Ival.First_Element - 1
            then
               Output_Ival.First_Element := Element (Curr).First_Element;
            end if;

            if Element (Curr).Last_Element > Input_Ival.Last_Element
              and then
              Element (Curr).First_Element <= Input_Ival.Last_Element + 1
            then
               Output_Ival.Last_Element := Element (Curr).Last_Element;
            end if;

            Next (Curr);
         end loop;
      end Extend_Interval;

      ----------------------------------------------------------------------

      procedure Insert_Interval
        (List     : in out Interval_List_Type;
         Interval :        Interval_Type)
      is
         Curr : Intervals_List_Package.Cursor
           := First (Container => List.Data);
      begin
         while Curr /= No_Element loop
            if Element (Curr).First_Element < Interval.First_Element then
               Next (Curr);
            elsif Element (Curr).First_Element > Interval.First_Element then
               Insert (Container => List.Data,
                       Before    => Curr,
                       New_Item  => Interval);
               return;
            end if;
         end loop;

         --  Either the list is empty or all entries start before Interval.
         if Curr = No_Element then
            Append (Container => List.Data,
                    New_Item  => Interval);
         end if;
      end Insert_Interval;

      Extended_Interval : Interval_Type;

   begin
      if Is_Empty (I => Interval) then
         return;
      end if;

      Extend_Interval
        (List        => List,
         Input_Ival  => Interval,
         Output_Ival => Extended_Interval);
      Delete_Contained_Intervals
        (List     => List,
         Interval => Extended_Interval);
      Insert_Interval
        (List     => List,
         Interval => Extended_Interval);
   end Add_Interval;

   -------------------------------------------------------------------------

   procedure Clear (List : in out Interval_List_Type)
   is
   begin
      List.Data.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Interval_List_To_String_Hex
     (List : in out Interval_List_Type)
     return String
   is
      use Ada.Strings.Unbounded;
      use type Intervals.Intervals_List_Package.Cursor;

      Output : Unbounded_String;
      Curr   : Intervals_List_Package.Cursor
        := Intervals_List_Package.First (Container => List.Data);
   begin
      while Curr /= Intervals_List_Package.No_Element loop
         if Output /= "" then
            Output := Output & " ";
         end if;
         Output := Output & "("
           & Mutools.Utils.To_Hex
           (Intervals_List_Package.Element (Curr).First_Element)
           & ", "
           & Mutools.Utils.To_Hex
           (Intervals_List_Package.Element (Curr).Last_Element)
           & ")";
         Intervals_List_Package.Next (Curr);
      end loop;
      return To_String (Output);
   end Interval_List_To_String_Hex;

   -------------------------------------------------------------------------

   function Is_Empty (I : Interval_Type) return Boolean
   is
   begin
      return (I.First_Element > I.Last_Element);
   end Is_Empty;

   -------------------------------------------------------------------------

   function Reserve_Interval
     (List : in out Interval_List_Type;
      Size :        Interfaces.Unsigned_64)
     return Interfaces.Unsigned_64
   is
      use Intervals_List_Package;

      Curr              : Intervals_List_Package.Cursor
        := First (Container => List.Data);
      Output            : Interfaces.Unsigned_64;
      Curr_Size_Minus_1 : Interfaces.Unsigned_64; -- Size-1 to avoid overflow
   begin
      while Curr /= No_Element loop
         Curr_Size_Minus_1 := Element (Curr).Last_Element
           - Element (Curr).First_Element;
         if Curr_Size_Minus_1 > Size - 1 then
            Output := Element (Curr).First_Element;
            Replace_Element
              (Container => List.Data,
               Position  => Curr,
               New_Item  =>
                 (First_Element => Element (Curr).First_Element + Size,
                  Last_Element  => Element (Curr).Last_Element));
            return Output;
         elsif Curr_Size_Minus_1 = Size - 1 then
            Output := Element (Curr).First_Element;
            Delete (Container => List.Data,
                    Position  => Curr);
            return Output;
         end if;
         Curr := Next (Curr);
      end loop;
      raise Out_Of_Space with
        "Cannot find free interval of size '"
        & Utils.To_Hex (Number => Size) & "'";
   end Reserve_Interval;

   -------------------------------------------------------------------------

   procedure Subtract_Interval
     (List          : in out Interval_List_Type;
      First_Element :        Interfaces.Unsigned_64;
      Size          :        Interfaces.Unsigned_64)
   is
   begin
      if Size = 0 then
         return;
      elsif First_Element > Interfaces.Unsigned_64'Last - Size + 1 then
         raise Invalid_Interval with
           "Unsigned_64 overflow when trying to subtract an interval";
      else
         Subtract_Interval
           (List     => List,
            Interval => (First_Element => First_Element,
                         Last_Element  => First_Element + Size - 1));
      end if;
   end Subtract_Interval;

   -------------------------------------------------------------------------

   procedure Subtract_Interval
     (List     : in out Interval_List_Type;
      Interval :        Interval_Type)
   is
      use Intervals_List_Package;

      Curr          : Intervals_List_Package.Cursor
        := First (Container => List.Data);
      Next_Position : Intervals_List_Package.Cursor
        := Curr;
   begin
      if Is_Empty (I => Interval) then
         return;
      end if;

      while Curr /= No_Element loop
         if Element (Curr).Last_Element < Interval.First_Element then
            Next (Curr);
         elsif Element (Curr).First_Element < Interval.First_Element
           and Element (Curr).Last_Element >= Interval.First_Element
         then
            Insert (Container => List.Data,
                    Before    => Next (Curr),
                    New_Item  => (First_Element => Interval.First_Element,
                                  Last_Element  => Element (Curr).Last_Element));
            Replace_Element
              (Container => List.Data,
               Position  => Curr,
               New_Item  => (First_Element => Element (Curr).First_Element,
                             Last_Element  => Interval.First_Element - 1));
            Curr := Next (Curr);
         elsif Element (Curr).Last_Element <= Interval.Last_Element then
            Next_Position := Next (Curr);
            Delete (Container => List.Data,
                    Position  => Curr);
            Curr := Next_Position;
         elsif Element (Curr).First_Element <= Interval.Last_Element then
            Replace_Element
              (Container => List.Data,
               Position  => Curr,
               New_Item  => (First_Element => Interval.Last_Element + 1,
                             Last_Element  => Element (Curr).Last_Element));
            Curr := Next_Position;
         else
            return;
         end if;
      end loop;
   end Subtract_Interval;

end Mutools.Intervals;
