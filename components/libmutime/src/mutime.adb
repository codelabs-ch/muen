--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--    * Redistributions of source code must retain the above copyright notice,
--      this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--

package body Mutime
is

   --  Number of days between Common Era and UNIX epoch.
   CE_To_Epoch_Days : constant := 719499;

   Year_Epoch    : constant := 1970;
   Secs_Per_Hour : constant := 60 * 60;
   Secs_Per_Day  : constant := Secs_Per_Hour * 24;

   subtype Days_Before_Month_Type is Natural range
     0 .. Max_Days_Per_Year_Type - 31;

   type Days_Before_Month_Array is array (Month_Type'Range)
     of Days_Before_Month_Type;

   --  Number of days passed before a month.
   Month_Yday : constant array (Boolean) of Days_Before_Month_Array :=
     (False => (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
      True  => (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335));

   --  Returns the number of leap years in the range 0 .. Y.
   function Leaps (Y : Positive) return Natural
   is
     (Y / 4 - Y / 100 + Y / 400)
   with
      Post => Leaps'Result = (Y / 4 - Y / 100 + Y / 400);

   function Is_Leap (Y : Positive) return Boolean
   is
     (Y mod 4 = 0 and then (Y mod 100 /= 0 or Y mod 400 = 0))
   with
      Post => Is_Leap'Result =
         (Y mod 4 = 0 and (Y mod 100 /= 0 or Y mod 400 = 0));

   function Leaps_Between
     (Y1 : Positive;
      Y2 : Positive)
      return Integer
   with
      Post => Leaps_Between'Result = Leaps (Y2) - Leaps (Y1);

   --  Internal year type required to represent possible year numbers without
   --  taking leap years into account. Used when guessing the year from a day
   --  count.
   subtype Internal_Year_Type is Positive range 1970 .. 10005;
   subtype Total_Days_Per_Year_Type is
     Positive range 365 .. Max_Days_Per_Year_Type;

   function Get_Day_Count
     (Y : Internal_Year_Type)
      return Total_Days_Per_Year_Type
   is
     (if Is_Leap (Y => Y) then 366 else 365)
   with
      Post => Get_Day_Count'Result = (if Is_Leap (Y => Y) then 366 else 365);

   -------------------------------------------------------------------------

   function "+"
     (Left  : Timestamp_Type;
      Right : Integer_63)
      return Timestamp_Type
   is
      Res : Timestamp_Type                 := 0;
      L   : constant Integer_63            := Integer_63 (Left);
      Sum : constant Interfaces.Integer_64 := Interfaces.Integer_64
        (L + Right);
   begin
      if Sum <= Interfaces.Integer_64 (Timestamp_Type'Last) and then
        Sum > 0
      then
         Res := Timestamp_Type (Sum);
      elsif Sum > Interfaces.Integer_64 (Timestamp_Type'Last) then
         Res := Timestamp_Type'Last;
      elsif Sum < 0 then
         Res := 0;
      end if;

      return Res;
   end "+";

   -------------------------------------------------------------------------

   function "-"
     (Left  : Timestamp_Type;
      Right : Interfaces.Unsigned_64)
      return Timestamp_Type
   is
      L : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Left);
   begin
      if Right < L then
         return Timestamp_Type (L) - Timestamp_Type (Right);
      else
         return 0;
      end if;
   end "-";

   -------------------------------------------------------------------------

   procedure Get_Month_And_Day
     (Days      :     Day_Of_Year_Type;
      Leap_Year :     Boolean;
      Month     : out Month_Type;
      Day       : out Day_Type)
   is
   begin
      Month := 1;
      Day   := 1;

      for I in reverse Days_Before_Month_Array'Range loop
         if Days - Month_Yday (Leap_Year)(I) in
           Positive (Day_Type'First) .. Positive (Day_Type'Last)
         then
            Day   := Day_Type (Days - Month_Yday (Leap_Year)(I));
            Month := I;
            return;
         end if;
      end loop;
   end Get_Month_And_Day;

   -------------------------------------------------------------------------

   function Leaps_Between
     (Y1 : Positive;
      Y2 : Positive)
      return Integer
   is
      L1, L2 : Natural;
   begin
      L1 := Leaps (Y => Y1);
      L2 := Leaps (Y => Y2);

      return L2 - L1;
   end Leaps_Between;

   -------------------------------------------------------------------------

   --  Algorithm extracted from the Linux kernel time_to_tm() function
   --  (kernel/time/timeconv.c).

   procedure Split
     (Timestamp :     Timestamp_Type;
      Date_Time : out Date_Time_Type)
   is
      Days, R : Integer;
      Y       : Internal_Year_Type;
   begin

      --  Discard microseconds.

      Days := Integer ((Timestamp / 10 ** 6) / Secs_Per_Day);
      R    := Integer ((Timestamp / 10 ** 6) mod Secs_Per_Day);

      Date_Time.Hour := Hour_Type (R / Secs_Per_Hour);
      R := R mod Secs_Per_Hour;

      Date_Time.Minute := Minute_Type (R / 60);
      Date_Time.Second := Second_Type (R mod 60);

      Y := Year_Epoch;

      while Days < 0 or else Days >= Get_Day_Count (Y => Y) loop
         declare
            Yc : Integer := Days / 365;
            Yg : Internal_Year_Type;
         begin
            if Yc = 0 then
               Yc := -1;
            end if;

            Yg := Y + Yc;

            Days := Days - ((Yg - Y) * 365 + Leaps_Between
                            (Y1 => Y  - 1,
                             Y2 => Yg - 1));

            Y := Yg;
         end;
      end loop;

      Date_Time.Year := Year_Type (Y);
      Get_Month_And_Day (Days      => Days + 1,
                         Leap_Year => Is_Leap (Y => Y),
                         Month     => Date_Time.Month,
                         Day       => Date_Time.Day);
   end Split;

   -------------------------------------------------------------------------

   --  Algorithm extracted from the Linux kernel mktime64() function
   --  (kernel/time/time.c).

   function Time_Of (Date_Time : Date_Time_Type) return Timestamp_Type
   is
      M    : Integer := Integer (Date_Time.Month);
      Y    : Integer := Integer (Date_Time.Year);
      Time : Timestamp_Type;
   begin

      --  Put February last because it contains leap day.

      M := M - 2;
      if 0 >= M then
         M := M + 12;
         Y := Y - 1;
      end if;

      --  Days

      Time := Timestamp_Type
        (Leaps (Y) + 367 * M / 12 + Positive (Date_Time.Day))
          + Timestamp_Type (Y) * 365 - Timestamp_Type (CE_To_Epoch_Days);

      --  Hours

      Time := Time * 24 + Timestamp_Type (Date_Time.Hour);

      --  Minutes

      Time := Time * 60 + Timestamp_Type (Date_Time.Minute);

      --  Seconds

      Time := Time * 60 + Timestamp_Type (Date_Time.Second);

      return Time * 10 ** 6;
   end Time_Of;

end Mutime;
