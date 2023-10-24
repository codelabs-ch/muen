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

   --  Shift Epoch to 0000-03-01
   Epoch_Shift_Days : constant := 719468;

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

   --  The following is adapted from Algorithm 5 of [1]. The Linux kernel
   --  uses the same algorithm (see kernel/time/timeconv.c).
   --
   --  [1] Neri C, Schneider L. Euclidean Affine Functions and Applications
   --  to Calendar Algorithms. Softw Pract Exper. 2023;53(4):937-970.
   --  doi: 10.1002/spe.3172

   procedure Split
     (Timestamp :     Timestamp_Type;
      Date_Time : out Date_Time_Type)
   is
      Days, R, Century, Day_Of_Century, Year_Of_Century, Day_Of_Year,
        Year, Month, Day, N1, N2, N3 : Interfaces.Unsigned_64;
   begin

      --  Discard microseconds.

      Days := Interfaces.Unsigned_64 ((Timestamp / 10 ** 6) / Secs_Per_Day);
      Days := Days + Epoch_Shift_Days;

      R    := Interfaces.Unsigned_64 ((Timestamp / 10 ** 6) mod Secs_Per_Day);

      Date_Time.Hour := Hour_Type (R / Secs_Per_Hour);
      R := R mod Secs_Per_Hour;

      Date_Time.Minute := Minute_Type (R / 60);
      Date_Time.Second := Second_Type (R mod 60);

      N1             := 4 * Days + 3;
      Century        := N1 / 146097;
      Day_Of_Century := (N1 mod 146097) / 4;

      N2              := 4 * Day_Of_Century + 3;
      N2              := N2 * 2939745;
      Year_Of_Century := N2 / 2 ** 32;
      Day_Of_Year     := (N2 mod 2 ** 32) / 2939745 / 4;

      N3    := Day_Of_Year * 2141 + 197913;
      Month := N3 / 2 ** 16;
      Day   := (N3 mod 2 ** 16) / 2141;

      Year := 100 * Century + Year_Of_Century;
      if Day_Of_Year >= 306 then
         Year  := Year + 1;
         Month := Month - 12;
      end if;
      Day := Day + 1;

      Date_Time.Year  := Year_Type (Year);
      Date_Time.Month := Month_Type (Month);
      Date_Time.Day   := Day_Type (Day);
   end Split;

   -------------------------------------------------------------------------

   --  The following is adapted from Algorithm 6 of [1].
   --
   --  [1] Neri C, Schneider L. Euclidean Affine Functions and Applications
   --  to Calendar Algorithms. Softw Pract Exper. 2023;53(4):937-970.
   --  doi: 10.1002/spe.3172

   function Time_Of (Date_Time : Date_Time_Type) return Timestamp_Type
   is
      Time : Timestamp_Type;

      C, Y, M, D, Y1, M1 : Interfaces.Unsigned_64;
   begin
      --  Required for gnatprove FSF 12.1.1.
      pragma Assert (1 <= Date_Time.Month and Date_Time.Month <= 12);
      pragma Assert
        (1_970 <= Date_Time.Year and Date_Time.Year <= 9_999);
      pragma Assert (1 <= Date_Time.Day and Date_Time.Day <= 31);
      pragma Assert (0 <= Date_Time.Hour and Date_Time.Hour <= 23);
      pragma Assert (0 <= Date_Time.Minute and Date_Time.Minute <= 59);
      pragma Assert (0 <= Date_Time.Second and Date_Time.Second <= 59);

      Y := Interfaces.Unsigned_64 (Date_Time.Year);
      M := Interfaces.Unsigned_64 (Date_Time.Month);

      if M <= 2 then
         M := M + 12;
         Y := Y - 1;
      end if;

      D := Interfaces.Unsigned_64 (Date_Time.Day) - 1;

      C  := Y / 100;
      Y1 := 1461 * Y / 4 - C + C / 4;
      M1 := (979 * M - 2919) / 2 ** 5;

      --  Days

      Time := Timestamp_Type ((Y1 + M1 + D) - Epoch_Shift_Days);

      --  Hours

      Time := Time * 24 + Timestamp_Type (Date_Time.Hour);

      --  Minutes

      Time := Time * 60 + Timestamp_Type (Date_Time.Minute);

      --  Seconds

      Time := Time * 60 + Timestamp_Type (Date_Time.Second);

      return Time * 10 ** 6;
   end Time_Of;

end Mutime;
