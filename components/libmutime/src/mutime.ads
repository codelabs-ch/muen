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

package Mutime
is

   --  Time in microseconds since 1970-01-01T00:00:00+00:00.
   type Time_Type is private;

   type Year_Type is range 1970 .. 9999;
   type Month_Type is range 1 .. 12;
   type Day_Type is range 1 .. 31;
   type Hour_Type is range 0 .. 23;
   type Minute_Type is range 0 .. 59;
   type Second_Type is range 0 .. 59;

   --  Convert given date to Muen timestamp.
   function Time_Of
     (Year   : Year_Type;
      Month  : Month_Type;
      Day    : Day_Type;
      Hour   : Hour_Type;
      Minute : Minute_Type;
      Second : Second_Type)
      return Time_Type;

   --  Convert given timestamp to date in International Atomic Time (TAI).
   procedure Split
     (Time   :     Time_Type;
      Year   : out Year_Type;
      Month  : out Month_Type;
      Day    : out Day_Type;
      Hour   : out Hour_Type;
      Minute : out Minute_Type;
      Second : out Second_Type);

private

   --  Range of allowed timestamps spanning the time in microseconds from
   --  1970-01-01T00:00:00+00:00 to 9999-12-31T23:59:59+00:00.
   type Time_Type is range 0 .. 253402300799000000;

   Max_Days_Per_Year_Type : constant := 366;
   subtype Day_Of_Year_Type is Positive range 1 .. Max_Days_Per_Year_Type;

   --  Calculate month and day from given day of year value.
   procedure Get_Month_And_Day
     (Days      :     Day_Of_Year_Type;
      Leap_Year :     Boolean;
      Month     : out Month_Type;
      Day       : out Day_Type)
   with
      Pre => (if Days = 366 then Leap_Year);

end Mutime;
