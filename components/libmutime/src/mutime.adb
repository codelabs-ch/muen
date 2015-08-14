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

   function Leaps (Y : Positive) return Natural
   is
     (((Y - 1) / 4) - ((Y - 1) / 100) + ((Y - 1) / 400))
   with
      Post => Leaps'Result = ((Y - 1) / 4) - ((Y - 1) / 100) + ((Y - 1) / 400);

   -------------------------------------------------------------------------

   --  Algorithm extracted from the Linux kernel mktime64() function
   --  (kernel/time/time.c).

   function Time_Of
     (Year   : Year_Type;
      Month  : Month_Type;
      Day    : Day_Type;
      Hour   : Hour_Type;
      Minute : Minute_Type;
      Second : Second_Type)
      return Time_Type
   is
      M    : Integer := Integer (Month);
      Y    : Integer := Integer (Year);
      Time : Time_Type;
   begin

      --  Put February last because it contains leap day.

      M := M - 2;
      if 0 >= M then
         M := M + 12;
         Y := Y - 1;
      end if;

      --  Days

      Time := Time_Type (Leaps (Y) + 367 * M / 12 + Positive (Day))
        + Time_Type (Y) * 365 - CE_To_Epoch_Days;

      --  Hours

      Time := Time * 24 + Time_Type (Hour);

      --  Minutes

      Time := Time * 60 + Time_Type (Minute);

      --  Seconds

      Time := Time * 60 + Time_Type (Second);

      return Time * 10 ** 6;
   end Time_Of;

end Mutime;
