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

package Mutime.Info
is

   subtype Timezone_Type is Integer_62 range
     -12 * 60 * 60 * 10 ** 6 .. 14 * 60 * 60 * 10 ** 6;

   subtype TSC_Tick_Rate_Mhz_Type is Interfaces.Unsigned_64 range 1 .. 100000;

   type Time_Info_Type is record
      --  Time when TSC was zero
      TSC_Time_Base      : Timestamp_Type;
      --  TSC Ticks in Mhz
      TSC_Tick_Rate_Mhz  : TSC_Tick_Rate_Mhz_Type;
      --  Timezone offset in microseconds
      Timezone_Microsecs : Timezone_Type;
   end record
     with
       Size => 3 * 8 * 8;

private

   for Time_Info_Type use record
      TSC_Time_Base      at  0 range 0 .. 63;
      TSC_Tick_Rate_Mhz  at  8 range 0 .. 63;
      Timezone_Microsecs at 16 range 0 .. 63;
   end record;

end Mutime.Info;
