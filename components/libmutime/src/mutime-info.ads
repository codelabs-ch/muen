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
with
   Abstract_State => (Valid, (State with External => Async_Writers))
is

   subtype Timezone_Type is Integer_62 range
     -12 * 60 * 60 * 10 ** 6 .. 14 * 60 * 60 * 10 ** 6;

   --  TSC tick rate in Hz from 1 Mhz to 100 Ghz.
   subtype TSC_Tick_Rate_Hz_Type is
     Interfaces.Unsigned_64 range 1000000 .. 100000000000;

   Time_Info_Size : constant := 24;

   type Time_Info_Type is record
      --  Mutime timestamp when TSC was zero. A TSC_Time_Base value of zero
      --  indicates that the time info is not (yet) valid. Use Update_Validity
      --  and Is_Valid operations to check.
      TSC_Time_Base      : Timestamp_Type'Base with Atomic;
      --  TSC Ticks in Hz
      TSC_Tick_Rate_Hz   : TSC_Tick_Rate_Hz_Type'Base;
      --  Timezone offset in microseconds
      Timezone_Microsecs : Timezone_Type'Base;
   end record
   with Size => Time_Info_Size * 8;

   --  Update time info validity flag.
   procedure Update_Validity
   with
      Global  => (Input  => State,
                  Output => Valid),
      Depends => (Valid => State);

   --  Return validity status of time info page.
   function Is_Valid return Boolean
   with
      Global => (Input => Valid);

   --  Calculate current timestamp using the information stored in the time
   --  info record and the specified CPU ticks. The procedure returns the
   --  timestamp and the calculated correction to the time base in
   --  microseconds.
   procedure Get_Current_Time
     (Schedule_Ticks :     Integer_62;
      Correction     : out Integer_63;
      Timestamp      : out Timestamp_Type)
   with
      Global  => (Proof_In => Valid,
                  Input    => State),
      Depends => ((Correction, Timestamp) => (Schedule_Ticks, State)),
      Pre     => Is_Valid;

   --  Return time at system boot.
   procedure Get_Boot_Time
     (Timestamp : out Timestamp_Type)
   with
      Global  => (Proof_In => Valid,
                  Input    => State),
      Depends => (Timestamp => State),
      Pre     => Is_Valid;

private

   for Time_Info_Type use record
      TSC_Time_Base      at  0 range 0 .. 63;
      TSC_Tick_Rate_Hz   at  8 range 0 .. 63;
      Timezone_Microsecs at 16 range 0 .. 63;
   end record;
   for Time_Info_Type'Object_Size use Time_Info_Size * 8;

   function Valid (TI : Time_Info_Type) return Boolean
   is (TI.TSC_Time_Base in Timestamp_Type
       and TI.TSC_Tick_Rate_Hz in TSC_Tick_Rate_Hz_Type
       and TI.Timezone_Microsecs in Timezone_Type);

   procedure Get_Current_Time
     (TI             :     Time_Info_Type;
      Schedule_Ticks :     Integer_62;
      Correction     : out Integer_63;
      Timestamp      : out Timestamp_Type)
   with
      Depends => ((Correction, Timestamp) => (Schedule_Ticks, TI));

   procedure Get_Boot_Time
     (TI        :     Time_Info_Type;
      Timestamp : out Timestamp_Type)
   with
      Depends => (Timestamp => TI);

   State_Valid : Boolean := False
   with
      Part_Of => Valid;

   function Is_Valid return Boolean is (State_Valid)
   with
      Refined_Global => (Input => State_Valid);

end Mutime.Info;
