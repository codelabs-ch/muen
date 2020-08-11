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

with System;

with Libmutime_Component.Channels;

package body Mutime.Info
with
   Refined_State => (State => Time_Info)
is

   package Cspecs renames Libmutime_Component.Channels;

   Time_Info : Time_Info_Type
   with
      Volatile,
      Async_Writers,
      Address => System'To_Address (Cspecs.Time_Info_Address);

   -------------------------------------------------------------------------

   procedure Get_Boot_Time
     (Timestamp : out Timestamp_Type)
   with
      Refined_Global  => (Input => Time_Info),
      Refined_Depends => (Timestamp => Time_Info)
   is
      Time : constant Time_Info_Type := Time_Info;
   begin
      if Valid (TI => Time) then
         Get_Boot_Time (TI        => Time,
                        Timestamp => Timestamp);
      else
         Timestamp := Timestamp_Type'First;
      end if;
   end Get_Boot_Time;

   -------------------------------------------------------------------------

   procedure Get_Boot_Time
     (TI        :     Time_Info_Type;
      Timestamp : out Timestamp_Type)
   is
   begin
      Timestamp := TI.TSC_Time_Base;
   end Get_Boot_Time;

   -------------------------------------------------------------------------

   procedure Get_Current_Time
     (TI             :     Time_Info_Type;
      Schedule_Ticks :     Integer_62;
      Correction     : out Integer_63;
      Timestamp      : out Timestamp_Type)
   is
      --  TSC tick rate in MHz from 1 Mhz to 100 Ghz.
      subtype TSC_Tick_Rate_Mhz_Type is Integer_62 range 1 .. 100000;

      Timezone_Microsecs : constant Timezone_Type
        := TI.Timezone_Microsecs;
      TSC_Tick_Rate_Hz   : constant TSC_Tick_Rate_Hz_Type
        := TI.TSC_Tick_Rate_Hz;
      TSC_Tick_Rate_Mhz  : constant TSC_Tick_Rate_Mhz_Type
        := TSC_Tick_Rate_Mhz_Type (TSC_Tick_Rate_Hz / 10 ** 6);
   begin
      Timestamp := TI.TSC_Time_Base;

      Correction := Timezone_Microsecs + Integer_62
        (Schedule_Ticks / TSC_Tick_Rate_Mhz);

      Timestamp := Timestamp + Correction;
   end Get_Current_Time;

   -------------------------------------------------------------------------

   procedure Get_Current_Time
     (Schedule_Ticks :     Integer_62;
      Correction     : out Integer_63;
      Timestamp      : out Timestamp_Type;
      Success        : out Boolean)
   with
      Refined_Global  => (Input => Time_Info),
      Refined_Depends => ((Correction, Timestamp) => (Schedule_Ticks,
                                                      Time_Info),
                          Success                 => Time_Info)
   is
      Time : constant Time_Info_Type := Time_Info;
   begin
      Success := Valid (TI => Time);
      if Success then
         Get_Current_Time (TI             => Time,
                           Schedule_Ticks => Schedule_Ticks,
                           Correction     => Correction,
                           Timestamp      => Timestamp);
      else
         Correction := Integer_63'First;
         Timestamp := Timestamp_Type'First;
      end if;
   end Get_Current_Time;

   -------------------------------------------------------------------------

   function Is_Valid return Boolean
   with
      Refined_Global => (Input => Time_Info)
   is
      Time : constant Time_Info_Type := Time_Info;
   begin
      return Valid (TI => Time);
   end Is_Valid;

end Mutime.Info;
