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

with System;

with Time_Component.Channels;

package body Tm.Publish
with
   Refined_State => (State => Time_Info)
is

   Time_Info : Mutime.Info.Time_Info_Type
     with
       Volatile,
       Async_Readers,
       Effective_Writes,
       Address => System'To_Address
         (Time_Component.Channels.Time_Info_Address);

   -------------------------------------------------------------------------

   procedure Update
     (TSC_Time_Base : Mutime.Timestamp_Type;
      TSC_Tick_Rate : Mutime.Info.TSC_Tick_Rate_Hz_Type;
      Timezone      : Mutime.Info.Timezone_Type)
   is
   begin
      Time_Info.TSC_Time_Base      := TSC_Time_Base;
      Time_Info.TSC_Tick_Rate_Hz   := TSC_Tick_Rate;
      Time_Info.Timezone_Microsecs := Timezone;
   end Update;

begin
   Time_Info := (TSC_Time_Base      => Mutime.Epoch_Timestamp,
                 TSC_Tick_Rate_Hz   => 1000000,
                 Timezone_Microsecs => 0);
end Tm.Publish;
