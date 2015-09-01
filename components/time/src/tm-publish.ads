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

with Mutime.Info;

package Tm.Publish
with
   Abstract_State =>
     (State with External => (Async_Readers, Effective_Writes)),
   Initializes    => State
is

   --  Update time information read by clients. The TSC tick rate is expected
   --  to be in Mhz and the timezone offset in microseconds.
   procedure Update
     (TSC_Time_Base : Mutime.Timestamp_Type;
      TSC_Tick_Rate : Mutime.Info.TSC_Tick_Rate_Mhz_Type;
      Timezone      : Mutime.Info.Timezone_Type)
   with
      Global  => (Output => State),
      Depends => (State  => (TSC_Time_Base, TSC_Tick_Rate, Timezone));

end Tm.Publish;
