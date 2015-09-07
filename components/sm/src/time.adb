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

with Interfaces;

with Musinfo;
with Mutime.Info;

pragma Warnings (Off);
with SK;
pragma Warnings (On);

with Debug_Ops;

package body Time
with
   Refined_State => (State => (Time_Info, Sinfo))
is

   Sinfo_Base_Address : constant := 16#000e_0000_0000#;

   Sinfo : Musinfo.Subject_Info_Type
     with
       Volatile,
       Async_Writers,
       Address => System'To_Address (Sinfo_Base_Address);

   Time_Info : Mutime.Info.Time_Info_Type
     with
       Volatile,
       Async_Writers,
       Address => System'To_Address (Mutime.Info.Time_Info_Base_Address);

   -------------------------------------------------------------------------

   function Get_Date_Time return Mutime.Date_Time_Type
   is
      use type Interfaces.Integer_64;
      use type Interfaces.Unsigned_64;
      use type Mutime.Timestamp_Type;

      Date_Time  : Mutime.Date_Time_Type;
      Sched      : Mutime.Integer_62;
      Correction : Mutime.Integer_63;

      TSC_Schedule_Start : constant Interfaces.Unsigned_64
        := Sinfo.TSC_Schedule_Start;
      TI                 : constant Mutime.Info.Time_Info_Type := Time_Info;
   begin
      if TSC_Schedule_Start <= Interfaces.Unsigned_64
        (Mutime.Integer_62'Last)
      then
         Sched := Mutime.Integer_62 (TSC_Schedule_Start);
      else
         pragma Debug (Debug_Ops.Put_Value64
                       (Message => "Error: Scheduling info out of bounds",
                        Value   => SK.Word64 (TSC_Schedule_Start)));
         Sched := 0;
      end if;

      Mutime.Info.Get_Current_Date_Time
        (Time_Info      => TI,
         Schedule_Ticks => Sched,
         Correction     => Correction,
         Date_Time      => Date_Time);
      pragma Debug (Debug_Ops.Put_Value64
                    (Message => "Correction to boot timestamp (microsecs)",
                     Value   => SK.Word64 (Correction)));

      return Date_Time;
   end Get_Date_Time;

end Time;
