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

with SK.CPU;

with Mutime;
with Musinfo;

with Tm.Rtc;
with Tm.Utils;

package body Tm
with
   Refined_State => (State => Sinfo)
is

   Sinfo : Musinfo.Subject_Info_Type
     with
       Volatile,
       Async_Writers,
       Effective_Reads,
       Address => System'To_Address (Subject_Info_Virtual_Addr);

   Subject_Info_Virtual_Addr : constant := 16#000e_0000_0000#;

   -------------------------------------------------------------------------

   procedure Run
   is
      use type Interfaces.Unsigned_8;

      Rtc_Time  : Rtc.Time_Type;
      Date_Time : Mutime.Date_Time_Type;
      Success   : Boolean;
   begin
      Debuglog.Client.Put_Line (Item => "Time subject running");

      Rtc.Read_Time (T => Rtc_Time);

      Debuglog.Client.Put      (Item => "RTC date/time: ");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Century);
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Year);
      Debuglog.Client.Put      (Item => "-");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Month);
      Debuglog.Client.Put      (Item => "-");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Day);
      Debuglog.Client.Put      (Item => "T");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Hour);
      Debuglog.Client.Put      (Item => ":");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Minute);
      Debuglog.Client.Put      (Item => ":");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Second);
      Debuglog.Client.New_Line;

      Debuglog.Client.Put_Reg8 (Name  => "RTC status B",
                                Value => Rtc_Time.Status_B);

      Utils.To_Mutime (Rtc_Time  => Rtc_Time,
                       Date_Time => Date_Time,
                       Success   => Success);
      if not Success then
         Debuglog.Client.Put_Line
           (Item => "Error: Unable to convert RTC date/time");
      else
         declare
            use type Interfaces.Unsigned_64;
            use type Mutime.Timestamp_Type;

            Timestamp : Mutime.Timestamp_Type;
            TSC_Khz   : constant Interfaces.Unsigned_64 := Sinfo.TSC_Khz;

            TSC_Mhz, TSC_Value, Microsecs_Boot : Interfaces.Unsigned_64;
         begin
            Timestamp := Mutime.Time_Of (Date_Time => Date_Time);
            TSC_Value := Interfaces.Unsigned_64 (SK.CPU.RDTSC64);

            TSC_Mhz        := TSC_Khz / 1000;
            Microsecs_Boot := TSC_Value / TSC_Mhz;

            Timestamp := Timestamp - Microsecs_Boot;

            Debuglog.Client.Put_Reg64
              (Name  => "Microseconds since boot",
               Value => Microsecs_Boot);
            Debuglog.Client.Put_Reg64
              (Name  => "Mutime timestamp",
               Value => Mutime.Get_Value (Timestamp => Timestamp));
         end;
      end if;

      loop
         SK.CPU.Hlt;
      end loop;
   end Run;

end Tm;
