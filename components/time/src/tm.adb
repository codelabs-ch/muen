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

with Tm.Rtc;

package body Tm
is

   -------------------------------------------------------------------------

   procedure Run
   is
      Rtc_Time : Rtc.Time_Type;
   begin
      Debuglog.Client.Put_Line (Item => "Time subject running");

      Rtc.Read_Time (T => Rtc_Time);

      Debuglog.Client.Put      (Item => "RTC date/time: ");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Year);
      Debuglog.Client.Put      (Item => "-");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Month);
      Debuglog.Client.Put      (Item => "-");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Day_Of_Month);
      Debuglog.Client.Put      (Item => "T");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Hours);
      Debuglog.Client.Put      (Item => ":");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Minutes);
      Debuglog.Client.Put      (Item => ":");
      Debuglog.Client.Put_Byte (Item => Rtc_Time.Seconds);
      Debuglog.Client.New_Line;

      Debuglog.Client.Put_Reg8 (Name  => "RTC status B",
                                Value => Rtc_Time.Status_B);

      loop
         null;
      end loop;
   end Run;

end Tm;
