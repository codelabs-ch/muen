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

with Interfaces;

with Debuglog.Client;

package body Tm.Utils
is
   use type Interfaces.Unsigned_8;

   function To_Binary
     (BCD : Interfaces.Unsigned_8)
      return Interfaces.Unsigned_8
   is
     (((BCD / 16) * 10) + (BCD and 16#0f#));

   -------------------------------------------------------------------------

   procedure To_Mutime
     (Rtc_Time  :     Rtc.Time_Type;
      Date_Time : out Mutime.Date_Time_Type;
      Success   : out Boolean)
   is
      Bin_Time : Rtc.Time_Type;
   begin
      Bin_Time.Year         := To_Binary (BCD => Rtc_Time.Year);
      Bin_Time.Month        := To_Binary (BCD => Rtc_Time.Month);
      Bin_Time.Day_Of_Month := To_Binary (BCD => Rtc_Time.Day_Of_Month);
      Bin_Time.Hours        := To_Binary (BCD => Rtc_Time.Hours);
      Bin_Time.Minutes      := To_Binary (BCD => Rtc_Time.Minutes);
      Bin_Time.Seconds      := To_Binary (BCD => Rtc_Time.Seconds);

      Date_Time := Mutime.Epoch;
      Success   := False;

      --  Assume 20th century.

      Date_Time.Year := Mutime.Year_Type (2000 + Natural (Bin_Time.Year));

      if Bin_Time.Month < Interfaces.Unsigned_8 (Mutime.Month_Type'First)
        or else Bin_Time.Month > Interfaces.Unsigned_8 (Mutime.Month_Type'Last)
      then
         Debuglog.Client.Put_Reg8 (Name  => "RTC month invalid",
                                   Value => Bin_Time.Month);
         return;
      else
         Date_Time.Month := Mutime.Month_Type (Bin_Time.Month);
      end if;

      if Bin_Time.Day_Of_Month < Interfaces.Unsigned_8 (Mutime.Day_Type'First)
        or else Bin_Time.Day_Of_Month >
          Interfaces.Unsigned_8 (Mutime.Day_Type'Last)
      then
         Debuglog.Client.Put_Reg8 (Name  => "RTC day invalid",
                                   Value => Bin_Time.Day_Of_Month);
         return;
      else
         Date_Time.Day := Mutime.Day_Type (Bin_Time.Day_Of_Month);
      end if;

      if Bin_Time.Hours > Interfaces.Unsigned_8 (Mutime.Hour_Type'Last)
      then
         Debuglog.Client.Put_Reg8 (Name  => "RTC hours invalid",
                                   Value => Bin_Time.Hours);
         return;
      else
         Date_Time.Hour := Mutime.Hour_Type (Bin_Time.Hours);
      end if;

      if Bin_Time.Minutes > Interfaces.Unsigned_8 (Mutime.Minute_Type'Last)
      then
         Debuglog.Client.Put_Reg8 (Name  => "RTC minutes invalid",
                                   Value => Bin_Time.Minutes);
         return;
      else
         Date_Time.Minute := Mutime.Minute_Type (Bin_Time.Minutes);
      end if;

      if Bin_Time.Seconds > Interfaces.Unsigned_8 (Mutime.Second_Type'Last)
      then
         Debuglog.Client.Put_Reg8 (Name  => "RTC seconds invalid",
                                   Value => Bin_Time.Seconds);
         return;
      else
         Date_Time.Second := Mutime.Second_Type (Bin_Time.Seconds);
      end if;

      Success := True;
   end To_Mutime;

end Tm.Utils;
