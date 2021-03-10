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

with SK.CPU;
with SK.Strings;

pragma $Release_Warnings (Off, "unit * is not referenced");
with Debuglog.Client;
pragma $Release_Warnings (On, "unit * is not referenced");

with Mutime.Info;

with Tm.Rtc;
with Tm.Utils;

package body Tm.Main
is

   -------------------------------------------------------------------------

   procedure Run
   is
      Rtc_Time  : Rtc.Time_Type;
      Date_Time : Mutime.Date_Time_Type;
      TSC_Value : Interfaces.Unsigned_64;
      Success   : Boolean;
   begin
      pragma Debug (Debuglog.Client.Init (Epoch => 1));
      pragma Debug (Debuglog.Client.Put_Line (Item => "Time subject running"));

      if not Musinfo.Instance.Is_Valid then
         pragma Debug (Debuglog.Client.Put_Line
                       (Item => "Error: Sinfo data not valid"));
         SK.CPU.Stop;
      end if;

      Mucontrol.Status.Instance.Initialize;

      Rtc.Read_Time (T => Rtc_Time);
      TSC_Value := SK.CPU.RDTSC;

      pragma Debug
        (Debuglog.Client.Put_Line
           (Item => "RTC date/time: "
            & SK.Strings.Img_Nobase (Rtc_Time.Century)
            & SK.Strings.Img_Nobase (Item => Rtc_Time.Year)
            & "-" & SK.Strings.Img_Nobase (Rtc_Time.Month)
            & "-" & SK.Strings.Img_Nobase (Rtc_Time.Day)
            & "T" & SK.Strings.Img_Nobase (Rtc_Time.Hour)
            & ":" & SK.Strings.Img_Nobase (Rtc_Time.Minute)
            & ":" & SK.Strings.Img_Nobase (Rtc_Time.Second)));

      pragma Debug (Debuglog.Client.Put_Line
                    (Item => "RTC status B: " & SK.Strings.Img
                     (Rtc_Time.Status_B)));

      Utils.To_Mutime (Rtc_Time  => Rtc_Time,
                       Date_Time => Date_Time,
                       Success   => Success);

      if not Success then
         pragma Debug (Debuglog.Client.Put_Line
                       (Item => "Error: Unable to convert RTC date/time, "
                        & "setting it to 01-01-2000"));

        --  Clients might watch null value to detect published time, therefore
        --  set it to something non-zero.

         Date_Time      := Mutime.Epoch;
         Date_Time.Year := 2000;
      end if;

      declare
         use type Interfaces.Unsigned_64;
         use type Mutime.Timestamp_Type;

         Timestamp      : Mutime.Timestamp_Type;
         TSC_Khz        : constant Musinfo.TSC_Tick_Rate_Khz_Type
           := Musinfo.Instance.TSC_Khz;
         TSC_Hz         : constant Mutime.Info.TSC_Tick_Rate_Hz_Type
           := TSC_Khz * 1000;
         TSC_Mhz        : constant Interfaces.Unsigned_64
           := TSC_Khz  / 1000;
         Microsecs_Boot : Interfaces.Unsigned_64;
      begin
         Timestamp := Mutime.Time_Of (Date_Time => Date_Time);
         Microsecs_Boot := TSC_Value / TSC_Mhz;

         Timestamp := Timestamp - Microsecs_Boot;

         pragma Debug
           (Debuglog.Client.Put_Line
              (Item => "Microseconds since boot " & SK.Strings.Img
                   (Microsecs_Boot)));
         pragma Debug
           (Debuglog.Client.Put_Line
              (Item => "Mutime timestamp " & SK.Strings.Img
                   (Interfaces.Unsigned_64 (Timestamp))));
         pragma Debug
           (Debuglog.Client.Put_Line
              (Item => "Exporting time information to clients"));

         Publish.Update (TSC_Time_Base => Timestamp,
                         TSC_Tick_Rate => TSC_Hz,
                         Timezone      => 0);
      end;

      pragma Debug
        (Debuglog.Client.Put_Line
           (Item => "Time successfully published, halting"));

      Mucontrol.Status.Instance.Set
        (New_State => Mucontrol.Status.STATE_FINISHED);
      SK.CPU.Stop;
   end Run;

end Tm.Main;
