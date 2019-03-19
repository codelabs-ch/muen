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

with Debug_Ops;

package body Time
is

   -------------------------------------------------------------------------

   function Get_Date_Time return Mutime.Date_Time_Type
   is
      use type Interfaces.Unsigned_64;

      Timestamp  : Mutime.Timestamp_Type;
      Date_Time  : Mutime.Date_Time_Type;
      Sched      : Mutime.Integer_62;
      Correction : Mutime.Integer_63;

      TSC_Schedule_Start : constant Interfaces.Unsigned_64
        := Musinfo.Instance.TSC_Schedule_Start;
   begin
      if TSC_Schedule_Start <= Interfaces.Unsigned_64
        (Mutime.Integer_62'Last)
      then
         Sched := Mutime.Integer_62 (TSC_Schedule_Start);
      else
         pragma Debug (Debug_Ops.Put_Line
                       (Item => "Error: Scheduling info out of bounds "
                        & SK.Strings.Img (TSC_Schedule_Start)));
         Sched := 0;
      end if;

      pragma Debug (Debug_Ops.Put_Line
                    (Item => "Schedule ticks is "
                     & SK.Strings.Img (SK.Word64 (Sched))));

      Mutime.Info.Get_Current_Time
        (Schedule_Ticks => Sched,
         Correction     => Correction,
         Timestamp      => Timestamp);
      pragma Debug (Debug_Ops.Put_Line
                    (Item => "Correction to boot timestamp (microsecs) "
                     & SK.Strings.Img (SK.Word64 (Correction))));

      pragma Debug (Debug_Ops.Put_Line
                    (Item => "SM timestamp "
                     & SK.Strings.Img (Mutime.Get_Value (Timestamp))));

      Mutime.Split (Timestamp => Timestamp,
                    Date_Time => Date_Time);
      return Date_Time;
   end Get_Date_Time;

   -------------------------------------------------------------------------

   procedure Initialize
   is
   begin
      if not Musinfo.Instance.Is_Valid then
         pragma Debug (Debug_Ops.Put_Line
                       (Item => "Error: Sinfo data not valid"));
         SK.CPU.Stop;
      end if;

      Mutime.Info.Update_Validity;
      if not Mutime.Info.Is_Valid then
         pragma Debug
           (Debug_Ops.Put_Line
              (Item => "Absolute time not yet available, waiting ..."));
         while not Mutime.Info.Is_Valid loop
            SK.CPU.Pause;
            Mutime.Info.Update_Validity;
         end loop;
      end if;
      pragma Debug
        (Debug_Ops.Put_Line
           (Item => "Absolute time available"));
   end Initialize;

end Time;
