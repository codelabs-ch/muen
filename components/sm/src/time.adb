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

      Timestamp         : Mutime.Timestamp_Type;
      Date_Time         : Mutime.Date_Time_Type;
      Sched             : Mutime.Integer_62;
      Correction_Unused : Mutime.Integer_63;
      Success           : Boolean;

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

      Mutime.Info.Get_Current_Time
        (Schedule_Ticks => Sched,
         Correction     => Correction_Unused,
         Timestamp      => Timestamp,
         Success        => Success);
      if Success then
         pragma Debug (Debug_Ops.Put_Line
                      (Item => "Correction to boot timestamp (microsecs) "
                        & SK.Strings.Img (SK.Word64 (Correction_Unused))));
         Mutime.Split (Timestamp => Timestamp,
                       Date_Time => Date_Time);
      else
         Date_Time := Mutime.Epoch;
         pragma Debug
           (Debug_Ops.Put_Line
              (Item => "Error: Unable to get current time, using epoch"));
      end if;

      return Date_Time;
   end Get_Date_Time;

   -------------------------------------------------------------------------

   procedure Initialize
   is
      Time_Valid : Boolean;
   begin
      if not Musinfo.Instance.Is_Valid then
         pragma Debug (Debug_Ops.Put_Line
                       (Item => "Error: Sinfo data not valid"));
         SK.CPU.Stop;
      end if;

      Time_Valid := Mutime.Info.Is_Valid;
      if not Time_Valid then
         pragma Debug
           (Debug_Ops.Put_Line
              (Item => "Absolute time not yet available, waiting ..."));
         while not Time_Valid loop
            Time_Valid := Mutime.Info.Is_Valid;
            SK.CPU.Pause;
         end loop;
      end if;
      pragma Debug (Debug_Ops.Put_Line (Item => "Absolute time available"));
   end Initialize;

end Time;
