--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.CPU;
with SK.Dump;
with SK.Strings;

package body SK.Measurements
with
   SPARK_Mode => Off
is

   -------------------------------------------------------------------------

   function Calc_Average (Values : Measurements_Array) return Word64
   is
      Result : Word64 := 0;
   begin
      for V of Values loop
         Result := Result + V;
      end loop;
      return Result / Word64 (Measurements_Range'Last);
   end Calc_Average;

   -------------------------------------------------------------------------

   procedure Start
   is
   begin
      Save_State (Current) := CPU.RDTSC;
   end Start;

   -------------------------------------------------------------------------

   procedure Stop
   is
      End_Time : constant SK.Word64 := CPU.RDTSC;
   begin
      Save_State (Current) := End_Time - Save_State (Current);

      if Current < Save_State'Last then
         Current := Current + 1;
      else
         Dump.Print_Message
           (Msg => "Average cycle count: "
            & Strings.Img (Item => Calc_Average (Values => Save_State)));
         Current := Save_State'First;
      end if;
   end Stop;

end SK.Measurements;
