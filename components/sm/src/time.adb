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

with Mutime.Info;

package body Time
with
   Refined_State => (State => Time_Info)
is

   Time_Info : Mutime.Info.Time_Info_Type
     with
       Address => System'To_Address (Mutime.Info.Time_Info_Base_Address);

   -------------------------------------------------------------------------

   function Get_Date_Time return Mutime.Date_Time_Type
   is
      Result : Mutime.Date_Time_Type;
   begin
      Mutime.Split (Timestamp => Time_Info.TSC_Time_Base,
                    Date_Time => Result);
      return Result;
   end Get_Date_Time;

begin

   --  FIXME: Initialization of state hidden.

   pragma SPARK_Mode (Off);

end Time;
