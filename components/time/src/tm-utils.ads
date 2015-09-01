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

with Mutime;

with Tm.Rtc;

package Tm.Utils
is

   --  Convert values from CMOS/RTC to Mutime date/time. Outputs error message
   --  and returns False if a value does not fulfill the range constraints
   --  implied by Mutime.
   procedure To_Mutime
     (Rtc_Time  :     Rtc.Time_Type;
      Date_Time : out Mutime.Date_Time_Type;
      Success   : out Boolean);

end Tm.Utils;
