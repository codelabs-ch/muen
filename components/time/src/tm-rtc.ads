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

with X86_64;

with Interfaces;

package Tm.Rtc
is

   --  Type used to store RTC values.
   type Time_Type is record
      Year     : Interfaces.Unsigned_8;
      Month    : Interfaces.Unsigned_8;
      Day      : Interfaces.Unsigned_8;
      Hour     : Interfaces.Unsigned_8;
      Minute   : Interfaces.Unsigned_8;
      Second   : Interfaces.Unsigned_8;
      Weekday  : Interfaces.Unsigned_8;
      Status_B : Interfaces.Unsigned_8;
   end record;

   --  Read time from CMOS RTC.
   procedure Read_Time (T : out Time_Type)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (T => X86_64.State, X86_64.State =>+ null);

end Tm.Rtc;
