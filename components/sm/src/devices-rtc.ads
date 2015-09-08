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

with Types;
with Time;
with Subject_Info;

package Devices.RTC
with
   Abstract_State => State,
   Initializes    => State
is

   --  Emulate CMOS RTC I/O access.
   procedure Emulate
     (Info :     Types.IO_Info_Type;
      Halt : out Boolean)
   with
      Global => (Input  => Time.State,
                 In_Out => (State, Subject_Info.State)),
      Post   => Halt = False;

end Devices.RTC;
