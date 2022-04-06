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

package Cmos_Rtc
is

   Port_Reg_Select : constant := 16#70#;
   Port_Data       : constant := 16#71#;

   Reg_Seconds      : constant := 16#00#;
   Reg_Minutes      : constant := 16#02#;
   Reg_Hours        : constant := 16#04#;
   Reg_Weekday      : constant := 16#06#;
   Reg_Day_Of_Month : constant := 16#07#;
   Reg_Month        : constant := 16#08#;
   Reg_Year         : constant := 16#09#;
   Reg_Century      : constant := 16#32#;
   Reg_Status_A     : constant := 16#0a#;
   Reg_Status_B     : constant := 16#0b#;
   Reg_Status_D     : constant := 16#0d#;

end Cmos_Rtc;
