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

with SK.IO;

package body Tm.Rtc
is

   Port_Reg_Select : constant := 16#70#;
   Port_Data       : constant := 16#71#;

   Bit_UIP : constant := 7;

   RTC_Reg_Seconds      : constant := 16#00#;
   RTC_Reg_Minutes      : constant := 16#02#;
   RTC_Reg_Hours        : constant := 16#04#;
   RTC_Reg_Weekday      : constant := 16#06#;
   RTC_Reg_Day_Of_Month : constant := 16#07#;
   RTC_Reg_Month        : constant := 16#08#;
   RTC_Reg_Year         : constant := 16#09#;
   RTC_Reg_Status_A     : constant := 16#0a#;
   RTC_Reg_Status_B     : constant := 16#0b#;

   -------------------------------------------------------------------------

   procedure Read_Time (T : out Time_Type)
   is
      Status_A : SK.Byte := 16#80#;
   begin

      --  Wait for Update to complete. This code is inspired by the Linux RTC
      --  driver found in arch/x86/kernel/rtc.c.

      while SK.Bit_Test
        (Value => SK.Word64 (Status_A),
         Pos   => Bit_UIP)
      loop
         SK.IO.Outb (Port  => Port_Reg_Select,
                     Value => RTC_Reg_Status_A);
         SK.IO.Inb  (Port  => Port_Data,
                     Value => Status_A);
      end loop;

      --  Read values from RTC.

      SK.IO.Outb (Port  => Port_Reg_Select,
                  Value => RTC_Reg_Seconds);
      SK.IO.Inb  (Port  => Port_Data,
                  Value => SK.Byte (T.Second));
      SK.IO.Outb (Port  => Port_Reg_Select,
                  Value => RTC_Reg_Minutes);
      SK.IO.Inb  (Port  => Port_Data,
                  Value => SK.Byte (T.Minute));
      SK.IO.Outb (Port  => Port_Reg_Select,
                  Value => RTC_Reg_Hours);
      SK.IO.Inb  (Port  => Port_Data,
                  Value => SK.Byte (T.Hour));
      SK.IO.Outb (Port  => Port_Reg_Select,
                  Value => RTC_Reg_Weekday);
      SK.IO.Inb  (Port  => Port_Data,
                  Value => SK.Byte (T.Weekday));
      SK.IO.Outb (Port  => Port_Reg_Select,
                  Value => RTC_Reg_Day_Of_Month);
      SK.IO.Inb  (Port  => Port_Data,
                  Value => SK.Byte (T.Day));
      SK.IO.Outb (Port  => Port_Reg_Select,
                  Value => RTC_Reg_Month);
      SK.IO.Inb  (Port  => Port_Data,
                  Value => SK.Byte (T.Month));
      SK.IO.Outb (Port  => Port_Reg_Select,
                  Value => RTC_Reg_Year);
      SK.IO.Inb  (Port  => Port_Data,
                  Value => SK.Byte (T.Year));

      SK.IO.Outb (Port  => Port_Reg_Select,
                  Value => RTC_Reg_Status_B);
      SK.IO.Inb  (Port  => Port_Data,
                  Value => SK.Byte (T.Status_B));
   end Read_Time;

end Tm.Rtc;
