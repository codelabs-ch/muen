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
with SK.Bitops;

with Cmos_Rtc;

package body Tm.Rtc
is

   Bit_UIP : constant := 7;

   -------------------------------------------------------------------------

   procedure Read_Time (T : out Time_Type)
   is
      Status_A : SK.Byte := 16#80#;
   begin

      --  Wait for Update to complete. This code is inspired by the Linux RTC
      --  driver found in arch/x86/kernel/rtc.c.

      while SK.Bitops.Bit_Test
        (Value => SK.Word64 (Status_A),
         Pos   => Bit_UIP)
      loop
         SK.IO.Outb (Port  => Cmos_Rtc.Port_Reg_Select,
                     Value => Cmos_Rtc.Reg_Status_A);
         SK.IO.Inb  (Port  => Cmos_Rtc.Port_Data,
                     Value => Status_A);
      end loop;

      --  Read values from RTC.

      SK.IO.Outb (Port  => Cmos_Rtc.Port_Reg_Select,
                  Value => Cmos_Rtc.Reg_Seconds);
      SK.IO.Inb  (Port  => Cmos_Rtc.Port_Data,
                  Value => SK.Byte (T.Second));
      SK.IO.Outb (Port  => Cmos_Rtc.Port_Reg_Select,
                  Value => Cmos_Rtc.Reg_Minutes);
      SK.IO.Inb  (Port  => Cmos_Rtc.Port_Data,
                  Value => SK.Byte (T.Minute));
      SK.IO.Outb (Port  => Cmos_Rtc.Port_Reg_Select,
                  Value => Cmos_Rtc.Reg_Hours);
      SK.IO.Inb  (Port  => Cmos_Rtc.Port_Data,
                  Value => SK.Byte (T.Hour));
      SK.IO.Outb (Port  => Cmos_Rtc.Port_Reg_Select,
                  Value => Cmos_Rtc.Reg_Day_Of_Month);
      SK.IO.Inb  (Port  => Cmos_Rtc.Port_Data,
                  Value => SK.Byte (T.Day));
      SK.IO.Outb (Port  => Cmos_Rtc.Port_Reg_Select,
                  Value => Cmos_Rtc.Reg_Month);
      SK.IO.Inb  (Port  => Cmos_Rtc.Port_Data,
                  Value => SK.Byte (T.Month));
      SK.IO.Outb (Port  => Cmos_Rtc.Port_Reg_Select,
                  Value => Cmos_Rtc.Reg_Year);
      SK.IO.Inb  (Port  => Cmos_Rtc.Port_Data,
                  Value => SK.Byte (T.Year));
      SK.IO.Outb (Port  => Cmos_Rtc.Port_Reg_Select,
                  Value => Cmos_Rtc.Reg_Century);
      SK.IO.Inb  (Port  => Cmos_Rtc.Port_Data,
                  Value => SK.Byte (T.Century));

      SK.IO.Outb (Port  => Cmos_Rtc.Port_Reg_Select,
                  Value => Cmos_Rtc.Reg_Status_B);
      SK.IO.Inb  (Port  => Cmos_Rtc.Port_Data,
                  Value => SK.Byte (T.Status_B));
   end Read_Time;

end Tm.Rtc;
