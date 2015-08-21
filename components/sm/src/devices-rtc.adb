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

with Mutime.Utils;

with SK;

package body Devices.RTC
with
   Refined_State => (State => (Current_Register, Current_Time))
is

   use Types;

   Port_Reg_Select : constant := 16#70#;
   Port_Data       : constant := 16#71#;

   --  Time is in BCD, 24-hour format.
   Status_B : constant := 2#10#;

   RTC_Reg_Seconds      : constant := 16#00#;
   RTC_Reg_Minutes      : constant := 16#02#;
   RTC_Reg_Hours        : constant := 16#04#;
   RTC_Reg_Weekday      : constant := 16#06#;
   RTC_Reg_Day_Of_Month : constant := 16#07#;
   RTC_Reg_Month        : constant := 16#08#;
   RTC_Reg_Year         : constant := 16#09#;
   RTC_Reg_Status_A     : constant := 16#0a#;
   RTC_Reg_Status_B     : constant := 16#0b#;

   Current_Register : SK.Byte               := 0;
   Current_Time     : Mutime.Date_Time_Type := Mutime.Epoch;

   -------------------------------------------------------------------------

   procedure Emulate
     (Info :     Types.IO_Info_Type;
      Halt : out Boolean)
   with
      Refined_Global => (Input  => Time.State,
                         In_Out => (Current_Time, Current_Register,
                                    Subject_Info.State))
   is
      use type SK.Word16;
      use type SK.Word64;
      use type Mutime.Year_Type;

      RAX : constant SK.Word64 := Subject_Info.State.Regs.RAX;
   begin
      Halt := False;

      --  Ignore invalid requests.

      if (Info.Port_Number = Port_Reg_Select and then Info.Direction = Dir_In)
        or (Info.Port_Number = Port_Data and then Info.Direction = Dir_Out)
      then
         return;
      end if;

      case Info.Port_Number is
         when Port_Reg_Select =>
            case RAX and 16#ff# is
               when RTC_Reg_Status_A => Current_Register := RTC_Reg_Status_A;
               when RTC_Reg_Seconds  => Current_Register := RTC_Reg_Seconds;
               when RTC_Reg_Minutes  => Current_Register := RTC_Reg_Minutes;
               when RTC_Reg_Hours    => Current_Register := RTC_Reg_Hours;
               when RTC_Reg_Weekday  => Current_Register := RTC_Reg_Weekday;
               when RTC_Reg_Day_Of_Month =>
                  Current_Register := RTC_Reg_Day_Of_Month;
               when RTC_Reg_Month    => Current_Register := RTC_Reg_Month;
               when RTC_Reg_Year     => Current_Register := RTC_Reg_Year;
               when RTC_Reg_Status_B => Current_Register := RTC_Reg_Status_B;
               when others           => null;
            end case;
         when Port_Data =>
            case Current_Register is
               when RTC_Reg_Status_A =>

                  --  Client initiates RTC read, get date and time.

                  Current_Time := Time.Get_Date_Time;

                  Subject_Info.State.Regs.RAX := 0;
               when RTC_Reg_Seconds =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Interfaces.Unsigned_8
                            (Current_Time.Second)));
               when RTC_Reg_Minutes =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Interfaces.Unsigned_8
                            (Current_Time.Minute)));
               when RTC_Reg_Hours =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Interfaces.Unsigned_8
                            (Current_Time.Hour)));
               when RTC_Reg_Weekday =>
                  Subject_Info.State.Regs.RAX := 0;
               when RTC_Reg_Day_Of_Month =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Interfaces.Unsigned_8
                            (Current_Time.Day)));
               when RTC_Reg_Month =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Interfaces.Unsigned_8
                            (Current_Time.Month)));
               when RTC_Reg_Year =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Mutime.Utils.Two_Digits_Type
                            (Current_Time.Year mod 100)));
               when RTC_Reg_Status_B =>
                  Subject_Info.State.Regs.RAX := Status_B;
               when others => null;
            end case;
         when others => null;
      end case;
   end Emulate;

end Devices.RTC;
