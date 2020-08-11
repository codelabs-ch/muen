--
--  Copyright (C) 2015, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Cmos_Rtc;

with Time;

package body Devices.RTC
with
   Refined_State => (State => (Current_Register, Current_Time, Status_A))
is

   use Types;

   --  Time is in BCD, 24-hour format.
   Status_B : constant := 2#10#;

   --  Status A UIP bit.
   UIP_Bit : constant := 16#80#;

   Status_A         : SK.Word64             := 0;
   Current_Register : SK.Byte               := 0;
   Current_Time     : Mutime.Date_Time_Type := Mutime.Epoch;

   -------------------------------------------------------------------------

   procedure Emulate
     (Info   :     Types.IO_Info_Type;
      Action : out Types.Subject_Action_Type)
   with
      Refined_Global =>
         (Proof_In => Musinfo.Instance.State,
          Input    => (Mutime.Info.State,
                       Musinfo.Instance.Scheduling_Info),
          In_Out   => (Current_Register, Current_Time, Status_A,
                       Subject_Info.State))
   is
      use type SK.Word16;
      use type SK.Word64;
      use type Mutime.Year_Type;

      package CR renames Cmos_Rtc;

      Cur_RAX : constant SK.Word64 := Subject_Info.State.Regs.RAX;
   begin
      Action := Types.Subject_Continue;

      --  Ignore invalid requests.

      if (Info.Port_Number = CR.Port_Reg_Select
          and then Info.Direction = Dir_In)
        or (Info.Port_Number = CR.Port_Data
            and then Info.Direction = Dir_Out)
      then
         return;
      end if;

      case Info.Port_Number is
         when CR.Port_Reg_Select =>
            case Cur_RAX and 16#ff# is
               when CR.Reg_Status_A =>
                  Current_Register := CR.Reg_Status_A;
               when CR.Reg_Seconds =>
                  Current_Register := CR.Reg_Seconds;
               when CR.Reg_Minutes =>
                  Current_Register := CR.Reg_Minutes;
               when CR.Reg_Hours =>
                  Current_Register := CR.Reg_Hours;
               when CR.Reg_Weekday =>
                  Current_Register := CR.Reg_Weekday;
               when CR.Reg_Day_Of_Month =>
                  Current_Register := CR.Reg_Day_Of_Month;
               when CR.Reg_Month =>
                  Current_Register := CR.Reg_Month;
               when CR.Reg_Year =>
                  Current_Register := CR.Reg_Year;
               when CR.Reg_Status_B =>
                  Current_Register := CR.Reg_Status_B;
               when others => null;
            end case;
         when CR.Port_Data =>
            case Current_Register is
               when CR.Reg_Status_A =>

                  --  Client initiates RTC read, get date and time.
                  --  Toggle UIP bit in status A to emulate update in progress
                  --  behavior.

                  Current_Time := Time.Get_Date_Time;

                  Status_A := Status_A xor UIP_Bit;
                  Subject_Info.State.Regs.RAX := Status_A;
               when CR.Reg_Seconds =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Interfaces.Unsigned_8
                            (Current_Time.Second)));
               when CR.Reg_Minutes =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Interfaces.Unsigned_8
                            (Current_Time.Minute)));
               when CR.Reg_Hours =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Interfaces.Unsigned_8
                            (Current_Time.Hour)));
               when CR.Reg_Weekday =>
                  Subject_Info.State.Regs.RAX := 0;
               when CR.Reg_Day_Of_Month =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Interfaces.Unsigned_8
                            (Current_Time.Day)));
               when CR.Reg_Month =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Interfaces.Unsigned_8
                            (Current_Time.Month)));
               when CR.Reg_Year =>
                  Subject_Info.State.Regs.RAX := SK.Word64
                    (Mutime.Utils.To_BCD
                       (Value => Mutime.Utils.Two_Digits_Type
                            (Current_Time.Year mod 100)));
               when CR.Reg_Status_B =>
                  Subject_Info.State.Regs.RAX := Status_B;
               when others => null;
            end case;
         when others => null;
      end case;
   end Emulate;

end Devices.RTC;
