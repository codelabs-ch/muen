--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutime.Info.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mutime.Info.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Current_Time (Gnattest_T : in out Test);
   procedure Test_Get_Current_Time_cea631 (Gnattest_T : in out Test) renames Test_Get_Current_Time;
--  id:2.2/cea6319c99ce0b67/Get_Current_Time/1/0/
   procedure Test_Get_Current_Time (Gnattest_T : in out Test) is
   --  mutime-info.ads:55:4:Get_Current_Time
--  end read only

      pragma Unreferenced (Gnattest_T);

      Tz : constant := 14 * 60 * 60 * (10 ** 6);
      Ti : constant := 62 * (10 ** 9);

      Ts : Timestamp_Type;
      T  : Time_Info_Type :=
        (TSC_Time_Base      => Epoch_Timestamp,
         TSC_Tick_Rate_Mhz  => 1000,
         Timezone_Microsecs => Tz);
      C  : Integer_62;
   begin
      Get_Current_Time (Time_Info      => T,
                        Schedule_Ticks => Ti,
                        Correction     => C,
                        Timestamp      => Ts);

      Assert (Condition => C = Tz + (Ti / 1000),
              Message   => "Correction mismatch");
      Assert (Condition => Ts = Epoch_Timestamp + C,
              Message   => "Timestamp mismatch");
--  begin read only
   end Test_Get_Current_Time;
--  end read only

end Mutime.Info.Test_Data.Tests;
