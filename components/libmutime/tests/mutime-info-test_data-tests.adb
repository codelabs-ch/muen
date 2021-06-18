--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutime.Info.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Mutime.Info.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Is_Valid (Gnattest_T : in out Test);
   procedure Test_Is_Valid_f361e9 (Gnattest_T : in out Test) renames Test_Is_Valid;
--  id:2.2/f361e911d85a7ba5/Is_Valid/1/0/
   procedure Test_Is_Valid (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested with Test_Valid");
--  begin read only
   end Test_Is_Valid;
--  end read only


--  begin read only
   procedure Test_2_Get_Current_Time (Gnattest_T : in out Test);
   procedure Test_Get_Current_Time_7024f2 (Gnattest_T : in out Test) renames Test_2_Get_Current_Time;
--  id:2.2/7024f2e671a6d9a3/Get_Current_Time/0/0/
   procedure Test_2_Get_Current_Time (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested with Test_1_Get_Current_Time");
--  begin read only
   end Test_2_Get_Current_Time;
--  end read only


--  begin read only
   procedure Test_1_Get_Boot_Time (Gnattest_T : in out Test);
   procedure Test_Get_Boot_Time_8ad4c1 (Gnattest_T : in out Test) renames Test_1_Get_Boot_Time;
--  id:2.2/8ad4c1d1687c803d/Get_Boot_Time/1/0/
   procedure Test_1_Get_Boot_Time (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      T  : Time_Info_Type
        := (TSC_Time_Base      => 1234567,
            TSC_Tick_Rate_Hz   => 1 * 10 ** 9,
            Timezone_Microsecs => 0);
      Timestamp : Timestamp_Type;
   begin
      Get_Boot_Time (TI        => T,
                     Timestamp => Timestamp);

      Assert (Condition => Timestamp = 1234567,
              Message   => "Timestamp mismatch");
--  begin read only
   end Test_1_Get_Boot_Time;
--  end read only


--  begin read only
   procedure Test_Valid (Gnattest_T : in out Test);
   procedure Test_Valid_b7cce7 (Gnattest_T : in out Test) renames Test_Valid;
--  id:2.2/b7cce7d60dedeb4c/Valid/1/0/
   procedure Test_Valid (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      TI : Time_Info_Type;
   begin
      TI := (TSC_Time_Base      => 1,
             TSC_Tick_Rate_Hz   => TSC_Tick_Rate_Hz_Type'First,
             Timezone_Microsecs => Timezone_Type'First);
      Assert (Condition => Valid (TI => TI),
              Message   => "Time info not valid");

      TI.TSC_Time_Base := Timestamp_Type'First;
      Assert (Condition => not Valid (TI => TI),
              Message   => "Time info valid (TSC Time Base 0)");
      TI.TSC_Time_Base := Timestamp_Type'Base'Last;
      Assert (Condition => not Valid (TI => TI),
              Message   => "Time info valid (TSC Time Base Last)");
      TI.TSC_Time_Base := 1;

      TI.TSC_Tick_Rate_Hz := TSC_Tick_Rate_Hz_Type'Base'Last;
      Assert (Condition => not Valid (TI => TI),
              Message   => "Time info valid (TSC Tick Rate)");
      TI.TSC_Tick_Rate_Hz := TSC_Tick_Rate_Hz_Type'First;

      TI.Timezone_Microsecs := Timezone_Type'Base'Last;
      Assert (Condition => not Valid (TI => TI),
              Message   => "Time info valid (Timezone)");
--  begin read only
   end Test_Valid;
--  end read only


--  begin read only
   procedure Test_1_Get_Current_Time (Gnattest_T : in out Test);
   procedure Test_Get_Current_Time_cea631 (Gnattest_T : in out Test) renames Test_1_Get_Current_Time;
--  id:2.2/cea6319c99ce0b67/Get_Current_Time/1/0/
   procedure Test_1_Get_Current_Time (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Tz : constant := 14 * 60 * 60 * (10 ** 6);
      Ti : constant := 62 * (10 ** 9);

      Ts : Timestamp_Type;
      T  : Time_Info_Type :=
        (TSC_Time_Base      => Epoch_Timestamp,
         TSC_Tick_Rate_Hz   => 1 * 10 ** 9,
         Timezone_Microsecs => Tz);
      C  : Integer_62;
   begin
      Get_Current_Time (TI             => T,
                        Schedule_Ticks => Ti,
                        Correction     => C,
                        Timestamp      => Ts);

      Assert (Condition => C = Tz + (Ti / 1000),
              Message   => "Correction mismatch");
      Assert (Condition => Ts = Epoch_Timestamp + C,
              Message   => "Timestamp mismatch");
--  begin read only
   end Test_1_Get_Current_Time;
--  end read only


--  begin read only
   procedure Test_2_Get_Boot_Time (Gnattest_T : in out Test);
   procedure Test_Get_Boot_Time_7d002a (Gnattest_T : in out Test) renames Test_2_Get_Boot_Time;
--  id:2.2/7d002a3b4ef43af3/Get_Boot_Time/0/0/
   procedure Test_2_Get_Boot_Time (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested with Test_1_Get_Boot_Time");
--  begin read only
   end Test_2_Get_Boot_Time;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Mutime.Info.Test_Data.Tests;
