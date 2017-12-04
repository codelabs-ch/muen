--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutime.Test_Data.

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
package body Mutime.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Time_Of (Gnattest_T : in out Test);
   procedure Test_Time_Of_d523a9 (Gnattest_T : in out Test) renames Test_Time_Of;
--  id:2.2/d523a9dcdb4da19a/Time_Of/1/0/
   procedure Test_Time_Of (Gnattest_T : in out Test) is
   --  mutime.ads:59:4:Time_Of
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Time_Of
              (Date_Time =>
                 (Year   => 1970,
                  Month  => 1,
                  Day    => 1,
                  Hour   => 0,
                  Minute => 0,
                  Second => 0)) = 0,
              Message   => "Timestamp mismatch (1)");
      Assert (Condition => Time_Of
              (Date_Time =>
                 (Year   => 2015,
                  Month  => 8,
                  Day    => 3,
                  Hour   => 9,
                  Minute => 18,
                  Second => 48)) = 1438593528 * 10 ** 6,
              Message   => "Timestamp mismatch (2)");
      Assert (Condition => Time_Of
              (Date_Time =>
                 (Year   => 1971,
                  Month  => 5,
                  Day    => 23,
                  Hour   => 23,
                  Minute => 12,
                  Second => 31)) = 43888351 * 10 ** 6,
              Message   => "Timestamp mismatch (3)");
      Assert (Condition => Time_Of
              (Date_Time =>
                 (Year   => 2000,
                  Month  => 1,
                  Day    => 1,
                  Hour   => 0,
                  Minute => 0,
                  Second => 0)) = 946684800 * 10 ** 6,
              Message   => "Timestamp mismatch (4)");
      Assert (Condition => Time_Of
              (Date_Time =>
                 (Year   => 1980,
                  Month  => 2,
                  Day    => 29,
                  Hour   => 10,
                  Minute => 10,
                  Second => 11)) = 320667011 * 10 ** 6,
              Message   => "Timestamp mismatch (5)");
      Assert (Condition => Time_Of
              (Date_Time =>
                 (Year   => 2016,
                  Month  => 5,
                  Day    => 2,
                  Hour   => 11,
                  Minute => 15,
                  Second => 02)) = 1462187702 * 10 ** 6,
              Message   => "Timestamp mismatch (6)");
--  begin read only
   end Test_Time_Of;
--  end read only


--  begin read only
   procedure Test_Split (Gnattest_T : in out Test);
   procedure Test_Split_9952e7 (Gnattest_T : in out Test) renames Test_Split;
--  id:2.2/9952e760e2df4669/Split/1/0/
   procedure Test_Split (Gnattest_T : in out Test) is
   --  mutime.ads:62:4:Split
--  end read only

      pragma Unreferenced (Gnattest_T);

      DT : Date_Time_Type;
   begin
      Split (Timestamp => 0,
             Date_Time => DT);
      Assert (Condition => DT.Year = 1970,
              Message   => "Year mismatch (1):" & DT.Year'Img);
      Assert (Condition => DT.Month = 1,
              Message   => "Month mismatch (1):" & DT.Month'Img);
      Assert (Condition => DT.Day = 1,
              Message   => "Day mismatch (1):" & DT.Day'Img);
      Assert (Condition => DT.Hour = 0,
              Message   => "Hour mismatch (1):" & DT.Hour'Img);
      Assert (Condition => DT.Minute = 0,
              Message   => "Minute mismatch (1):" & DT.Minute'Img);
      Assert (Condition => DT.Second = 0,
              Message   => "Second mismatch (1):" & DT.Second'Img);

      Split (Timestamp => 48313583112 * 10 ** 6,
             Date_Time => DT);
      Assert (Condition => DT.Year = 3500,
              Message   => "Year mismatch (2):" & DT.Year'Img);
      Assert (Condition => DT.Month = 12,
              Message   => "Month mismatch (2):" & DT.Month'Img);
      Assert (Condition => DT.Day = 30,
              Message   => "Day mismatch (2):" & DT.Day'Img);
      Assert (Condition => DT.Hour = 23,
              Message   => "Hour mismatch (2):" & DT.Hour'Img);
      Assert (Condition => DT.Minute = 45,
              Message   => "Minute mismatch (2):" & DT.Minute'Img);
      Assert (Condition => DT.Second = 12,
              Message   => "Second mismatch (2):" & DT.Second'Img);

      Split (Timestamp => 2678998713 * 10 ** 6,
             Date_Time => DT);
      Assert (Condition => DT.Year = 2054,
              Message   => "Year mismatch (3):" & DT.Year'Img);
      Assert (Condition => DT.Month = 11,
              Message   => "Month mismatch (3):" & DT.Month'Img);
      Assert (Condition => DT.Day = 22,
              Message   => "Day mismatch (3):" & DT.Day'Img);
      Assert (Condition => DT.Hour = 22,
              Message   => "Hour mismatch (3):" & DT.Hour'Img);
      Assert (Condition => DT.Minute = 18,
              Message   => "Minute mismatch (3):" & DT.Minute'Img);
      Assert (Condition => DT.Second = 33,
              Message   => "Second mismatch (3):" & DT.Second'Img);

      Split (Timestamp => 253402300799 * 10 ** 6,
             Date_Time => DT);
      Assert (Condition => DT.Year = 9999,
              Message   => "Year mismatch (4):" & DT.Year'Img);
      Assert (Condition => DT.Month = 12,
              Message   => "Month mismatch (4):" & DT.Month'Img);
      Assert (Condition => DT.Day = 31,
              Message   => "Day mismatch (4):" & DT.Day'Img);
      Assert (Condition => DT.Hour = 23,
              Message   => "Hour mismatch (4):" & DT.Hour'Img);
      Assert (Condition => DT.Minute = 59,
              Message   => "Minute mismatch (4):" & DT.Minute'Img);
      Assert (Condition => DT.Second = 59,
              Message   => "Second mismatch (4):" & DT.Second'Img);

      --  Leap years.

      Split (Timestamp => 13450924572 * 10 ** 6,
             Date_Time => DT);
      Assert (Condition => DT.Year = 2396,
              Message   => "Year mismatch (5):" & DT.Year'Img);
      Assert (Condition => DT.Month = 3,
              Message   => "Month mismatch (5):" & DT.Month'Img);
      Assert (Condition => DT.Day = 29,
              Message   => "Day mismatch (5):" & DT.Day'Img);
      Assert (Condition => DT.Hour = 23,
              Message   => "Hour mismatch (5):" & DT.Hour'Img);
      Assert (Condition => DT.Minute = 56,
              Message   => "Minute mismatch (5):" & DT.Minute'Img);
      Assert (Condition => DT.Second = 12,
              Message   => "Second mismatch (5):" & DT.Second'Img);

      Split (Timestamp => 320667011 * 10 ** 6,
             Date_Time => DT);
      Assert (Condition => DT.Year = 1980,
              Message   => "Year mismatch (6):" & DT.Year'Img);
      Assert (Condition => DT.Month = 2,
              Message   => "Month mismatch (6):" & DT.Month'Img);
      Assert (Condition => DT.Day = 29,
              Message   => "Day mismatch (6):" & DT.Day'Img);
      Assert (Condition => DT.Hour = 10,
              Message   => "Hour mismatch (6):" & DT.Hour'Img);
      Assert (Condition => DT.Minute = 10,
              Message   => "Minute mismatch (6):" & DT.Minute'Img);
      Assert (Condition => DT.Second = 11,
              Message   => "Second mismatch (6):" & DT.Second'Img);

      --  Assure that the last 6 digits have no effect (fraction of a second).

      Split (Timestamp => 320667011999999,
             Date_Time => DT);
      Assert (Condition => DT.Year = 1980,
              Message   => "Year mismatch (7):" & DT.Year'Img);
      Assert (Condition => DT.Month = 2,
              Message   => "Month mismatch (7):" & DT.Month'Img);
      Assert (Condition => DT.Day = 29,
              Message   => "Day mismatch (7):" & DT.Day'Img);
      Assert (Condition => DT.Hour = 10,
              Message   => "Hour mismatch (7):" & DT.Hour'Img);
      Assert (Condition => DT.Minute = 10,
              Message   => "Minute mismatch (7):" & DT.Minute'Img);
      Assert (Condition => DT.Second = 11,
              Message   => "Second mismatch (7):" & DT.Second'Img);
--  begin read only
   end Test_Split;
--  end read only


--  begin read only
   procedure Test_Minus (Gnattest_T : in out Test);
   procedure Test_Minus_689998 (Gnattest_T : in out Test) renames Test_Minus;
--  id:2.2/689998bec2d5aea1/Minus/1/0/
   procedure Test_Minus (Gnattest_T : in out Test) is
   --  mutime.ads:68:4:"-"
--  end read only

      pragma Unreferenced (Gnattest_T);

      T : Timestamp_Type := 124;
   begin
      Assert (Condition => T - Interfaces.Unsigned_64 (4) = 120,
              Message   => "Result mismatch (1)");
      Assert (Condition => T - Interfaces.Unsigned_64 (128) = 0,
              Message   => "Result mismatch (2)");
--  begin read only
   end Test_Minus;
--  end read only


--  begin read only
   procedure Test_Plus (Gnattest_T : in out Test);
   procedure Test_Plus_ca074c (Gnattest_T : in out Test) renames Test_Plus;
--  id:2.2/ca074cb7e2de085c/Plus/1/0/
   procedure Test_Plus (Gnattest_T : in out Test) is
   --  mutime.ads:82:4:"+"
--  end read only

      pragma Unreferenced (Gnattest_T);

      T : Timestamp_Type := 123;
   begin
      Assert (Condition => T + Integer_63 (123) = 246,
              Message   => "Result mismatch (1)");
      Assert (Condition => T + Integer_63 (-155) = 0,
              Message   => "Result mismatch (2)");

      T := Timestamp_Type'Last;
      Assert (Condition => T + Integer_63 (12) = Timestamp_Type'Last,
              Message   => "Result mismatch (3)");
--  begin read only
   end Test_Plus;
--  end read only


--  begin read only
   procedure Test_Get_Value (Gnattest_T : in out Test);
   procedure Test_Get_Value_9ee63f (Gnattest_T : in out Test) renames Test_Get_Value;
--  id:2.2/9ee63f1097e75da5/Get_Value/1/0/
   procedure Test_Get_Value (Gnattest_T : in out Test) is
   --  mutime.ads:88:4:Get_Value
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      T : Timestamp_Type := 123;
   begin
      Assert (Condition => Get_Value (Timestamp => T) = 123,
              Message   => "Value mismatch");
--  begin read only
   end Test_Get_Value;
--  end read only


--  begin read only
   procedure Test_Get_Month_And_Day (Gnattest_T : in out Test);
   procedure Test_Get_Month_And_Day_7c71b5 (Gnattest_T : in out Test) renames Test_Get_Month_And_Day;
--  id:2.2/7c71b57f9ae57aa9/Get_Month_And_Day/1/0/
   procedure Test_Get_Month_And_Day (Gnattest_T : in out Test) is
   --  mutime.ads:114:4:Get_Month_And_Day
--  end read only

      pragma Unreferenced (Gnattest_T);

      Month : Month_Type;
      Day   : Day_Type;
   begin
      Get_Month_And_Day
        (Days      => 1,
         Leap_Year => False,
         Month     => Month,
         Day       => Day);
      Assert (Condition => Month = 1,
              Message   => "Month mismatch (1):" & Month'Img);
      Assert (Condition => Day = 1,
              Message   => "Day mismatch (1):" & Day'Img);

      Get_Month_And_Day
        (Days      => 12,
         Leap_Year => False,
         Month     => Month,
         Day       => Day);
      Assert (Condition => Month = 1,
              Message   => "Month mismatch (2):" & Month'Img);
      Assert (Condition => Day = 12,
              Message   => "Day mismatch (2):" & Day'Img);

      Get_Month_And_Day
        (Days      => 31,
         Leap_Year => False,
         Month     => Month,
         Day       => Day);
      Assert (Condition => Month = 1,
              Message   => "Month mismatch (3):" & Month'Img);
      Assert (Condition => Day = 31,
              Message   => "Day mismatch (3):" & Day'Img);

      Get_Month_And_Day
        (Days      => 306,
         Leap_Year => True,
         Month     => Month,
         Day       => Day);
      Assert (Condition => Month = 11,
              Message   => "Month mismatch (4):" & Month'Img);
      Assert (Condition => Day = 1,
              Message   => "Day mismatch (4):" & Day'Img);

      Get_Month_And_Day
        (Days      => 366,
         Leap_Year => True,
         Month     => Month,
         Day       => Day);
      Assert (Condition => Month = 12,
              Message   => "Month mismatch (4):" & Month'Img);
      Assert (Condition => Day = 31,
              Message   => "Day mismatch (4):" & Day'Img);

      Get_Month_And_Day
        (Days      => 365,
         Leap_Year => False,
         Month     => Month,
         Day       => Day);
      Assert (Condition => Month = 12,
              Message   => "Month mismatch (5):" & Month'Img);
      Assert (Condition => Day = 31,
              Message   => "Day mismatch (5):" & Day'Img);

      Get_Month_And_Day
        (Days      => 59,
         Leap_Year => False,
         Month     => Month,
         Day       => Day);
      Assert (Condition => Month = 2,
              Message   => "Month mismatch (6):" & Month'Img);
      Assert (Condition => Day = 28,
              Message   => "Day mismatch (6):" & Day'Img);

      Get_Month_And_Day
        (Days      => 60,
         Leap_Year => False,
         Month     => Month,
         Day       => Day);
      Assert (Condition => Month = 3,
              Message   => "Month mismatch (7):" & Month'Img);
      Assert (Condition => Day = 1,
              Message   => "Day mismatch (7):" & Day'Img);

      Get_Month_And_Day
        (Days      => 60,
         Leap_Year => True,
         Month     => Month,
         Day       => Day);
      Assert (Condition => Month = 2,
              Message   => "Month mismatch (8):" & Month'Img);
      Assert (Condition => Day = 29,
              Message   => "Day mismatch (8):" & Day'Img);
--  begin read only
   end Test_Get_Month_And_Day;
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
end Mutime.Test_Data.Tests;
