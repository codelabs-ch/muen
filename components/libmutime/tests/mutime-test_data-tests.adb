--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutime.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mutime.Test_Data.Tests is


--  begin read only
   procedure Test_Time_Of (Gnattest_T : in out Test);
   procedure Test_Time_Of_a0832e (Gnattest_T : in out Test) renames Test_Time_Of;
--  id:2.2/a0832e6f81ca9164/Time_Of/1/0/
   procedure Test_Time_Of (Gnattest_T : in out Test) is
   --  mutime.ads:43:4:Time_Of
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Time_Of
              (Year   => 1970,
               Month  => 1,
               Day    => 1,
               Hour   => 0,
               Minute => 0,
               Second => 0) = 0,
              Message   => "Timestamp mismatch (1)");
      Assert (Condition => Time_Of
              (Year   => 2015,
               Month  => 8,
               Day    => 3,
               Hour   => 9,
               Minute => 18,
               Second => 48) = 1438593528 * 10 ** 6,
              Message   => "Timestamp mismatch (2)");
      Assert (Condition => Time_Of
              (Year   => 1971,
               Month  => 5,
               Day    => 23,
               Hour   => 23,
               Minute => 12,
               Second => 31) = 43888351 * 10 ** 6,
              Message   => "Timestamp mismatch (3)");
      Assert (Condition => Time_Of
              (Year   => 2000,
               Month  => 1,
               Day    => 1,
               Hour   => 0,
               Minute => 0,
               Second => 0) = 946684800 * 10 ** 6,
              Message   => "Timestamp mismatch (4)");
      Assert (Condition => Time_Of
              (Year   => 1980,
               Month  => 2,
               Day    => 29,
               Hour   => 10,
               Minute => 10,
               Second => 11) = 320667011 * 10 ** 6,
              Message   => "Timestamp mismatch (5)");
--  begin read only
   end Test_Time_Of;
--  end read only


--  begin read only
   procedure Test_Split (Gnattest_T : in out Test);
   procedure Test_Split_3c222d (Gnattest_T : in out Test) renames Test_Split;
--  id:2.2/3c222d0aebc09370/Split/1/0/
   procedure Test_Split (Gnattest_T : in out Test) is
   --  mutime.ads:53:4:Split
--  end read only

      pragma Unreferenced (Gnattest_T);

      Y   : Year_Type;
      M   : Month_Type;
      D   : Day_Type;
      H   : Hour_Type;
      Min : Minute_Type;
      S   : Second_Type;
   begin
      Split (Time   => 0,
             Year   => Y,
             Month  => M,
             Day    => D,
             Hour   => H,
             Minute => Min,
             Second => S);
      Assert (Condition => Y = 1970,
              Message   => "Year mismatch (1):" & Y'Img);
      Assert (Condition => M = 1,
              Message   => "Month mismatch (1):" & M'Img);
      Assert (Condition => D = 1,
              Message   => "Day mismatch (1):" & D'Img);
      Assert (Condition => H = 0,
              Message   => "Hour mismatch (1):" & H'Img);
      Assert (Condition => Min = 0,
              Message   => "Minute mismatch (1):" & Min'Img);
      Assert (Condition => S = 0,
              Message   => "Second mismatch (1):" & S'Img);

      Split (Time   => 48313583112 * 10 ** 6,
             Year   => Y,
             Month  => M,
             Day    => D,
             Hour   => H,
             Minute => Min,
             Second => S);
      Assert (Condition => Y = 3500,
              Message   => "Year mismatch (2):" & Y'Img);
      Assert (Condition => M = 12,
              Message   => "Month mismatch (2):" & M'Img);
      Assert (Condition => D = 30,
              Message   => "Day mismatch (2):" & D'Img);
      Assert (Condition => H = 23,
              Message   => "Hour mismatch (2):" & H'Img);
      Assert (Condition => Min = 45,
              Message   => "Minute mismatch (2):" & Min'Img);
      Assert (Condition => S = 12,
              Message   => "Second mismatch (2):" & S'Img);

      Split (Time   => 2678998713 * 10 ** 6,
             Year   => Y,
             Month  => M,
             Day    => D,
             Hour   => H,
             Minute => Min,
             Second => S);
      Assert (Condition => Y = 2054,
              Message   => "Year mismatch (3):" & Y'Img);
      Assert (Condition => M = 11,
              Message   => "Month mismatch (3):" & M'Img);
      Assert (Condition => D = 22,
              Message   => "Day mismatch (3):" & D'Img);
      Assert (Condition => H = 22,
              Message   => "Hour mismatch (3):" & H'Img);
      Assert (Condition => Min = 18,
              Message   => "Minute mismatch (3):" & Min'Img);
      Assert (Condition => S = 33,
              Message   => "Second mismatch (3):" & S'Img);

      Split (Time   => 253402300799 * 10 ** 6,
             Year   => Y,
             Month  => M,
             Day    => D,
             Hour   => H,
             Minute => Min,
             Second => S);
      Assert (Condition => Y = 9999,
              Message   => "Year mismatch (4):" & Y'Img);
      Assert (Condition => M = 12,
              Message   => "Month mismatch (4):" & M'Img);
      Assert (Condition => D = 31,
              Message   => "Day mismatch (4):" & D'Img);
      Assert (Condition => H = 23,
              Message   => "Hour mismatch (4):" & H'Img);
      Assert (Condition => Min = 59,
              Message   => "Minute mismatch (4):" & Min'Img);
      Assert (Condition => S = 59,
              Message   => "Second mismatch (4):" & S'Img);

      --  Leap years.

      Split (Time      => 13450924572 * 10 ** 6,
             Year      => Y,
             Month     => M,
             Day       => D,
             Hour      => H,
             Minute    => Min,
             Second    => S);
      Assert (Condition => Y = 2396,
              Message   => "Year mismatch (5):" & Y'Img);
      Assert (Condition => M = 3,
              Message   => "Month mismatch (5):" & M'Img);
      Assert (Condition => D = 29,
              Message   => "Day mismatch (5):" & D'Img);
      Assert (Condition => H = 23,
              Message   => "Hour mismatch (5):" & H'Img);
      Assert (Condition => Min = 56,
              Message   => "Minute mismatch (5):" & Min'Img);
      Assert (Condition => S = 12,
              Message   => "Second mismatch (5):" & S'Img);

      Split (Time      => 320667011 * 10 ** 6,
             Year      => Y,
             Month     => M,
             Day       => D,
             Hour      => H,
             Minute    => Min,
             Second    => S);
      Assert (Condition => Y = 1980,
              Message   => "Year mismatch (6):" & Y'Img);
      Assert (Condition => M = 2,
              Message   => "Month mismatch (6):" & M'Img);
      Assert (Condition => D = 29,
              Message   => "Day mismatch (6):" & D'Img);
      Assert (Condition => H = 10,
              Message   => "Hour mismatch (6):" & H'Img);
      Assert (Condition => Min = 10,
              Message   => "Minute mismatch (6):" & Min'Img);
      Assert (Condition => S = 11,
              Message   => "Second mismatch (6):" & S'Img);

      --  Assure that the last 6 digits have no effect (fraction of a second).

      Split (Time      => 320667011999999,
             Year      => Y,
             Month     => M,
             Day       => D,
             Hour      => H,
             Minute    => Min,
             Second    => S);
      Assert (Condition => Y = 1980,
              Message   => "Year mismatch (7):" & Y'Img);
      Assert (Condition => M = 2,
              Message   => "Month mismatch (7):" & M'Img);
      Assert (Condition => D = 29,
              Message   => "Day mismatch (7):" & D'Img);
      Assert (Condition => H = 10,
              Message   => "Hour mismatch (7):" & H'Img);
      Assert (Condition => Min = 10,
              Message   => "Minute mismatch (7):" & Min'Img);
      Assert (Condition => S = 11,
              Message   => "Second mismatch (7):" & S'Img);
--  begin read only
   end Test_Split;
--  end read only


--  begin read only
   procedure Test_Get_Month_And_Day (Gnattest_T : in out Test);
   procedure Test_Get_Month_And_Day_7c71b5 (Gnattest_T : in out Test) renames Test_Get_Month_And_Day;
--  id:2.2/7c71b57f9ae57aa9/Get_Month_And_Day/1/0/
   procedure Test_Get_Month_And_Day (Gnattest_T : in out Test) is
   --  mutime.ads:72:4:Get_Month_And_Day
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

end Mutime.Test_Data.Tests;
