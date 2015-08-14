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

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

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

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Month_And_Day;
--  end read only

end Mutime.Test_Data.Tests;
