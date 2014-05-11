--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mergers.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mergers.Test_Data.Tests is


--  begin read only
   procedure Test_Merge_Platform (Gnattest_T : in out Test);
   procedure Test_Merge_Platform_0cd3e6 (Gnattest_T : in out Test) renames Test_Merge_Platform;
--  id:2.2/0cd3e620cc856c4c/Merge_Platform/1/0/
   procedure Test_Merge_Platform (Gnattest_T : in out Test) is
   --  mergers.ads:25:4:Merge_Platform
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Merge_Platform;
--  end read only

end Mergers.Test_Data.Tests;
