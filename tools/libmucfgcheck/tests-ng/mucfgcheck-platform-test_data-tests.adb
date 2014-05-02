--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Platform.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Platform.Test_Data.Tests is


--  begin read only
   procedure Test_Memory_Space (Gnattest_T : in out Test);
   procedure Test_Memory_Space_20d775 (Gnattest_T : in out Test) renames Test_Memory_Space;
--  id:2.2/20d775fbae27b871/Memory_Space/1/0/
   procedure Test_Memory_Space (Gnattest_T : in out Test) is
   --  mucfgcheck-platform.ads:25:4:Memory_Space
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Memory_Space;
--  end read only


--  begin read only
   procedure Test_Memory_Block_Overlap (Gnattest_T : in out Test);
   procedure Test_Memory_Block_Overlap_12597b (Gnattest_T : in out Test) renames Test_Memory_Block_Overlap;
--  id:2.2/12597bd947918ca6/Memory_Block_Overlap/1/0/
   procedure Test_Memory_Block_Overlap (Gnattest_T : in out Test) is
   --  mucfgcheck-platform.ads:28:4:Memory_Block_Overlap
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Memory_Block_Overlap;
--  end read only

end Mucfgcheck.Platform.Test_Data.Tests;
