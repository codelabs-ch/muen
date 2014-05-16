--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bzpatch.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Bzpatch.Test_Data.Tests is


--  begin read only
   procedure Test_Patch (Gnattest_T : in out Test);
   procedure Test_Patch_4c544e (Gnattest_T : in out Test) renames Test_Patch;
--  id:2.2/4c544ef13505f519/Patch/1/0/
   procedure Test_Patch (Gnattest_T : in out Test) is
   --  bzpatch.ads:24:4:Patch
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Patch;
--  end read only

end Bzpatch.Test_Data.Tests;
