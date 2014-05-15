--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Msrbm.MSRs.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Msrbm.MSRs.Test_Data.Tests is


--  begin read only
   procedure Test_Allow_MSRs (Gnattest_T : in out Test);
   procedure Test_Allow_MSRs_c99a60 (Gnattest_T : in out Test) renames Test_Allow_MSRs;
--  id:2.2/c99a60488753a1e9/Allow_MSRs/1/0/
   procedure Test_Allow_MSRs (Gnattest_T : in out Test) is
   --  msrbm-msrs.ads:43:4:Allow_MSRs
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Allow_MSRs;
--  end read only


--  begin read only
   procedure Test_To_Stream (Gnattest_T : in out Test);
   procedure Test_To_Stream_a9353e (Gnattest_T : in out Test) renames Test_To_Stream;
--  id:2.2/a9353e0af3662022/To_Stream/1/0/
   procedure Test_To_Stream (Gnattest_T : in out Test) is
   --  msrbm-msrs.ads:55:4:To_Stream
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_To_Stream;
--  end read only

end Msrbm.MSRs.Test_Data.Tests;
