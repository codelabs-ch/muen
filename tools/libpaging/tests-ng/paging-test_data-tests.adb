--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Paging.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Indexes (Gnattest_T : in out Test);
   procedure Test_Get_Indexes_b21229 (Gnattest_T : in out Test) renames Test_Get_Indexes;
--  id:2.2/b21229569172abbf/Get_Indexes/1/0/
   procedure Test_Get_Indexes (Gnattest_T : in out Test) is
   --  paging.ads:48:4:Get_Indexes
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Indexes;
--  end read only

end Paging.Test_Data.Tests;
