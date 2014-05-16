--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Pack.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_e5a2dd (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e5a2dd86b12d7902/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  pack.ads:25:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_U (Gnattest_T : in out Test);
   procedure Test_U_0031f0 (Gnattest_T : in out Test) renames Test_U;
--  id:2.2/0031f06ca01d7683/U/1/0/
   procedure Test_U (Gnattest_T : in out Test) is
   --  pack.ads:34:4:U
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_U;
--  end read only


--  begin read only
   procedure Test_S (Gnattest_T : in out Test);
   procedure Test_S_e42122 (Gnattest_T : in out Test) renames Test_S;
--  id:2.2/e42122b64eb45b09/S/1/0/
   procedure Test_S (Gnattest_T : in out Test) is
   --  pack.ads:39:4:S
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_S;
--  end read only

end Pack.Test_Data.Tests;
