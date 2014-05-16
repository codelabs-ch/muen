--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Zp.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Zp.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Create_e820_Map (Gnattest_T : in out Test);
   procedure Test_Create_e820_Map_f36a26 (Gnattest_T : in out Test) renames Test_Create_e820_Map;
--  id:2.2/f36a2614086ebbe1/Create_e820_Map/1/0/
   procedure Test_Create_e820_Map (Gnattest_T : in out Test) is
   --  zp-utils.ads:27:4:Create_e820_Map
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Create_e820_Map;
--  end read only

end Zp.Utils.Test_Data.Tests;
