--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Layouts.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Paging.Layouts.Test_Data.Tests is


--  begin read only
   procedure Test_Set_Address (Gnattest_T : in out Test);
   procedure Test_Set_Address_9d0225 (Gnattest_T : in out Test) renames Test_Set_Address;
--  id:2.2/9d0225d7a0c1251e/Set_Address/1/0/
   procedure Test_Set_Address (Gnattest_T : in out Test) is
   --  paging-layouts.ads:34:4:Set_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Set_Address;
--  end read only


--  begin read only
   procedure Test_Get_Address (Gnattest_T : in out Test);
   procedure Test_Get_Address_963b67 (Gnattest_T : in out Test) renames Test_Get_Address;
--  id:2.2/963b678c82b80686/Get_Address/1/0/
   procedure Test_Get_Address (Gnattest_T : in out Test) is
   --  paging-layouts.ads:39:4:Get_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Address;
--  end read only


--  begin read only
   procedure Test_Set_Large_Page_Support (Gnattest_T : in out Test);
   procedure Test_Set_Large_Page_Support_ade5cf (Gnattest_T : in out Test) renames Test_Set_Large_Page_Support;
--  id:2.2/ade5cfaf8657f33e/Set_Large_Page_Support/1/0/
   procedure Test_Set_Large_Page_Support (Gnattest_T : in out Test) is
   --  paging-layouts.ads:44:4:Set_Large_Page_Support
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Set_Large_Page_Support;
--  end read only

end Paging.Layouts.Test_Data.Tests;
