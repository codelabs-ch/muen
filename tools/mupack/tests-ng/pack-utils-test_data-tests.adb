--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Pack.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Image_Size (Gnattest_T : in out Test);
   procedure Test_Get_Image_Size_046aa6 (Gnattest_T : in out Test) renames Test_Get_Image_Size;
--  id:2.2/046aa68f3a717a5c/Get_Image_Size/1/0/
   procedure Test_Get_Image_Size (Gnattest_T : in out Test) is
   --  pack-utils.ads:27:4:Get_Image_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Image_Size;
--  end read only

end Pack.Utils.Test_Data.Tests;
