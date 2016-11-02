--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.Tables.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body VTd.Tables.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_6bf94d (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/6bf94dcde86c129b/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  vtd-tables.ads:38:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      AUnit.Assertions.Assert
        (Condition => True,
         Message   => "Tested in DMAR/IR Serialize tests");
--  begin read only
   end Test_Write;
--  end read only

end VTd.Tables.Test_Data.Tests;
