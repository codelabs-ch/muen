--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack.Manifest.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Pack.Manifest.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Entry_a5a36f (Gnattest_T : in out Test) renames Test_Add_Entry;
--  id:2.2/a5a36f31266c59ba/Add_Entry/1/0/
   procedure Test_Add_Entry (Gnattest_T : in out Test) is
   --  pack-manifest.ads:30:4:Add_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Add_Entry;
--  end read only


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_afa96b (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/afa96b009f0d8d47/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  pack-manifest.ads:40:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Write;
--  end read only

end Pack.Manifest.Test_Data.Tests;
