--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bin_Split.Spec.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Bin_Split.Spec.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Fill_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Fill_Entry_8b460f (Gnattest_T : in out Test) renames Test_Add_Fill_Entry;
--  id:2.2/8b460fbd51b295b2/Add_Fill_Entry/1/0/
   procedure Test_Add_Fill_Entry (Gnattest_T : in out Test) is
   --  bin_split-spec.ads:35:4:Add_Fill_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Add_Fill_Entry;
--  end read only


--  begin read only
   procedure Test_Add_File_Entry (Gnattest_T : in out Test);
   procedure Test_Add_File_Entry_f30ecc (Gnattest_T : in out Test) renames Test_Add_File_Entry;
--  id:2.2/f30eccc8f250fb29/Add_File_Entry/1/0/
   procedure Test_Add_File_Entry (Gnattest_T : in out Test) is
   --  bin_split-spec.ads:55:4:Add_File_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Add_File_Entry;
--  end read only

end Bin_Split.Spec.Test_Data.Tests;
