--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Features.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expanders.Features.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Default_Features (Gnattest_T : in out Test);
   procedure Test_Add_Default_Features_d0ea97 (Gnattest_T : in out Test) renames Test_Add_Default_Features;
--  id:2.2/d0ea97d5d2b23d44/Add_Default_Features/1/0/
   procedure Test_Add_Default_Features (Gnattest_T : in out Test) is
   --  expanders-features.ads:25:4:Add_Default_Features
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/features_default.xml",
         Ref_Diff => "data/features_default.xml.diff",
         Pre      => Remove_Features'Access,
         Expander => Add_Default_Features'Access);
--  begin read only
   end Test_Add_Default_Features;
--  end read only

end Expanders.Features.Test_Data.Tests;
