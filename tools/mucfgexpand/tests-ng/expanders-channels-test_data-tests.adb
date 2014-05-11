--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Channels.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expanders.Channels.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Physical_Memory (Gnattest_T : in out Test);
   procedure Test_Add_Physical_Memory_127041 (Gnattest_T : in out Test) renames Test_Add_Physical_Memory;
--  id:2.2/127041296e3a499b/Add_Physical_Memory/1/0/
   procedure Test_Add_Physical_Memory (Gnattest_T : in out Test) is
   --  expanders-channels.ads:24:4:Add_Physical_Memory
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Add_Physical_Memory;
--  end read only

end Expanders.Channels.Test_Data.Tests;
