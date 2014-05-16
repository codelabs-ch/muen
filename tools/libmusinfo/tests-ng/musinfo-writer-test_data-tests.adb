--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Musinfo.Writer.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Musinfo.Writer.Test_Data.Tests is


--  begin read only
   procedure Test_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_d5673b (Gnattest_T : in out Test) renames Test_Serialize;
--  id:2.2/d5673ba8214929bb/Serialize/1/0/
   procedure Test_Serialize (Gnattest_T : in out Test) is
   --  musinfo-writer.ads:24:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Serialize;
--  end read only

end Musinfo.Writer.Test_Data.Tests;
