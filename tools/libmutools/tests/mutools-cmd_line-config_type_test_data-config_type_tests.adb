--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Cmd_Line.Config_Type_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Mutools.Cmd_Line.Config_Type_Test_Data.Config_Type_Tests is


--  begin read only
   procedure Test_Finalize (Gnattest_T : in out Test_Config_Type);
   procedure Test_Finalize_1d29f1 (Gnattest_T : in out Test_Config_Type) renames Test_Finalize;
--  id:2.2/1d29f15228a8f8f4/Finalize/1/0/
   procedure Test_Finalize (Gnattest_T : in out Test_Config_Type) is
   --  mutools-cmd_line.ads:31:4:Finalize
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      AUnit.Assertions.Assert
        (Condition => True,
         Message   => "GNAT.Command_Line.Free not testable");
--  begin read only
   end Test_Finalize;
--  end read only

end Mutools.Cmd_Line.Config_Type_Test_Data.Config_Type_Tests;
