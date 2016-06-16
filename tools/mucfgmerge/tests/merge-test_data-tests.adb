--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Merge.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Merge.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_6c1c8f (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/6c1c8ff63395de3b/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  merge.ads:23:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure No_Additional_Hw
      is
         Output : constant String := "obj/run_no_additional_hw.xml";
      begin
         Run (Policy_File        => "data/test_policy.xml",
              Hardware_File      => "data/hardware.xml",
              Additional_Hw_File => "",
              Platform_File      => "data/platform.xml",
              Output_File        => Output);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/run_no_additional_hw.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => "obj/run_no_additional_hw.xml");
      end No_Additional_Hw;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Output : constant String := "obj/run.xml";
      begin
         Run (Policy_File        => "data/test_policy.xml",
              Hardware_File      => "data/hardware.xml",
              Additional_Hw_File => "data/additional_hw.xml",
              Platform_File      => "data/platform.xml",
              Output_File        => Output);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/run.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => "obj/run.xml");
      end Positive_Test;
   begin
      No_Additional_Hw;
      Positive_Test;
--  begin read only
   end Test_Run;
--  end read only

end Merge.Test_Data.Tests;