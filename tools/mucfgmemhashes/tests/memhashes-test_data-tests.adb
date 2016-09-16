--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Memhashes.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Memhashes.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_e84213 (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e84213e130018c54/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  memhashes.ads:25:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fname : constant String := "obj/test_policy_hashes.xml";
   begin
      Run (Policy_In  => "data/test_policy.xml",
           Policy_Out => Fname,
           Input_Dir  => "data");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Fname,
               Filename2 => "data/test_policy_hashes.xml"),
              Message   => "Policy mismatch");
      Ada.Directories.Delete_File (Name => Fname);
--  begin read only
   end Test_Run;
--  end read only

end Memhashes.Test_Data.Tests;