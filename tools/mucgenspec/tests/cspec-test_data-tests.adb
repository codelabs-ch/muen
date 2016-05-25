--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cspec.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Cspec.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_e5a2dd (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e5a2dd86b12d7902/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  cspec.ads:23:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dir : constant String := "obj/outdir";
   begin
      Run (Policy_File      => "data/test_policy.xml",
           Component_Name   => "tau0",
           Output_Directory => Dir);

      Assert (Condition => Ada.Directories.Exists (Name => Dir),
              Message   => "Directory not created");
      Ada.Directories.Delete_Tree (Directory => Dir);
--  begin read only
   end Test_Run;
--  end read only

end Cspec.Test_Data.Tests;
