--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expand.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expand.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_aabd4c (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/aabd4c81c4d5a23a/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  expand.ads:25:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Filename : constant String := "obj/execute_run.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Run (Policy      => Policy,
           Output_File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/execute_run.ref.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
      Pre_Checks.Clear;
      Post_Checks.Clear;
--  begin read only
   end Test_Run;
--  end read only

end Expand.Test_Data.Tests;
