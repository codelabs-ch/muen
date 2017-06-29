--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Pack.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_b41e7b (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/b41e7ba747ab173c/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  pack.ads:26:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Execute_Dry_Run
      is
         Imgpath : constant String := "obj/myimage_dry.img";
      begin
         Run (Policy_File    => "data/execute_dry_run.xml",
              Input_Dir      => "data",
              Output_Dir     => "obj",
              Output_Imgname => "myimage_dry.img",
              Dry_Run        => True);

         Assert (Condition => Pre_Checks.Get_Count = 0,
                 Message   => "Pre checks still registered (3)");
         Assert (Condition => Post_Checks.Get_Count = 0,
                 Message   => "Post checks still registered (3)");
         Assert (Condition => Content_Providers.Get_Count = 0,
                 Message   => "Content providers still registered (3)");
         Assert (Condition => not Ada.Directories.Exists (Name => Imgpath),
                 Message   => "System image found");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Imgpath & ".manifest",
                  Filename2 => "data/execute_run_dry.manifest"),
                 Message   => "Manifest file differs (2)");

         Ada.Directories.Delete_File (Name => Imgpath & ".manifest");
      end Execute_Dry_Run;

      ----------------------------------------------------------------------

      procedure Execute_Run
      is
         Imgpath : constant String := "obj/myimage.img";
      begin
         Run (Policy_File    => "data/execute_run.xml",
              Input_Dir      => "data",
              Output_Dir     => "obj",
              Output_Imgname => "myimage.img",
              Dry_Run        => False);

         Assert (Condition => Pre_Checks.Get_Count = 0,
                 Message   => "Pre checks still registered (1)");
         Assert (Condition => Post_Checks.Get_Count = 0,
                 Message   => "Post checks still registered (1)");
         Assert (Condition => Content_Providers.Get_Count = 0,
                 Message   => "Content providers still registered (1)");
         Assert (Condition => Ada.Directories.Exists (Name => Imgpath),
                 Message   => "System image not found");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Imgpath,
                  Filename2 => "data/execute_run.img"),
                 Message   => "Image file differs");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Imgpath & ".manifest",
                  Filename2 => "data/execute_run.manifest"),
                 Message   => "Manifest file differs (1)");

         Ada.Directories.Delete_File (Name => Imgpath);
         Ada.Directories.Delete_File (Name => Imgpath & ".manifest");
      end Execute_Run;

      ----------------------------------------------------------------------

      procedure Execute_Run_No_Content
      is
      begin
         Run (Policy_File    => "data/test_policy.xml",
              Input_Dir      => "data",
              Output_Dir     => "obj",
              Output_Imgname => "myimage.img",
              Dry_Run        => False);

      exception
         when E : Pack_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Image size is zero, no content to pack",
                    Message   => "Exception mismatch");
            Assert (Condition => Pre_Checks.Get_Count = 0,
                    Message   => "Pre checks still registered (2)");
            Assert (Condition => Post_Checks.Get_Count = 0,
                    Message   => "Post checks still registered (2)");
            Assert (Condition => Content_Providers.Get_Count = 0,
                    Message   => "Content providers still registered (2)");
      end Execute_Run_No_Content;
   begin
      Execute_Run;
      Execute_Run_No_Content;
      Execute_Dry_Run;
--  begin read only
   end Test_Run;
--  end read only

end Pack.Test_Data.Tests;
