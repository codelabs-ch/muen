--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Subjects.Profiles.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Expanders.Subjects.Profiles.Test_Data.Tests is


--  begin read only
   procedure Test_Handle_Linux_Profile (Gnattest_T : in out Test);
   procedure Test_Handle_Linux_Profile_71d669 (Gnattest_T : in out Test) renames Test_Handle_Linux_Profile;
--  id:2.2/71d6693a77e1ae5b/Handle_Linux_Profile/1/0/
   procedure Test_Handle_Linux_Profile (Gnattest_T : in out Test) is
   --  expanders-subjects-profiles.ads:28:4:Handle_Linux_Profile
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Handle_Profile test");
--  begin read only
   end Test_Handle_Linux_Profile;
--  end read only

end Expanders.Subjects.Profiles.Test_Data.Tests;