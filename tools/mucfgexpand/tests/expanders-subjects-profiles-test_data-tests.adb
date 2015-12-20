--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Subjects.Profiles.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Expanders.Subjects.Profiles.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Handle_Linux_Profile (Gnattest_T : in out Test);
   procedure Test_Handle_Linux_Profile_71d669 (Gnattest_T : in out Test) renames Test_Handle_Linux_Profile;
--  id:2.2/71d6693a77e1ae5b/Handle_Linux_Profile/1/0/
   procedure Test_Handle_Linux_Profile (Gnattest_T : in out Test) is
   --  expanders-subjects-profiles.ads:28:4:Handle_Linux_Profile
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_profiles_bios.xml",
         Ref_Diff => "data/subjects_profiles_bios.xml.diff",
         Pre      => Prepare_Profile_BIOS'Access,
         Expander => Handle_Profile'Access);
--  begin read only
   end Test_Handle_Linux_Profile;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Expanders.Subjects.Profiles.Test_Data.Tests;
