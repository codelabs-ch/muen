--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Siblings.Test_Data.

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
package body Expanders.Siblings.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Subject_Profile_VCPU (Gnattest_T : in out Test);
   procedure Test_Add_Subject_Profile_VCPU_8ab93c (Gnattest_T : in out Test) renames Test_Add_Subject_Profile_VCPU;
--  id:2.2/8ab93cbcf52e18f0/Add_Subject_Profile_VCPU/1/0/
   procedure Test_Add_Subject_Profile_VCPU (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/siblings_add_profile.xml",
         Ref_Diff => "data/siblings_add_profile.xml.diff",
         Pre      => Prepare_Profile'Access,
         Expander => Add_Subject_Profile_VCPU'Access);
--  begin read only
   end Test_Add_Subject_Profile_VCPU;
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
end Expanders.Siblings.Test_Data.Tests;
