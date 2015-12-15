--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Platform.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expanders.Platform.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Section_Skeleton (Gnattest_T : in out Test);
   procedure Test_Add_Section_Skeleton_797fa9 (Gnattest_T : in out Test) renames Test_Add_Section_Skeleton;
--  id:2.2/797fa93bd19d8580/Add_Section_Skeleton/1/0/
   procedure Test_Add_Section_Skeleton (Gnattest_T : in out Test) is
   --  expanders-platform.ads:25:4:Add_Section_Skeleton
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/platform_skeleton.xml",
         Ref_Filename => "data/platform_skeleton.ref.xml",
         Pre          => Remove_Platform_Section'Access,
         Expander     => Add_Section_Skeleton'Access);
--  begin read only
   end Test_Add_Section_Skeleton;
--  end read only


--  begin read only
   procedure Test_Resolve_Device_Aliases (Gnattest_T : in out Test);
   procedure Test_Resolve_Device_Aliases_2c46de (Gnattest_T : in out Test) renames Test_Resolve_Device_Aliases;
--  id:2.2/2c46dec9d926880c/Resolve_Device_Aliases/1/0/
   procedure Test_Resolve_Device_Aliases (Gnattest_T : in out Test) is
   --  expanders-platform.ads:28:4:Resolve_Device_Aliases
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/platform_device_alias.xml",
         Ref_Filename => "data/platform_device_alias.ref.xml",
         Expander     => Resolve_Device_Aliases'Access);
--  begin read only
   end Test_Resolve_Device_Aliases;
--  end read only

end Expanders.Platform.Test_Data.Tests;
