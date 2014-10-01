--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Device_Domains.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expanders.Device_Domains.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Section_Skeleton (Gnattest_T : in out Test);
   procedure Test_Add_Section_Skeleton_797fa9 (Gnattest_T : in out Test) renames Test_Add_Section_Skeleton;
--  id:2.2/797fa93bd19d8580/Add_Section_Skeleton/1/0/
   procedure Test_Add_Section_Skeleton (Gnattest_T : in out Test) is
   --  expanders-device_domains.ads:25:4:Add_Section_Skeleton
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/device_domains_skeleton.xml",
         Ref_Filename => "data/device_domains_skeleton.ref.xml",
         Pre          => Remove_Device_Domains'Access,
         Expander     => Add_Section_Skeleton'Access);
--  begin read only
   end Test_Add_Section_Skeleton;
--  end read only


--  begin read only
   procedure Test_Add_Domain_IDs (Gnattest_T : in out Test);
   procedure Test_Add_Domain_IDs_acdb9f (Gnattest_T : in out Test) renames Test_Add_Domain_IDs;
--  id:2.2/acdb9ff1b910151d/Add_Domain_IDs/1/0/
   procedure Test_Add_Domain_IDs (Gnattest_T : in out Test) is
   --  expanders-device_domains.ads:28:4:Add_Domain_IDs
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/device_domains_ids.xml",
         Ref_Filename => "data/device_domains_ids.ref.xml",
         Pre          => Add_Section_Skeleton'Access,
         Expander     => Add_Domain_IDs'Access);
--  begin read only
   end Test_Add_Domain_IDs;
--  end read only


--  begin read only
   procedure Test_Add_Tables (Gnattest_T : in out Test);
   procedure Test_Add_Tables_17f65c (Gnattest_T : in out Test) renames Test_Add_Tables;
--  id:2.2/17f65ca27fa9a88d/Add_Tables/1/0/
   procedure Test_Add_Tables (Gnattest_T : in out Test) is
   --  expanders-device_domains.ads:31:4:Add_Tables
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/device_domains_tables.xml",
         Ref_Filename => "data/device_domains_tables.ref.xml",
         Pre          => Add_Section_Skeleton_And_Kernel'Access,
         Expander     => Add_Tables'Access);
--  begin read only
   end Test_Add_Tables;
--  end read only

end Expanders.Device_Domains.Test_Data.Tests;
