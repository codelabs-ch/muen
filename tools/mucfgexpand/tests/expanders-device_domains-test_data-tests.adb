--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Device_Domains.Test_Data.

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
package body Expanders.Device_Domains.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

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
        (Filename => "obj/device_domains_skeleton.xml",
         Ref_Diff => "data/device_domains_skeleton.xml.diff",
         Pre      => Remove_Device_Domains'Access,
         Expander => Add_Section_Skeleton'Access);
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
        (Filename => "obj/device_domains_ids.xml",
         Ref_Diff => "data/device_domains_ids.xml.diff",
         Pre      => Add_Section_Skeleton'Access,
         Expander => Add_Domain_IDs'Access);
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
        (Filename => "obj/device_domains_tables.xml",
         Ref_Diff => "data/device_domains_tables.xml.diff",
         Pre      => Add_Section_Skeleton_And_Kernel'Access,
         Expander => Add_Tables'Access);
--  begin read only
   end Test_Add_Tables;
--  end read only


--  begin read only
   procedure Test_Add_Reserved_Memory_Region_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Reserved_Memory_Region_Mappings_48a720 (Gnattest_T : in out Test) renames Test_Add_Reserved_Memory_Region_Mappings;
--  id:2.2/48a720bc6d127fd2/Add_Reserved_Memory_Region_Mappings/1/0/
   procedure Test_Add_Reserved_Memory_Region_Mappings (Gnattest_T : in out Test) is
   --  expanders-device_domains.ads:34:4:Add_Reserved_Memory_Region_Mappings
--  end read only

      pragma Unreferenced (Gnattest_T);
   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/device_domains_reserved_memory_regions.xml",
         Ref_Diff => "data/device_domains_reserved_memory_regions.xml.diff",
         Pre      => Add_Section_Skeleton_And_RMRRs'Access,
         Expander => Add_Reserved_Memory_Region_Mappings'Access);

      Test_Utils.Expander.Run_Test
        (Filename => "obj/device_domains_reserved_memory_regions_"
         & "nomem.xml",
         Ref_Diff => "data/device_domains_reserved_memory_regions_"
         & "nomem.xml.diff",
         Pre      => Prepare_Dev_Domain_Without_Mem'Access,
         Expander => Add_Reserved_Memory_Region_Mappings'Access);
--  begin read only
   end Test_Add_Reserved_Memory_Region_Mappings;
--  end read only


--  begin read only
   procedure Test_Remove_Map_Reserved_Mem_Attribute (Gnattest_T : in out Test);
   procedure Test_Remove_Map_Reserved_Mem_Attribute_7d25a8 (Gnattest_T : in out Test) renames Test_Remove_Map_Reserved_Mem_Attribute;
--  id:2.2/7d25a82518ed9200/Remove_Map_Reserved_Mem_Attribute/1/0/
   procedure Test_Remove_Map_Reserved_Mem_Attribute (Gnattest_T : in out Test) is
   --  expanders-device_domains.ads:38:4:Remove_Map_Reserved_Mem_Attribute
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/device_domains_map_reserved_mem_attr.xml",
         Ref_Diff => "data/device_domains_map_reserved_mem_attr.xml.diff",
         Expander => Remove_Map_Reserved_Mem_Attribute'Access);
--  begin read only
   end Test_Remove_Map_Reserved_Mem_Attribute;
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
end Expanders.Device_Domains.Test_Data.Tests;
