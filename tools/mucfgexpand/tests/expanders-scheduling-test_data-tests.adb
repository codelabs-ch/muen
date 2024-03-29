--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Scheduling.Test_Data.

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
package body Expanders.Scheduling.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Partition_IDs (Gnattest_T : in out Test);
   procedure Test_Add_Partition_IDs_2c8af4 (Gnattest_T : in out Test) renames Test_Add_Partition_IDs;
--  id:2.2/2c8af4fd310fd409/Add_Partition_IDs/1/0/
   procedure Test_Add_Partition_IDs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/scheduling_partition_ids.xml",
         Ref_Diff => "data/scheduling_partition_ids.xml.diff",
         Expander => Add_Partition_IDs'Access);
--  begin read only
   end Test_Add_Partition_IDs;
--  end read only


--  begin read only
   procedure Test_Add_Group_IDs (Gnattest_T : in out Test);
   procedure Test_Add_Group_IDs_1b9a9c (Gnattest_T : in out Test) renames Test_Add_Group_IDs;
--  id:2.2/1b9a9ce478d7ce72/Add_Group_IDs/1/0/
   procedure Test_Add_Group_IDs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/scheduling_group_ids.xml",
         Ref_Diff => "data/scheduling_group_ids.xml.diff",
         Expander => Add_Group_IDs'Access);
--  begin read only
   end Test_Add_Group_IDs;
--  end read only


--  begin read only
   procedure Test_Add_Partition_CPU_IDs (Gnattest_T : in out Test);
   procedure Test_Add_Partition_CPU_IDs_0f7b0a (Gnattest_T : in out Test) renames Test_Add_Partition_CPU_IDs;
--  id:2.2/0f7b0a4bbfefa06b/Add_Partition_CPU_IDs/1/0/
   procedure Test_Add_Partition_CPU_IDs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/scheduling_partition_cpu_ids.xml",
         Ref_Diff => "data/scheduling_partition_cpu_ids.xml.diff",
         Expander => Add_Partition_CPU_IDs'Access);
--  begin read only
   end Test_Add_Partition_CPU_IDs;
--  end read only


--  begin read only
   procedure Test_Add_Barrier_Configs (Gnattest_T : in out Test);
   procedure Test_Add_Barrier_Configs_057525 (Gnattest_T : in out Test) renames Test_Add_Barrier_Configs;
--  id:2.2/057525ba5aaf3024/Add_Barrier_Configs/1/0/
   procedure Test_Add_Barrier_Configs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/scheduling_barrier_configs.xml",
         Ref_Diff => "data/scheduling_barrier_configs.xml.diff",
         Expander => Add_Barrier_Configs'Access);
--  begin read only
   end Test_Add_Barrier_Configs;
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
end Expanders.Scheduling.Test_Data.Tests;
