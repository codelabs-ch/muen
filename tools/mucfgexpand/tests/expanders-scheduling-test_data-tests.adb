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
   procedure Test_Add_Barrier_Configs (Gnattest_T : in out Test);
   procedure Test_Add_Barrier_Configs_057525 (Gnattest_T : in out Test) renames Test_Add_Barrier_Configs;
--  id:2.2/057525ba5aaf3024/Add_Barrier_Configs/1/0/
   procedure Test_Add_Barrier_Configs (Gnattest_T : in out Test) is
   --  expanders-scheduling.ads:25:4:Add_Barrier_Configs
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
