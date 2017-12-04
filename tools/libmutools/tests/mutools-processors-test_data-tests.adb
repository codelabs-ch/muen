--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Processors.Test_Data.

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
package body Mutools.Processors.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Register (Gnattest_T : in out Test);
   procedure Test_Register_b121fc (Gnattest_T : in out Test) renames Test_Register;
--  id:2.2/b121fcc7e5b84c35/Register/1/0/
   procedure Test_Register (Gnattest_T : in out Test) is
   --  mutools-processors.ads:31:4:Register
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;
   begin
      Assert (Condition => Procs.Length = 1,
              Message   => "No procedure registered");
--  begin read only
   end Test_Register;
--  end read only


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_0faa3c (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/0faa3cd38e37f95c/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  mutools-processors.ads:34:4:Run
--  end read only
   begin
      Run (Data => Gnattest_T.Param.all);
      Assert (Condition => Instances.Counter = 25,
              Message   => "Counter mismatch" & Instances.Counter'Img);
      Instances.Counter := 0;
--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Get_Count (Gnattest_T : in out Test);
   procedure Test_Get_Count_1fbd7c (Gnattest_T : in out Test) renames Test_Get_Count;
--  id:2.2/1fbd7c784b3d55c2/Get_Count/1/0/
   procedure Test_Get_Count (Gnattest_T : in out Test) is
   --  mutools-processors.ads:37:4:Get_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Get_Count = 1,
              Message   => "Count not 1");
--  begin read only
   end Test_Get_Count;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_4b4f85 (Gnattest_T : in out Test) renames Test_Clear;
--  id:2.2/4b4f85da05a9b689/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test) is
   --  mutools-processors.ads:40:4:Clear
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Get_Count = 1,
              Message   => "Count not 1");
      Clear;
      Assert (Condition => Get_Count = 0,
              Message   => "Count not 0");
--  begin read only
   end Test_Clear;
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
end Mutools.Processors.Test_Data.Tests;
