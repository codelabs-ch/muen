--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack.Post_Checks.Test_Data.

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
package body Pack.Post_Checks.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Multiboot_Header (Gnattest_T : in out Test);
   procedure Test_Multiboot_Header_a1691f (Gnattest_T : in out Test) renames Test_Multiboot_Header;
--  id:2.2/a1691fde93ca7ba7/Multiboot_Header/1/0/
   procedure Test_Multiboot_Header (Gnattest_T : in out Test) is
   --  pack-post_checks.ads:27:4:Multiboot_Header
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Content_Providers.Param_Type (End_Address => 16#102000#,
                                           Dry_Run     => False);
   begin
      begin
         Multiboot_Header (Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Check_Error => null;
      end;

      Mutools.Image.Add_Pattern (Image   => Data.Image,
                                 Pattern => 16#02#,
                                 Size    => 1,
                                 Address => 16#101ffc#);
      Mutools.Image.Add_Pattern (Image   => Data.Image,
                                 Pattern => 16#b0#,
                                 Size    => 1,
                                 Address => 16#101ffd#);
      Mutools.Image.Add_Pattern (Image   => Data.Image,
                                 Pattern => 16#ad#,
                                 Size    => 1,
                                 Address => 16#101ffe#);
      Mutools.Image.Add_Pattern (Image   => Data.Image,
                                 Pattern => 16#1b#,
                                 Size    => 1,
                                 Address => 16#101fff#);
      Multiboot_Header (Data => Data);
--  begin read only
   end Test_Multiboot_Header;
--  end read only


--  begin read only
   procedure Test_Register_All (Gnattest_T : in out Test);
   procedure Test_Register_All_3f90ea (Gnattest_T : in out Test) renames Test_Register_All;
--  id:2.2/3f90ea30314141bf/Register_All/1/0/
   procedure Test_Register_All (Gnattest_T : in out Test) is
   --  pack-post_checks.ads:30:4:Register_All
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Register_All;
      Assert (Condition => Check_Procs.Get_Count = 1,
              Message   => "Count mismatch");
--  begin read only
   end Test_Register_All;
--  end read only


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_ca760b (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/ca760b43164ae370/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  pack-post_checks.ads:33:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Unused : Content_Providers.Param_Type
        := (End_Address => 12,
            Dry_Run     => False,
            others      => <>);
   begin
      Check_Procs.Register (Process => Inc_Counter'Access);
      Run (Data => Unused);

      Assert (Condition => Test_Counter = 1,
              Message   => "Counter mismatch");
--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Get_Count (Gnattest_T : in out Test);
   procedure Test_Get_Count_1fbd7c (Gnattest_T : in out Test) renames Test_Get_Count;
--  id:2.2/1fbd7c784b3d55c2/Get_Count/1/0/
   procedure Test_Get_Count (Gnattest_T : in out Test) is
   --  pack-post_checks.ads:36:4:Get_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Check_Procs.Register (Process => Inc_Counter'Access);
      Assert (Condition => Check_Procs.Get_Count = 1,
              Message   => "Procs not one:" & Check_Procs.Get_Count'Img);
--  begin read only
   end Test_Get_Count;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_4b4f85 (Gnattest_T : in out Test) renames Test_Clear;
--  id:2.2/4b4f85da05a9b689/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test) is
   --  pack-post_checks.ads:39:4:Clear
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Check_Procs.Register (Process => Inc_Counter'Access);
      Assert (Condition => Check_Procs.Get_Count = 1,
              Message   => "Procs not one:" & Check_Procs.Get_Count'Img);

      Clear;
      Assert (Condition => Check_Procs.Get_Count = 0,
              Message   => "Procs not cleared:" & Check_Procs.Get_Count'Img);
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
end Pack.Post_Checks.Test_Data.Tests;
