--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Memhashes.Pre_Checks.Test_Data.

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
package body Memhashes.Pre_Checks.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Hash_References (Gnattest_T : in out Test);
   procedure Test_Hash_References_3a68ae (Gnattest_T : in out Test) renames Test_Hash_References;
--  id:2.2/3a68ae38770d6a85/Hash_References/1/0/
   procedure Test_Hash_References (Gnattest_T : in out Test) is
   --  memhashes-pre_checks.ads:27:4:Hash_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Hash_References (Data => Policy);

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/memory/memory/hashRef",
         Name  => "memory",
         Value => "nonexistent");

      begin
         Hash_References (Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical memory region 'nonexistent' referenced by "
                    & "hashRef of memory region 'dst1' does not exist",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Hash_References;
--  end read only


--  begin read only
   procedure Test_Register_All (Gnattest_T : in out Test);
   procedure Test_Register_All_3f90ea (Gnattest_T : in out Test) renames Test_Register_All;
--  id:2.2/3f90ea30314141bf/Register_All/1/0/
   procedure Test_Register_All (Gnattest_T : in out Test) is
   --  memhashes-pre_checks.ads:30:4:Register_All
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Check_Procs.Get_Count = 0,
              Message   => "Initial count not zero:"
              & Check_Procs.Get_Count'Img);
      Register_All;
      Assert (Condition => Check_Procs.Get_Count = 3,
              Message   => "Count mismatch:" & Check_Procs.Get_Count'Img);
      Clear;

   exception
      when others =>
         Clear;
         raise;
--  begin read only
   end Test_Register_All;
--  end read only


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_aabd4c (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/aabd4c81c4d5a23a/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  memhashes-pre_checks.ads:33:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Unused : Muxml.XML_Data_Type;
   begin
      Check_Procs.Register (Process => Inc_Counter'Access);
      Run (Data      => Unused,
           Input_Dir => "input_dir");

      Assert (Condition => Test_Counter = 1,
              Message   => "Counter mismatch");
      Clear;

   exception
      when others =>
         Clear;
         raise;
--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Get_Count (Gnattest_T : in out Test);
   procedure Test_Get_Count_1fbd7c (Gnattest_T : in out Test) renames Test_Get_Count;
--  id:2.2/1fbd7c784b3d55c2/Get_Count/1/0/
   procedure Test_Get_Count (Gnattest_T : in out Test) is
   --  memhashes-pre_checks.ads:38:4:Get_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Check_Procs.Register (Process => Inc_Counter'Access);
      Assert (Condition => Check_Procs.Get_Count = 1,
              Message   => "Procs not one:" & Check_Procs.Get_Count'Img);
      Clear;

   exception
      when others =>
         Clear;
         raise;
--  begin read only
   end Test_Get_Count;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_4b4f85 (Gnattest_T : in out Test) renames Test_Clear;
--  id:2.2/4b4f85da05a9b689/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test) is
   --  memhashes-pre_checks.ads:41:4:Clear
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
end Memhashes.Pre_Checks.Test_Data.Tests;
