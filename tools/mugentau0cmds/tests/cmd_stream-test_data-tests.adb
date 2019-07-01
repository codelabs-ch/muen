--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cmd_Stream.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Directories;

with Muxml;

with Test_Utils;
--  begin read only
--  end read only
package body Cmd_Stream.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_aabd4c (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/aabd4c81c4d5a23a/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  cmd_stream.ads:28:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Fn     : constant String := "test_policy_cmds.xml";
      Fn_Obj : constant String := "obj/" & Fn;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Run (Policy      => Policy,
           Output_File => Fn_Obj);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/" & Fn,
               Filename2 => Fn_Obj),
              Message   => "Files differ");
      Ada.Directories.Delete_File (Name => Fn_Obj);
--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_S (Gnattest_T : in out Test);
   procedure Test_S_e42122 (Gnattest_T : in out Test) renames Test_S;
--  id:2.2/e42122b64eb45b09/S/1/0/
   procedure Test_S (Gnattest_T : in out Test) is
   --  cmd_stream.ads:34:4:S
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Helper function via rename");
--  begin read only
   end Test_S;
--  end read only


--  begin read only
   procedure Test_U (Gnattest_T : in out Test);
   procedure Test_U_0031f0 (Gnattest_T : in out Test) renames Test_U;
--  id:2.2/0031f06ca01d7683/U/1/0/
   procedure Test_U (Gnattest_T : in out Test) is
   --  cmd_stream.ads:39:4:U
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Helper function via rename");
--  begin read only
   end Test_U;
--  end read only


--  begin read only
   procedure Test_Trim (Gnattest_T : in out Test);
   procedure Test_Trim_94bfac (Gnattest_T : in out Test) renames Test_Trim;
--  id:2.2/94bfac163d7308cf/Trim/1/0/
   procedure Test_Trim (Gnattest_T : in out Test) is
   --  cmd_stream.ads:44:4:Trim
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Helper function via rename");
--  begin read only
   end Test_Trim;
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
end Cmd_Stream.Test_Data.Tests;
