--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Validation_Errors.Test_Data.

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
package body Mucfgcheck.Validation_Errors.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Insert (Gnattest_T : in out Test);
   procedure Test_Insert_74a32a (Gnattest_T : in out Test) renames Test_Insert;
--  id:2.2/74a32ad76da38cf4/Insert/1/0/
   procedure Test_Insert (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Is_Empty,
              Message   => "Error box not empty");

      Insert (Msg => "Test error");

      Assert (Condition => not Is_Empty,
              Message   => "Error box still empty");
--  begin read only
   end Test_Insert;
--  end read only


--  begin read only
   procedure Test_Check (Gnattest_T : in out Test);
   procedure Test_Check_76a05c (Gnattest_T : in out Test) renames Test_Check;
--  id:2.2/76a05c490f0afc15/Check/1/0/
   procedure Test_Check (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      --  Positive test, must not raise an exception.

      Check;

      Insert (Msg => "Test msg");

      begin
         Check;
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error => null;
      end;
--  begin read only
   end Test_Check;
--  end read only


--  begin read only
   procedure Test_Get_Error_Message (Gnattest_T : in out Test);
   procedure Test_Get_Error_Message_a8113e (Gnattest_T : in out Test) renames Test_Get_Error_Message;
--  id:2.2/a8113e4629fdb809/Get_Error_Message/1/0/
   procedure Test_Get_Error_Message (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Msg : constant String
        := "Found 2 error(s):" & ASCII.LF & ASCII.LF
        & " 1: Test msg1" & ASCII.LF
        & " 2: Test msg2" & ASCII.LF;
   begin
      Insert (Msg => "Test msg1");
      Insert (Msg => "Test msg2");

      Assert (Condition => Get_Error_Message = Ref_Msg,
              Message   => "Message mismatch");
--  begin read only
   end Test_Get_Error_Message;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_4b4f85 (Gnattest_T : in out Test) renames Test_Clear;
--  id:2.2/4b4f85da05a9b689/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Insert (Msg => "Test msg");
      Assert (Condition => not Is_Empty,
              Message   => "Error box still empty");
      Clear;
      Assert (Condition => Is_Empty,
              Message   => "Error box not empty after clear");
--  begin read only
   end Test_Clear;
--  end read only


--  begin read only
   procedure Test_Contains (Gnattest_T : in out Test);
   procedure Test_Contains_798827 (Gnattest_T : in out Test) renames Test_Contains;
--  id:2.2/798827bec567cedb/Contains/1/0/
   procedure Test_Contains (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => not Contains (Msg => "nonexistent"),
              Message   => "Unexpected msg");
      Insert (Msg => "New message");
      Assert (Condition => Contains (Msg => "New message"),
              Message   => "Message not found");
--  begin read only
   end Test_Contains;
--  end read only


--  begin read only
   procedure Test_Is_Empty (Gnattest_T : in out Test);
   procedure Test_Is_Empty_6cfacf (Gnattest_T : in out Test) renames Test_Is_Empty;
--  id:2.2/6cfacf4c6329513c/Is_Empty/1/0/
   procedure Test_Is_Empty (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Insert test");
--  begin read only
   end Test_Is_Empty;
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
end Mucfgcheck.Validation_Errors.Test_Data.Tests;
