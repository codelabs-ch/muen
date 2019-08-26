--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Acpi.Test_Data.

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
package body Acpi.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_To_ID_4 (Gnattest_T : in out Test);
   procedure Test_To_ID_4_e1b59a (Gnattest_T : in out Test) renames Test_To_ID_4;
--  id:2.2/e1b59a6b9d0ef843/To_ID_4/1/0/
   procedure Test_To_ID_4 (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ID : ID_4;
   begin
      ID := To_ID_4 (Str => "Muen");
      Assert (Condition => ID'Length = 4,
              Message   => "Length mismatch");
      Assert (Condition => ID = (16#4d#, 16#75#, 16#65#, 16#6e#),
              Message   => "Content mismatch");
--  begin read only
   end Test_To_ID_4;
--  end read only


--  begin read only
   procedure Test_To_ID_6 (Gnattest_T : in out Test);
   procedure Test_To_ID_6_e79ef5 (Gnattest_T : in out Test) renames Test_To_ID_6;
--  id:2.2/e79ef5be8ab3e586/To_ID_6/1/0/
   procedure Test_To_ID_6 (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ID : ID_6;
   begin
      ID := To_ID_6 (Str => "Muen");
      Assert (Condition => ID'Length = 6,
              Message   => "Length mismatch");
      Assert (Condition => ID =
              (16#4d#, 16#75#, 16#65#, 16#6e#, 16#20#, 16#20#),
              Message   => "Content mismatch");
--  begin read only
   end Test_To_ID_6;
--  end read only


--  begin read only
   procedure Test_To_ID_8 (Gnattest_T : in out Test);
   procedure Test_To_ID_8_2d240f (Gnattest_T : in out Test) renames Test_To_ID_8;
--  id:2.2/2d240f1845b60b24/To_ID_8/1/0/
   procedure Test_To_ID_8 (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ID : ID_8;
   begin
      ID := To_ID_8 (Str => "Muen");
      Assert (Condition => ID'Length = 8,
              Message   => "Length mismatch");
      Assert (Condition => ID =
              (16#4d#, 16#75#, 16#65#, 16#6e#, 16#20#, 16#20#, 16#20#, 16#20#),
              Message   => "Content mismatch");
--  begin read only
   end Test_To_ID_8;
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
end Acpi.Test_Data.Tests;
