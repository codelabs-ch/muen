--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Stackcheck.Test_Data.

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
package body Stackcheck.Test_Data.Tests is

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
   procedure Test_Run_81ffbb (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/81ffbb75b0753062/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  stackcheck.ads:27:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Failure : Boolean;
   begin
      Run (Project_File => "data/testci",
           Limit        => 224,
           Overflow     => Failure);
      Assert (Condition => not Failure,
              Message   => "Failure with limit 224");

      Run (Project_File => "data/testci",
           Limit        => 223,
           Overflow     => Failure);
      Assert (Condition => Failure,
              Message   => "No failure with limit 223");

      Run (Project_File => "data/testci",
           Limit        => 0,
           Overflow     => Failure);
      Assert (Condition => Failure,
              Message   => "No failure with limit 0");
--  begin read only
   end Test_Run;
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
end Stackcheck.Test_Data.Tests;
