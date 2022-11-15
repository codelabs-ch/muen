--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Mergers.Test_Data.

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
package body Mutools.Mergers.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Merge_Config_Section (Gnattest_T : in out Test);
   procedure Test_Merge_Config_Section_d6f1b2 (Gnattest_T : in out Test) renames Test_Merge_Config_Section;
--  id:2.2/d6f1b28e7c4ea535/Merge_Config_Section/1/0/
   procedure Test_Merge_Config_Section (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      -- this test is implemented in mucfgmerge.mergers and omitted here
      Assert (Condition => True,
              Message   => "Tested in mucfgmerge.mergers");

--  begin read only
   end Test_Merge_Config_Section;
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
end Mutools.Mergers.Test_Data.Tests;
