--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Types.Test_Data.

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
package body Mutools.Types.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_Max_ID (Gnattest_T : in out Test);
   procedure Test_Get_Max_ID_a65afa (Gnattest_T : in out Test) renames Test_Get_Max_ID;
--  id:2.2/a65afae2a79d6438/Get_Max_ID/1/0/
   procedure Test_Get_Max_ID (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Get_Max_ID (Group => Vmx_Exit) = 59,
              Message   => "Invalid VMX exit max ID");
      Assert (Condition => Get_Max_ID (Group => Vmcall) = 63,
              Message   => "Invalid Vmcall max ID");
--  begin read only
   end Test_Get_Max_ID;
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
end Mutools.Types.Test_Data.Tests;
