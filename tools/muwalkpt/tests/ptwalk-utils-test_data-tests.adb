--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ptwalk.Utils.Test_Data.

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
package body Ptwalk.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_To_Number (Gnattest_T : in out Test);
   procedure Test_To_Number_48f364 (Gnattest_T : in out Test) renames Test_To_Number;
--  id:2.2/48f364dbae9e251d/To_Number/1/0/
   procedure Test_To_Number (Gnattest_T : in out Test) is
   --  ptwalk-utils.ads:25:4:To_Number
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;
   begin
      Assert (Condition => To_Number (Str => "0") = 0,
              Message   => "Number mismatch (1)");
      Assert (Condition => To_Number (Str => "0x0") = 0,
              Message   => "Number mismatch (2)");
      Assert (Condition => To_Number (Str => "16#4000#") = 16#4000#,
              Message   => "Number mismatch (3)");
      Assert (Condition => To_Number (Str => "0xffffffffffffffff")
              = Interfaces.Unsigned_64'Last,
              Message   => "Number mismatch (4)");
--  begin read only
   end Test_To_Number;
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
end Ptwalk.Utils.Test_Data.Tests;
