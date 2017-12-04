--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.OS.Test_Data.

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
package body Mutools.OS.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Execute (Gnattest_T : in out Test);
   procedure Test_Execute_be370a (Gnattest_T : in out Test) renames Test_Execute;
--  id:2.2/be370a9becf523af/Execute/1/0/
   procedure Test_Execute (Gnattest_T : in out Test) is
   --  mutools-os.ads:24:4:Execute
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Execute_Command
      is
      begin
         Execute (Command => "true");

         --  Should not raise an exception.

      end Execute_Command;

      ----------------------------------------------------------------------

      procedure Failing_Command
      is
      begin
         Execute (Command => "nonexistent");

      exception
         when E : Command_Failed =>
            Assert
              (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = "Command 'nonexistent' failed",
               Message   => "Command not failed");
      end Failing_Command;
   begin
      Execute_Command;
      Failing_Command;
--  begin read only
   end Test_Execute;
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
end Mutools.OS.Test_Data.Tests;
