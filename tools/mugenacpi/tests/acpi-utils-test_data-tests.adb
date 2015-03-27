--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Acpi.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Acpi.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Indent (Gnattest_T : in out Test);
   procedure Test_Indent_399a7a (Gnattest_T : in out Test) renames Test_Indent;
--  id:2.2/399a7ad24f629c80/Indent/1/0/
   procedure Test_Indent (Gnattest_T : in out Test) is
   --  acpi-utils.ads:24:4:Indent
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Already tested in Libmutools");
--  begin read only
   end Test_Indent;
--  end read only

end Acpi.Utils.Test_Data.Tests;
