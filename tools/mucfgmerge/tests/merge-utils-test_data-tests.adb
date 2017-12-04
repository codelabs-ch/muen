--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Merge.Utils.Test_Data.

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
package body Merge.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Lookup_File (Gnattest_T : in out Test);
   procedure Test_Lookup_File_fc67f1 (Gnattest_T : in out Test) renames Test_Lookup_File;
--  id:2.2/fc67f18c520e0b64/Lookup_File/1/0/
   procedure Test_Lookup_File (Gnattest_T : in out Test) is
   --  merge-utils.ads:25:4:Lookup_File
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Lookup_File
              (Name => "/tmp",
               Dirs => "data") = "/tmp",
              Message   => "File mismatch (1)");
      Assert (Condition => Lookup_File
              (Name => "test_policy.xml",
               Dirs => "/nonexistent:foo/bar:data:/home")
              = "data/test_policy.xml",
              Message   => "File mismatch (2)");
--  begin read only
   end Test_Lookup_File;
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
end Merge.Utils.Test_Data.Tests;
