--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Merge.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Merge.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Lookup_File (Gnattest_T : in out Test);
   procedure Test_Lookup_File_007216 (Gnattest_T : in out Test) renames Test_Lookup_File;
--  id:2.2/0072162147f78f0f/Lookup_File/1/0/
   procedure Test_Lookup_File (Gnattest_T : in out Test) is
   --  merge-utils.ads:27:4:Lookup_File
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Lookup_File
              (Filename    => "run.xml",
               Directories => (1 => U ("nonexistent/path"),
                               2 => U ("obj"),
                               3 => U ("data"),
                               4 => U ("src"))) = "data/run.xml",
              Message   => "Directory mismatch (1)");

      Ada.Directories.Copy_File
        (Source_Name => "data/run.xml",
         Target_Name => "obj/run.xml");
      Assert (Condition => Lookup_File
              (Filename    => "run.xml",
               Directories => (1 => U ("nonexistent/path"),
                               2 => U ("obj"),
                               3 => U ("data"),
                               4 => U ("src"))) = "obj/run.xml",
              Message   => "Directory mismatch (2)");

      begin
         declare
            Dummy : constant String
              := Lookup_File
                (Filename    => "run.xml",
                 Directories => (1 => U ("nonexistent/path"),
                                 2 => U ("some/other/path"),
                                 3 => U ("src")));
         begin
            Assert (Condition => False,
                    Message   => "Exception expected (1)");
         end;

      exception
         when E : File_Not_Found =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "File 'run.xml' not found in any of the specified "
                    & "directories",
                    Message   => "Exception message mismatch (1)");
      end;

      begin
         declare
            Dummy : constant String
              := Lookup_File
                (Filename    => "nonexistent.xml",
                 Directories => (1 => U ("data"),
                                 2 => U ("src")));
         begin
            Assert (Condition => False,
                    Message   => "Exception expected (2)");
         end;

      exception
         when E : File_Not_Found =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "File 'nonexistent.xml' not found in any of the"
                    & " specified directories",
                    Message   => "Exception message mismatch (2)");
      end;

      Ada.Directories.Delete_File (Name => "obj/run.xml");
--  begin read only
   end Test_Lookup_File;
--  end read only

end Merge.Utils.Test_Data.Tests;
