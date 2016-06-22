--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cspec.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Cspec.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_e5a2dd (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e5a2dd86b12d7902/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  cspec.ads:23:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dir : constant String := "obj/outdir";
      P   : constant String := "_component";
   begin
      Component:
      declare
         C : constant String := "vt";
      begin
         Run (Policy_File      => "data/test_policy.xml",
              Component_Name   => C,
              Output_Directory => Dir);

         Assert (Condition => Ada.Directories.Exists (Name => Dir),
                 Message   => "Directory not created (1)");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & ".ads",
                  Filename2 => "data/" & C & P & ".ads"),
                 Message   => C & P & ".ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-memory.ads",
                  Filename2 => "data/" & C & P & "-memory.ads"),
                 Message   => C & P & "-memory.ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-channels.ads",
                  Filename2 => "data/" & C & P & "-channels.ads"),
                 Message   => C & P & "-channels.ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-devices.ads",
                  Filename2 => "data/" & C & P & "-devices.ads"),
                 Message   => C & P & "-devices.ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-memory_arrays.ads",
                  Filename2 => "data/" & C & P & "-memory_arrays.ads"),
                 Message   => C & P & "-memory_arrays.ads mismatch");
      end Component;

      Library:
      declare
         C : constant String := "libdebug";
      begin
         Run (Policy_File      => "data/test_policy.xml",
              Component_Name   => C,
              Output_Directory => Dir);

         Assert (Condition => Ada.Directories.Exists (Name => Dir),
                 Message   => "Directory not created (2)");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & ".ads",
                  Filename2 => "data/" & C & P & ".ads"),
                 Message   => C & P & ".ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-memory.ads",
                  Filename2 => "data/" & C & P & "-memory.ads"),
                 Message   => C & P & "-memory.ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-devices.ads",
                  Filename2 => "data/" & C & P & "-devices.ads"),
                 Message   => C & P & "-devices.ads mismatch");
      end Library;

      Ada.Directories.Delete_Tree (Directory => Dir);

      begin
         Run (Policy_File      => "data/test_policy.xml",
              Component_Name   => "nonexistent",
              Output_Directory => Dir);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Component_Not_Found =>
            Assert (Condition => not Ada.Directories.Exists (Name => Dir),
                    Message   => "Out directory created (1)");
      end;

      --  No resources found.

      Run (Policy_File      => "data/test_policy.xml",
           Component_Name   => "no_res",
           Output_Directory => "obj");
      Assert (Condition => not Ada.Directories.Exists (Name => Dir),
              Message   => "Out directory created (2)");
--  begin read only
   end Test_Run;
--  end read only

end Cspec.Test_Data.Tests;