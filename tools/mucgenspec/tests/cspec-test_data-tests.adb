--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cspec.Test_Data.

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
package body Cspec.Test_Data.Tests is

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
   procedure Test_Run_e5a2dd (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e5a2dd86b12d7902/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dir : constant String := "obj/outdir";
      P   : constant String := "_component";
   begin
      Component:
      declare
         C : constant String := "vt";
      begin
         Run (Input_Spec       => "data/component_vt.xml",
              Output_Directory => Dir);

         Assert (Condition => Ada.Directories.Exists (Name => Dir),
                 Message   => "Directory not created (1)");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & ".ads",
                  Filename2 => "data/" & C & P & ".ads"),
                 Message   => C & P & ".ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & ".adb",
                  Filename2 => "data/" & C & P & ".adb"),
                 Message   => C & P & ".adb mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-config.ads",
                  Filename2 => "data/" & C & P & "-config.ads"),
                 Message   => C & P & "-config.ads mismatch");
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
                 (Filename1 => Dir & "/" & C & P & "-events.ads",
                  Filename2 => "data/" & C & P & "-events.ads"),
                 Message   => C & P & "-events.ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-memory_arrays.ads",
                  Filename2 => "data/" & C & P & "-memory_arrays.ads"),
                 Message   => C & P & "-memory_arrays.ads mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-channel_arrays.ads",
                  Filename2 => "data/" & C & P & "-channel_arrays.ads"),
                 Message   => C & P & "-channel_arrays.ads mismatch");
      end Component;

      Ada.Directories.Delete_Tree (Directory => Dir);

      Library:
      declare
         C : constant String := "libdebug";
      begin
         Run (Input_Spec       => "data/library_debug.xml",
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
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & "-events.ads",
                  Filename2 => "data/" & C & P & "-events.ads"),
                 Message   => C & P & "-events.ads mismatch");
      end Library;

      Ada.Directories.Delete_Tree (Directory => Dir);

      Package_Name :
      declare
         C : constant String := "xt";
      begin
         Run (Input_Spec       => "data/component_vt.xml",
              Output_Directory => Dir,
              Package_Name     => "xt");

         Assert (Condition => Ada.Directories.Exists (Name => Dir),
                 Message   => "Directory not created (6)");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Dir & "/" & C & P & ".ads",
                  Filename2 => "data/" & C & P & ".ads"),
                 Message   => C & P & ".ads mismatch");
      end Package_Name;

      Ada.Directories.Delete_Tree (Directory => Dir);

       --  No resources found.

      Run (Input_Spec       => "data/component_nores.xml",
           Output_Directory => Dir);

      Assert (Condition => Ada.Directories.Exists (Name => Dir),
              Message   => "Directory not created (7)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Dir & "/no_res_component.ads",
               Filename2 => "data/no_res_component.ads"),
              Message   => "Top-level spec mismatch");

      Ada.Directories.Delete_Tree (Directory => Dir);
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
end Cspec.Test_Data.Tests;
