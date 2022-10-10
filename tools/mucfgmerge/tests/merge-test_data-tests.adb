--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Merge.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;
--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Mucfgcheck.Validation_Errors;
with Muxml;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with DOM.Core.Nodes;
--  begin read only
--  end read only
package body Merge.Test_Data.Tests is

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

      ----------------------------------------------------------------------

      procedure Duplicate_Config_Value
      is
         Output : constant String := "obj/duplicate_cfg.xml";
      begin
         Run (Config_File  => "data/cfg_duplicate.xml",
              Output_File  => Output,
              Include_Path => "");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Mucfgcheck.Validation_Errors.Validation_Error =>
            Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                    (Msg => "The names given to config variables "
                     & "and expressions are not unique. Conflicting value: "
                     & "'supports_xhci_debug'"),
                    Message   => "Exception message mismatch");
      end Duplicate_Config_Value;

      ----------------------------------------------------------------------

      procedure Duplicate_Config_Value_Due_To_Template
      is
         Output : constant String := "obj/duplicate_cfg_template.xml";
      begin
         Run (Config_File  => "data/config_templateNameCollision.xml",
              Output_File  => Output,
              Include_Path => "data/");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Mucfgcheck.Validation_Errors.Validation_Error =>
            Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                    (Msg => "The names given to config variables and "
                     & "expressions are not unique. "
                     & "Conflicting value: 't3_memory_name'"),
                    Message   => "Exception message mismatch");
      end Duplicate_Config_Value_Due_To_Template;

      ----------------------------------------------------------------------

      procedure Include_Path
      is
         Output : constant String := "obj/run_include_path.xml";
      begin
         Run (Config_File  => "data/config_include_path.xml",
              Output_File  => Output,
              Include_Path => "data/hw:data/platform:data");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/run_include_path.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end Include_Path;

      ----------------------------------------------------------------------

      procedure No_Additional_Hw
      is
         Output : constant String := "obj/run_no_additional_hw.xml";
      begin
         Run (Config_File  => "data/config_no_additional_hw.xml",
              Output_File  => Output,
              Include_Path => "");

         Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/run_no_additional_hw.xml",
                     Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end No_Additional_Hw;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Output : constant String := "obj/run.xml";
      begin
         Run (Config_File  => "data/config_with_templateAmend.xml",
              Output_File  => Output,
              Include_Path => "data");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/output_templateAmend.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Positive_Test_With_Debugging
      is
         Output : constant String := "obj/output_config_with_templateAmend.xml";

         -- parse as XML-file and write out with same name and without comments
         procedure Remove_Comments (File : String)
         is
            Data : Muxml.XML_Data_Type;
            Output_File : Ada.Text_IO.File_Type;
         begin
            Muxml.Parse (Data => Data,
                         Kind => Muxml.None,
                         File => File);
            Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                                Name => File);
            DOM.Core.Nodes.Write
               (Stream         => Ada.Text_IO.Text_Streams.Stream (Output_File),
                N              => Data.Doc,
                Print_Comments => False,
                Pretty_Print   => True);
            Ada.Text_IO.Close (Output_File);
         end Remove_Comments;

      begin
         Run (Config_File  => "data/config_with_templateAmend.xml",
              Output_File  => Output,
              Include_Path => "data",
              Debug_Level  => VERBOSE_ERRORS);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/output_templateAmend.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);
         Ada.Directories.Delete_File (Name => Output);

         Run (Config_File  => "data/config_with_templateAmend.xml",
              Output_File  => Output,
              Include_Path => "data",
              Debug_Level  => VERBOSE_OUTPUT);
         Remove_Comments (File => Output);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/output_templateAmend.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end Positive_Test_With_Debugging;


   begin
      Mucfgcheck.Validation_Errors.Clear;
      Duplicate_Config_Value;
      Mucfgcheck.Validation_Errors.Clear;
      Duplicate_Config_Value_Due_To_Template;
      Mucfgcheck.Validation_Errors.Clear;

      Include_Path;
      No_Additional_Hw;
      Positive_Test;
      Positive_Test_With_Debugging;
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
end Merge.Test_Data.Tests;
