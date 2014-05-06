--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Cmd_Line.Infile_Outfile.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mutools.Cmd_Line.Infile_Outfile.Test_Data.Tests is


--  begin read only
   procedure Test_Init (Gnattest_T : in out Test);
   procedure Test_Init_a69a58 (Gnattest_T : in out Test) renames Test_Init;
--  id:2.2/a69a5871ab5eef40/Init/1/0/
   procedure Test_Init (Gnattest_T : in out Test) is
   --  mutools-cmd_line-infile_outfile.ads:27:4:Init
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Strings.Unbounded.Unbounded_String;

      Args        : aliased GNAT.OS_Lib.Argument_List
        := (1 => new String'("data/test_policy.xml"),
            2 => new String'("obj/test_policy.xml"));
      Test_Parser : GNAT.Command_Line.Opt_Parser;
   begin
      GNAT.Command_Line.Initialize_Option_Scan
        (Parser       => Test_Parser,
         Command_Line => Args'Unchecked_Access);

      Parser := Test_Parser;

      Init (Description => "Test run");

      for A in Args'Range loop
         GNAT.OS_Lib.Free (X => Args (A));
      end loop;

      Assert (Condition => File_In = "data/test_policy.xml",
              Message   => "Infile mismatch");
      Assert (Condition => File_Out = "obj/test_policy.xml",
              Message   => "Outfile mismatch");
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_1_Run (Gnattest_T : in out Test);
   procedure Test_Run_5aaca8 (Gnattest_T : in out Test) renames Test_1_Run;
--  id:2.2/5aaca89eb545984a/Run/1/0/
   procedure Test_1_Run (Gnattest_T : in out Test) is
   --  mutools-cmd_line-infile_outfile.ads:36:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_1_Run;
--  end read only


--  begin read only
   procedure Test_2_Run (Gnattest_T : in out Test);
   procedure Test_Run_6e38f6 (Gnattest_T : in out Test) renames Test_2_Run;
--  id:2.2/6e38f6fcf0b245bb/Run/0/0/
   procedure Test_2_Run (Gnattest_T : in out Test) is
   --  mutools-cmd_line-infile_outfile.ads:47:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_2_Run;
--  end read only


--  begin read only
   procedure Test_3_Run (Gnattest_T : in out Test);
   procedure Test_Run_37dd64 (Gnattest_T : in out Test) renames Test_3_Run;
--  id:2.2/37dd649102409a79/Run/0/0/
   procedure Test_3_Run (Gnattest_T : in out Test) is
   --  mutools-cmd_line-infile_outfile.ads:53:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_3_Run;
--  end read only

end Mutools.Cmd_Line.Infile_Outfile.Test_Data.Tests;
