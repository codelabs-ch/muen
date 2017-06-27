--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack.Cmd_Line.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Pack.Cmd_Line.Test_Data.Tests is


--  begin read only
   procedure Test_Init (Gnattest_T : in out Test);
   procedure Test_Init_a69a58 (Gnattest_T : in out Test) renames Test_Init;
--  id:2.2/a69a5871ab5eef40/Init/1/0/
   procedure Test_Init (Gnattest_T : in out Test) is
   --  pack-cmd_line.ads:27:4:Init
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Strings.Unbounded.Unbounded_String;

      ----------------------------------------------------------------------

      procedure Invalid_Switch
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-x"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args'Unchecked_Access);

         Parser := Test_Parser;

         begin
            Init (Description => "Test run");
            for A in Args'Range loop
               GNAT.OS_Lib.Free (X => Args (A));
            end loop;
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when Invalid_Cmd_Line =>
               null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Invalid_Switch;

      ----------------------------------------------------------------------

      procedure Null_Argument
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-o"),
               2 => new String'("obj"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args'Unchecked_Access);

         Parser := Test_Parser;

         begin
            Init (Description => "Test run");
            for A in Args'Range loop
               GNAT.OS_Lib.Free (X => Args (A));
            end loop;
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when Invalid_Cmd_Line =>
               null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Null_Argument;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-o"),
               2 => new String'("objdir"),
               3 => new String'("-i"),
               4 => new String'("indir"),
               5 => new String'("--dry-run"),
               6 => new String'("data/test_policy.xml"));
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

         Assert (Condition => Output_Dir = "objdir",
                 Message   => "Outdir mismatch");
         Assert (Condition => Input_Dir = "indir",
                 Message   => "Indir mismatch");
         Assert (Condition => Policy = "data/test_policy.xml",
                 Message   => "Policy mismatch");
         Assert (Condition => Dry_Run,
                 Message   => "Not a dry run");
      end Positive_Test;
   begin
      Invalid_Switch;
      Null_Argument;
      Positive_Test;
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Get_Policy (Gnattest_T : in out Test);
   procedure Test_Get_Policy_aac0d6 (Gnattest_T : in out Test) renames Test_Get_Policy;
--  id:2.2/aac0d695aae58756/Get_Policy/1/0/
   procedure Test_Get_Policy (Gnattest_T : in out Test) is
   --  pack-cmd_line.ads:30:4:Get_Policy
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("testpolicy.xml");
   begin
      Policy := Ref;
      Assert (Condition => Get_Policy = Ref,
              Message   => "Policy mismatch");
--  begin read only
   end Test_Get_Policy;
--  end read only


--  begin read only
   procedure Test_Get_Output_Dir (Gnattest_T : in out Test);
   procedure Test_Get_Output_Dir_c3e9c8 (Gnattest_T : in out Test) renames Test_Get_Output_Dir;
--  id:2.2/c3e9c87208c742d1/Get_Output_Dir/1/0/
   procedure Test_Get_Output_Dir (Gnattest_T : in out Test) is
   --  pack-cmd_line.ads:33:4:Get_Output_Dir
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("output");
   begin
      Output_Dir := Ref;
      Assert (Condition => Get_Output_Dir = Ref,
              Message   => "Outdir mismatch");
--  begin read only
   end Test_Get_Output_Dir;
--  end read only


--  begin read only
   procedure Test_Get_Output_Imgname (Gnattest_T : in out Test);
   procedure Test_Get_Output_Imgname_1df5ca (Gnattest_T : in out Test) renames Test_Get_Output_Imgname;
--  id:2.2/1df5cafd876b765b/Get_Output_Imgname/1/0/
   procedure Test_Get_Output_Imgname (Gnattest_T : in out Test) is
   --  pack-cmd_line.ads:36:4:Get_Output_Imgname
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("img_name");
   begin
      Output_Imgname := Ref;
      Assert (Condition => Get_Output_Imgname = Ref,
              Message   => "Image name mismatch");
--  begin read only
   end Test_Get_Output_Imgname;
--  end read only


--  begin read only
   procedure Test_Get_Input_Dir (Gnattest_T : in out Test);
   procedure Test_Get_Input_Dir_da1404 (Gnattest_T : in out Test) renames Test_Get_Input_Dir;
--  id:2.2/da14045cb4f1843e/Get_Input_Dir/1/0/
   procedure Test_Get_Input_Dir (Gnattest_T : in out Test) is
   --  pack-cmd_line.ads:39:4:Get_Input_Dir
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("indir");
   begin
      Input_Dir := Ref;
      Assert (Condition => Get_Input_Dir = Ref,
              Message   => "Indir mismatch");
--  begin read only
   end Test_Get_Input_Dir;
--  end read only

end Pack.Cmd_Line.Test_Data.Tests;
