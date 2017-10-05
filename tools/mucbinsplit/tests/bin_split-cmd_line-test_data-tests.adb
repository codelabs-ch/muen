--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bin_Split.Cmd_Line.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Bin_Split.Cmd_Line.Test_Data.Tests is


--  begin read only
   procedure Test_Init (Gnattest_T : in out Test);
   procedure Test_Init_a69a58 (Gnattest_T : in out Test) renames Test_Init;
--  id:2.2/a69a5871ab5eef40/Init/1/0/
   procedure Test_Init (Gnattest_T : in out Test) is
   --  bin_split-cmd_line.ads:28:4:Init
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
           := (1 => new String'("file"));
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
           := (1 => new String'("test_data/test_cspec.xml"),
               2 => new String'("test_data/test_binary"),
               3 => new String'("test_data/test_output"));
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

         Assert (Condition => Spec = "test_data/test_cspec.xml",
                 Message   => "CSpec mismatch");
         Assert (Condition => Binary = "test_data/test_binary",
                 Message   => "Input binary mismatch");
         Assert (Condition => Output_Spec = "test_data/test_output",
                 Message   => "Output CSpec mismatch");
      end;
   begin
      Invalid_Switch;
      Null_Argument;
      Positive_Test;

--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Get_Spec (Gnattest_T : in out Test);
   procedure Test_Get_Spec_03d696 (Gnattest_T : in out Test) renames Test_Get_Spec;
--  id:2.2/03d6960e256007fe/Get_Spec/1/0/
   procedure Test_Get_Spec (Gnattest_T : in out Test) is
   --  bin_split-cmd_line.ads:31:4:Get_Spec
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("spec.xml");
   begin
      Spec := Ref;
      Assert (Condition => Get_Spec = Ref,
              Message   => "Spec mismatch");

--  begin read only
   end Test_Get_Spec;
--  end read only


--  begin read only
   procedure Test_Get_Binary (Gnattest_T : in out Test);
   procedure Test_Get_Binary_3b9c4e (Gnattest_T : in out Test) renames Test_Get_Binary;
--  id:2.2/3b9c4e52a1e6a791/Get_Binary/1/0/
   procedure Test_Get_Binary (Gnattest_T : in out Test) is
   --  bin_split-cmd_line.ads:34:4:Get_Binary
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("my_precious");
   begin
      Binary := Ref;
      Assert (Condition => Get_Binary = Ref,
              Message   => "Input binary mismatch");

--  begin read only
   end Test_Get_Binary;
--  end read only


--  begin read only
   procedure Test_Get_Output_Spec (Gnattest_T : in out Test);
   procedure Test_Get_Output_Spec_faaf96 (Gnattest_T : in out Test) renames Test_Get_Output_Spec;
--  id:2.2/faaf964f54641d5e/Get_Output_Spec/1/0/
   procedure Test_Get_Output_Spec (Gnattest_T : in out Test) is
   --  bin_split-cmd_line.ads:37:4:Get_Output_Spec
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("somewhere/output.xml");
   begin
      Output_Spec := Ref;
      Assert (Condition => Get_Output_Spec = Ref,
              Message   => "Output spec mismatch");

--  begin read only
   end Test_Get_Output_Spec;
--  end read only


--  begin read only
   procedure Test_Get_Output_Dir (Gnattest_T : in out Test);
   procedure Test_Get_Output_Dir_c3e9c8 (Gnattest_T : in out Test) renames Test_Get_Output_Dir;
--  id:2.2/c3e9c87208c742d1/Get_Output_Dir/1/0/
   procedure Test_Get_Output_Dir (Gnattest_T : in out Test) is
   --  bin_split-cmd_line.ads:40:4:Get_Output_Dir
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("somewhere");
   begin

      Output_Dir := Ref;
      Assert (Condition => Get_Output_Dir = Ref,
              Message   => "Output_Dir mismatch");

--  begin read only
   end Test_Get_Output_Dir;
--  end read only

end Bin_Split.Cmd_Line.Test_Data.Tests;
