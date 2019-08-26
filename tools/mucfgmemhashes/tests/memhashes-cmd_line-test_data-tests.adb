--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Memhashes.Cmd_Line.Test_Data.

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
package body Memhashes.Cmd_Line.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Init (Gnattest_T : in out Test);
   procedure Test_Init_a69a58 (Gnattest_T : in out Test) renames Test_Init;
--  id:2.2/a69a5871ab5eef40/Init/1/0/
   procedure Test_Init (Gnattest_T : in out Test) is
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
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Invalid_Switch;

      ----------------------------------------------------------------------

      procedure Invalid_Parameter
      is
         Args : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("in"),
               2 => new String'("out"),
               3 => new String'("-i"));
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
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Invalid_Parameter;

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
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Null_Argument;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Args : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-i"),
               2 => new String'("some_dir"),
               3 => new String'("in_policy"),
               4 => new String'("out_policy"));
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

         Assert (Condition => Policy_In = "in_policy",
                 Message   => "Policy input mismatch");
         Assert (Condition => Policy_Out = "out_policy",
                 Message   => "Policy output mismatch");
         Assert (Condition => Input_Dir = "some_dir",
                 Message   => "Input directory mismatch");
      end Positive_Test;
   begin
      Invalid_Switch;
      Invalid_Parameter;
      Null_Argument;
      Positive_Test;
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Get_Policy_Input (Gnattest_T : in out Test);
   procedure Test_Get_Policy_Input_4a0d5a (Gnattest_T : in out Test) renames Test_Get_Policy_Input;
--  id:2.2/4a0d5a4e86e5bc6b/Get_Policy_Input/1/0/
   procedure Test_Get_Policy_Input (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("input.xml");
   begin
      Policy_In := Ref;
      Assert (Condition => Get_Policy_Input = Ref,
              Message   => "Input policy mismatch");
--  begin read only
   end Test_Get_Policy_Input;
--  end read only


--  begin read only
   procedure Test_Get_Policy_Output (Gnattest_T : in out Test);
   procedure Test_Get_Policy_Output_39909f (Gnattest_T : in out Test) renames Test_Get_Policy_Output;
--  id:2.2/39909f751c4aca3b/Get_Policy_Output/1/0/
   procedure Test_Get_Policy_Output (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("output.xml");
   begin
      Policy_Out := Ref;
      Assert (Condition => Get_Policy_Output = Ref,
              Message   => "Output policy mismatch");
--  begin read only
   end Test_Get_Policy_Output;
--  end read only


--  begin read only
   procedure Test_Get_Input_Dir (Gnattest_T : in out Test);
   procedure Test_Get_Input_Dir_da1404 (Gnattest_T : in out Test) renames Test_Get_Input_Dir;
--  id:2.2/da14045cb4f1843e/Get_Input_Dir/1/0/
   procedure Test_Get_Input_Dir (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("input_dir");
   begin
      Input_Dir := Ref;
      Assert (Condition => Get_Input_Dir = Ref,
              Message   => "Input dir mismatch");
--  begin read only
   end Test_Get_Input_Dir;
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
end Memhashes.Cmd_Line.Test_Data.Tests;
