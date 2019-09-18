--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Compjoin.Cmd_Line.Test_Data.

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
package body Compjoin.Cmd_Line.Test_Data.Tests is

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

      procedure Reset
      is
      begin
         Input_File     := Ada.Strings.Unbounded.Null_Unbounded_String;
         Output_File    := Ada.Strings.Unbounded.Null_Unbounded_String;
         Component_List := Ada.Strings.Unbounded.Null_Unbounded_String;
      end Reset;

      ----------------------------------------------------------------------

      procedure Invalid_Parameter
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-i"),
               2 => new String'("policy_src.xml"),
               3 => new String'("-o"),
               4 => new String'("policy_src_joined.xml"),
               5 => new String'("-c"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         Reset;
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

      procedure Invalid_Switch
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-Qx"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         Reset;
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

      procedure No_Component_List
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-i"),
               2 => new String'("policy_src.xml"),
               3 => new String'("-o"),
               4 => new String'("obj/policy_src_joined.xml"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         Reset;
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
      end No_Component_List;

      ----------------------------------------------------------------------

      procedure No_Input_File
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-o"),
               2 => new String'("policy_src_joined.xml"),
               3 => new String'("-c"),
               4 => new String'("obj/c1.xml,obj/c2.xml"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         Reset;
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
      end No_Input_File;

      ----------------------------------------------------------------------

      procedure No_Output_File
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-i"),
               2 => new String'("policy_src.xml"),
               3 => new String'("-c"),
               4 => new String'("obj/c1.xml,obj/c2.xml"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         Reset;
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
      end No_Output_File;

      ----------------------------------------------------------------------

      procedure Null_Argument
      is
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         Reset;
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => GNAT.OS_Lib.Argument_String_To_List
              (Arg_String => ""));

         Parser := Test_Parser;

         begin
            Init (Description => "Test run");
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when Invalid_Cmd_Line => null;
         end;
      end Null_Argument;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-i"),
               2 => new String'("policy_src.xml"),
               3 => new String'("-o"),
               4 => new String'("policy_src_joined.xml"),
               5 => new String'("-c"),
               6 => new String'("obj/c1.xml,obj/c2.xml"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         Reset;
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args'Unchecked_Access);

         Parser := Test_Parser;

         Init (Description => "Test run");

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;

         Assert (Condition => Input_File = "policy_src.xml",
                 Message   => "Input file mismatch");
         Assert (Condition => Output_File = "policy_src_joined.xml",
                 Message   => "Output file mismatch");
         Assert (Condition => Component_List = "obj/c1.xml,obj/c2.xml",
                 Message   => "Component list mismatch");
      end Positive_Test;
   begin
      Invalid_Parameter;
      Invalid_Switch;
      Null_Argument;
      No_Input_File;
      No_Output_File;
      No_Component_List;
      Positive_Test;
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Get_Input_File (Gnattest_T : in out Test);
   procedure Test_Get_Input_File_01e3dc (Gnattest_T : in out Test) renames Test_Get_Input_File;
--  id:2.2/01e3dc0b95283e09/Get_Input_File/1/0/
   procedure Test_Get_Input_File (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("input.xml");
   begin
      Input_File := Ref;
      Assert (Condition => Get_Input_File = Ref,
              Message   => "Input file mismatch");
--  begin read only
   end Test_Get_Input_File;
--  end read only


--  begin read only
   procedure Test_Get_Output_File (Gnattest_T : in out Test);
   procedure Test_Get_Output_File_762f34 (Gnattest_T : in out Test) renames Test_Get_Output_File;
--  id:2.2/762f34e807656cc2/Get_Output_File/1/0/
   procedure Test_Get_Output_File (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("output.xml");
   begin
      Output_File := Ref;
      Assert (Condition => Get_Output_File = Ref,
              Message   => "Output file mismatch");
--  begin read only
   end Test_Get_Output_File;
--  end read only


--  begin read only
   procedure Test_Get_Component_List (Gnattest_T : in out Test);
   procedure Test_Get_Component_List_78c74e (Gnattest_T : in out Test) renames Test_Get_Component_List;
--  id:2.2/78c74e89cdecbfa8/Get_Component_List/1/0/
   procedure Test_Get_Component_List (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("c1.xml,c2.xml,c3.xml");
   begin
      Component_List := Ref;
      Assert (Condition => Get_Component_List = Ref,
              Message   => "Component list mismatch");
--  begin read only
   end Test_Get_Component_List;
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
end Compjoin.Cmd_Line.Test_Data.Tests;
