--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Stackcheck.Cmd_Line.Test_Data.

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
package body Stackcheck.Cmd_Line.Test_Data.Tests is

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
   --  stackcheck-cmd_line.ads:27:4:Init
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
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-P"));
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

      procedure No_Limit
      is
         Args : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-i"),
               2 => new String'("obj/debug"));
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
      end No_Limit;

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
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-P"),
               2 => new String'("data/sm.gpr"),
               3 => new String'("-l"),
               4 => new String'("4096"));

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

         Assert (Condition => GPR_File = "data/sm.gpr",
                 Message   => "Project file mismatch");
         Assert (Condition => Stack_Limit = 4096,
                 Message   => "Stack limit mismatch");
      end Positive_Test;
   begin
      Invalid_Switch;
      Invalid_Parameter;
      Null_Argument;
      No_Limit;
      Positive_Test;
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Get_GPR_File (Gnattest_T : in out Test);
   procedure Test_Get_GPR_File_d99656 (Gnattest_T : in out Test) renames Test_Get_GPR_File;
--  id:2.2/d996562a3a51c290/Get_GPR_File/1/0/
   procedure Test_Get_GPR_File (Gnattest_T : in out Test) is
   --  stackcheck-cmd_line.ads:30:4:Get_GPR_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("project.gpr");
   begin
      GPR_File := Ref;
      Assert (Condition => Get_GPR_File = Ref,
              Message   => "Project file mismatch");
--  begin read only
   end Test_Get_GPR_File;
--  end read only


--  begin read only
   procedure Test_Get_Stack_Limit (Gnattest_T : in out Test);
   procedure Test_Get_Stack_Limit_b164ad (Gnattest_T : in out Test) renames Test_Get_Stack_Limit;
--  id:2.2/b164adda84d281df/Get_Stack_Limit/1/0/
   procedure Test_Get_Stack_Limit (Gnattest_T : in out Test) is
   --  stackcheck-cmd_line.ads:33:4:Get_Stack_Limit
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Stack_Limit := 4096;
      Assert (Condition => Get_Stack_Limit = 4096,
              Message   => "Stack limit mismatch");
--  begin read only
   end Test_Get_Stack_Limit;
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
end Stackcheck.Cmd_Line.Test_Data.Tests;
