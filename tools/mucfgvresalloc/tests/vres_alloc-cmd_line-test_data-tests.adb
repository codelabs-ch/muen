--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Vres_Alloc.Cmd_Line.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with GNAT.OS_Lib;

with Ada.Strings.Unbounded;

--  begin read only
--  end read only
package body Vres_Alloc.Cmd_Line.Test_Data.Tests is

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

      ----------------------------------------------------------------------

      procedure All_Arguments
      is
         use Ada.Strings.Unbounded;
         use type Ada.Strings.Unbounded.Unbounded_String;

         Args        : aliased GNAT.OS_Lib.Argument_List
            := (1 => new String'("specs/folder/input_policy.xml"),
                2 => new String'("output/output_policy.xml"));
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

         Assert (Condition => Policy = "specs/folder/input_policy.xml",
                 Message   => "Input file mismatch");
         Assert (Condition => Output_Filename = "output/output_policy.xml",
                 Message   => "Output file  mismatch");

         Policy   := To_Unbounded_String ("");
         Output_Filename := To_Unbounded_String ("");
      end All_Arguments;

      ----------------------------------------------------------------------

      procedure Missing_Argument
      is
         use Ada.Strings.Unbounded;

         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("specs/folder/input_policy.xml"));
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args'Unchecked_Access);

         Parser := Test_Parser;
         begin
            Init (Description => "Test run");
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when Invalid_Cmd_Line =>
               null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
         Policy := To_Unbounded_String ("");
         Output_Filename := To_Unbounded_String ("");
      end Missing_Argument;

   begin
      All_Arguments;
      Missing_Argument;

--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Get_Policy (Gnattest_T : in out Test);
   procedure Test_Get_Policy_aac0d6 (Gnattest_T : in out Test) renames Test_Get_Policy;
--  id:2.2/aac0d695aae58756/Get_Policy/1/0/
   procedure Test_Get_Policy (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("some_filename.xml");
   begin
      Policy := Ref;
      Assert (Condition => Get_Policy = Ref,
              Message   => "Input file mismatch");
--  begin read only
   end Test_Get_Policy;
--  end read only


--  begin read only
   procedure Test_Get_Output_Filename (Gnattest_T : in out Test);
   procedure Test_Get_Output_Filename_611572 (Gnattest_T : in out Test) renames Test_Get_Output_Filename;
--  id:2.2/611572b0a7327662/Get_Output_Filename/1/0/
   procedure Test_Get_Output_Filename (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("some_filename.xml");
   begin
      Output_Filename := Ref;
      Assert (Condition => Get_Output_Filename = Ref,
              Message   => "Output file mismatch");


--  begin read only
   end Test_Get_Output_Filename;
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
end Vres_Alloc.Cmd_Line.Test_Data.Tests;
