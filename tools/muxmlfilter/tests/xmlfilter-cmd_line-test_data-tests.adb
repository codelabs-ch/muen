--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Xmlfilter.Cmd_Line.Test_Data.

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
package body Xmlfilter.Cmd_Line.Test_Data.Tests is

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
      package ASU renames Ada.Strings.Unbounded;
      use type ASU.Unbounded_String;

      procedure Reset_Init_Vars
      is
      begin
         Input_Xml_Path     := ASU.Null_Unbounded_String;
         Input_Schema_Name  := ASU.Null_Unbounded_String;
         Output_Xml_Path    := ASU.Null_Unbounded_String;
         Output_Schema_Name := ASU.Null_Unbounded_String;
         Output_Schema_Path := ASU.Null_Unbounded_String;
      end Reset_Init_Vars;

      procedure Positive_Test1
      is
         Test_Parser : GNAT.Command_Line.Opt_Parser;
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-isn" & "Component_Ext"),
               2 => new String'("-osn" & "Component"),
               3 => new String'("some/input/file"),
               4 => new String'("some/output/file"));
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args'Unchecked_Access);
         Parser := Test_Parser;
         Init (Description => "Test run");

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;

         Assert (Condition => Input_Xml_Path = "some/input/file",
                 Message   => "Input XML path mismatch");
         Assert (Condition => Input_Schema_Name = "Component_Ext",
                 Message   => "Input schema name mismatch");
         Assert (Condition => Output_Xml_Path = "some/output/file",
                 Message   => "Output XML path mismatch");
         Assert (Condition => Output_Schema_Name = "Component",
                 Message   => "Output schema name mismatch");
         Assert (Condition => Output_Schema_Path = ASU.Null_Unbounded_String,
                 Message   => "Output schema path mismatch");
      end Positive_Test1;

      procedure Positive_Test2
      is
         Test_Parser : GNAT.Command_Line.Opt_Parser;
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-isn" & "Component_Ext"),
               2 => new String'("-osp" & "/path/to/out_scheme.xsd"),
               3 => new String'("some/input/file"),
               4 => new String'("some/output/file"));
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args'Unchecked_Access);
         Parser := Test_Parser;
         Init (Description => "Test run");

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;

         Assert (Condition => Input_Xml_Path = "some/input/file",
                 Message   => "Input XML path mismatch");
         Assert (Condition => Input_Schema_Name = "Component_Ext",
                 Message   => "Input schema name mismatch");
         Assert (Condition => Output_Xml_Path = "some/output/file",
                 Message   => "Output XML path mismatch");
         Assert (Condition => Output_Schema_Name = ASU.Null_Unbounded_String,
                 Message   => "Output schema name mismatch");
         Assert (Condition => Output_Schema_Path = "/path/to/out_scheme.xsd",
                 Message   => "Output schema path mismatch");
      end Positive_Test2;

      procedure Negative_Test_Missing_Output_Schema
      is
         Test_Parser : GNAT.Command_Line.Opt_Parser;
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-isn" & "Component_Ext"),
               2 => new String'("some/input/file"),
               3 => new String'("some/output/file"));
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
      end Negative_Test_Missing_Output_Schema;

      procedure Negative_Test_Missing_File
      is
         Test_Parser : GNAT.Command_Line.Opt_Parser;
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-isn" & "Component_Ext"),
               2 => new String'("-osp" & "/path/to/out_scheme.xsd"),
               3 => new String'("some/output/file"));
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
      end Negative_Test_Missing_File;

      procedure Negative_Test_Invalid_Switch
      is
         Test_Parser : GNAT.Command_Line.Opt_Parser;
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-isn" & "Component_Ext"),
               2 => new String'("-osp" & "/path/to/out_scheme.xsd"),
               3 => new String'("-osf" & "/path/to/out_scheme.xsd"),
               4 => new String'("some/input/file"),
               5 => new String'("some/output/file"));
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
      end Negative_Test_Invalid_Switch;
   begin
      Reset_Init_Vars;
      Positive_Test1;

      Reset_Init_Vars;
      Positive_Test2;

      Reset_Init_Vars;
      Negative_Test_Missing_Output_Schema;
      Negative_Test_Missing_File;
      Negative_Test_Invalid_Switch;
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Get_Input_Xml_Path (Gnattest_T : in out Test);
   procedure Test_Get_Input_Xml_Path_57980a (Gnattest_T : in out Test) renames Test_Get_Input_Xml_Path;
--  id:2.2/57980a87baac852a/Get_Input_Xml_Path/1/0/
   procedure Test_Get_Input_Xml_Path (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use Ada.Strings.Unbounded;

      Ref : constant String := "some/string.txt";
   begin
      Input_Xml_Path := To_Unbounded_String (Ref);
      Assert (Condition => Get_Input_Xml_Path = Ref,
              Message   => "Input XML path mismatch");
--  begin read only
   end Test_Get_Input_Xml_Path;
--  end read only


--  begin read only
   procedure Test_Get_Input_Schema_Name (Gnattest_T : in out Test);
   procedure Test_Get_Input_Schema_Name_101a88 (Gnattest_T : in out Test) renames Test_Get_Input_Schema_Name;
--  id:2.2/101a88d62a394693/Get_Input_Schema_Name/1/0/
   procedure Test_Get_Input_Schema_Name (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use Ada.Strings.Unbounded;

      Ref : constant String := "some/string.txt";
   begin
      Input_Schema_Name := To_Unbounded_String (Ref);
      Assert (Condition => Get_Input_Schema_Name = Ref,
              Message   => "Input schema name mismatch");

--  begin read only
   end Test_Get_Input_Schema_Name;
--  end read only


--  begin read only
   procedure Test_Get_Output_Xml_Path (Gnattest_T : in out Test);
   procedure Test_Get_Output_Xml_Path_af51ba (Gnattest_T : in out Test) renames Test_Get_Output_Xml_Path;
--  id:2.2/af51ba031c5e8b56/Get_Output_Xml_Path/1/0/
   procedure Test_Get_Output_Xml_Path (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use Ada.Strings.Unbounded;

      Ref : constant String := "some/string.txt";
   begin
      Output_Xml_Path := To_Unbounded_String (Ref);
      Assert (Condition => Get_Output_Xml_Path = Ref,
              Message   => "Output XML path mismatch");

--  begin read only
   end Test_Get_Output_Xml_Path;
--  end read only


--  begin read only
   procedure Test_Get_Output_Schema_Name (Gnattest_T : in out Test);
   procedure Test_Get_Output_Schema_Name_7cd613 (Gnattest_T : in out Test) renames Test_Get_Output_Schema_Name;
--  id:2.2/7cd6130d7521caa9/Get_Output_Schema_Name/1/0/
   procedure Test_Get_Output_Schema_Name (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use Ada.Strings.Unbounded;

      Ref : constant String := "some/string.txt";
   begin
      Output_Schema_Name := To_Unbounded_String (Ref);
      Assert (Condition => Get_Output_Schema_Name = Ref,
              Message   => "Output schema name mismatch");

--  begin read only
   end Test_Get_Output_Schema_Name;
--  end read only


--  begin read only
   procedure Test_Get_Output_Schema_Path (Gnattest_T : in out Test);
   procedure Test_Get_Output_Schema_Path_5e7b33 (Gnattest_T : in out Test) renames Test_Get_Output_Schema_Path;
--  id:2.2/5e7b338e0565c057/Get_Output_Schema_Path/1/0/
   procedure Test_Get_Output_Schema_Path (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use Ada.Strings.Unbounded;

      Ref : constant String := "some/string.txt";
   begin
      Output_Schema_Path := To_Unbounded_String (Ref);
      Assert (Condition => Get_Output_Schema_Path = Ref,
              Message   => "Output schema path mismatch");

--  begin read only
   end Test_Get_Output_Schema_Path;
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
end Xmlfilter.Cmd_Line.Test_Data.Tests;
