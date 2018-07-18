--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cspec.Cmd_Line.Test_Data.

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
package body Cspec.Cmd_Line.Test_Data.Tests is

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
   --  cspec-cmd_line.ads:27:4:Init
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
           := (1 => new String'("-p"),
               2 => new String'("system.xml"),
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
               2 => new String'("my_cspec"),
               3 => new String'("-I"),
               4 => new String'("incdir"),
               5 => new String'("-o"),
               6 => new String'("obj/out.xml"),
               7 => new String'("obj/gen"));
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

         Assert (Condition => Output_Dir = "obj/gen",
                 Message   => "Output dir mismatch");
         Assert (Condition => Output_Spec = "obj/out.xml",
                 Message   => "Output spec mismatch");
         Assert (Condition => Input_Spec = "my_cspec",
                 Message   => "Cspec input mismatch");
         Assert (Condition => Include_Path = "incdir",
                 Message   => "Include dir mismatch");
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
   procedure Test_Get_Input_Spec (Gnattest_T : in out Test);
   procedure Test_Get_Input_Spec_b3e784 (Gnattest_T : in out Test) renames Test_Get_Input_Spec;
--  id:2.2/b3e784f1773d203f/Get_Input_Spec/1/0/
   procedure Test_Get_Input_Spec (Gnattest_T : in out Test) is
   --  cspec-cmd_line.ads:30:4:Get_Input_Spec
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("cspec.xml");
   begin
      Input_Spec := Ref;
      Assert (Condition => Get_Input_Spec = Ref,
              Message   => "Input cspec mismatch");
--  begin read only
   end Test_Get_Input_Spec;
--  end read only


--  begin read only
   procedure Test_Get_Output_Dir (Gnattest_T : in out Test);
   procedure Test_Get_Output_Dir_c3e9c8 (Gnattest_T : in out Test) renames Test_Get_Output_Dir;
--  id:2.2/c3e9c87208c742d1/Get_Output_Dir/1/0/
   procedure Test_Get_Output_Dir (Gnattest_T : in out Test) is
   --  cspec-cmd_line.ads:33:4:Get_Output_Dir
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("output.xml");
   begin
      Output_Dir := Ref;
      Assert (Condition => Get_Output_Dir = Ref,
              Message   => "Output dir mismatch");
--  begin read only
   end Test_Get_Output_Dir;
--  end read only


--  begin read only
   procedure Test_Get_Output_Spec (Gnattest_T : in out Test);
   procedure Test_Get_Output_Spec_faaf96 (Gnattest_T : in out Test) renames Test_Get_Output_Spec;
--  id:2.2/faaf964f54641d5e/Get_Output_Spec/1/0/
   procedure Test_Get_Output_Spec (Gnattest_T : in out Test) is
   --  cspec-cmd_line.ads:36:4:Get_Output_Spec
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("output.xml");
   begin
      Output_Spec := Ref;
      Assert (Condition => Get_Output_Spec = Ref,
              Message   => "Output spec mismatch");
--  begin read only
   end Test_Get_Output_Spec;
--  end read only


--  begin read only
   procedure Test_Get_Include_Path (Gnattest_T : in out Test);
   procedure Test_Get_Include_Path_a61f46 (Gnattest_T : in out Test) renames Test_Get_Include_Path;
--  id:2.2/a61f46d53b52eaa1/Get_Include_Path/1/0/
   procedure Test_Get_Include_Path (Gnattest_T : in out Test) is
   --  cspec-cmd_line.ads:39:4:Get_Include_Path
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("/path");
   begin
      Include_Path := Ref;
      Assert (Condition => Get_Include_Path = Ref,
              Message   => "Include path mismatch");
--  begin read only
   end Test_Get_Include_Path;
--  end read only


--  begin read only
   procedure Test_Get_Package_Name (Gnattest_T : in out Test);
   procedure Test_Get_Package_Name_a709c7 (Gnattest_T : in out Test) renames Test_Get_Package_Name;
--  id:2.2/a709c76161253f85/Get_Package_Name/1/0/
   procedure Test_Get_Package_Name (Gnattest_T : in out Test) is
   --  cspec-cmd_line.ads:43:4:Get_Package_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("comp_name");
   begin
      Package_Name := Ref;
      Assert (Condition => Get_Package_Name = Ref,
              Message   => "Package name mismatch");
--  begin read only
   end Test_Get_Package_Name;
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
end Cspec.Cmd_Line.Test_Data.Tests;
