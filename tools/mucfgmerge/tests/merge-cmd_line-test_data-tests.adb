--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Merge.Cmd_Line.Test_Data.

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
package body Merge.Cmd_Line.Test_Data.Tests is

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
   --  merge-cmd_line.ads:27:4:Init
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

      procedure No_Include_Path
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("policy/cfg/system_config.xml"),
               2 => new String'("policy-merged.xml"));
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

         Assert (Condition => Config_File = "policy/cfg/system_config.xml",
                 Message   => "Config file mismatch");
         Assert (Condition => Output_File = "policy-merged.xml",
                 Message   => "Output file  mismatch");
         Assert (Condition => Include_Path = "",
                 Message   => "Include path mismatch");
      end No_Include_Path;

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
           := (1 => new String'("-I/tmp/component"),
               2 => new String'("data/system_config.xml"),
               3 => new String'("merged.xml"));
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

         Assert (Condition => Config_File = "data/system_config.xml",
                 Message   => "Config file mismatch");
         Assert (Condition => Output_File = "merged.xml",
                 Message   => "Output file  mismatch");
         Assert (Condition => Include_Path = "/tmp/component",
                 Message   => "Include path mismatch");
      end Positive_Test;
   begin
      Invalid_Switch;
      Null_Argument;
      No_Include_Path;
      Positive_Test;
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Get_Config_File (Gnattest_T : in out Test);
   procedure Test_Get_Config_File_86576e (Gnattest_T : in out Test) renames Test_Get_Config_File;
--  id:2.2/86576e84aef15e99/Get_Config_File/1/0/
   procedure Test_Get_Config_File (Gnattest_T : in out Test) is
   --  merge-cmd_line.ads:30:4:Get_Config_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("system_config.xml");
   begin
      Config_File := Ref;
      Assert (Condition => Get_Config_File = Ref,
              Message   => "Config file mismatch");
--  begin read only
   end Test_Get_Config_File;
--  end read only


--  begin read only
   procedure Test_Get_Output_File (Gnattest_T : in out Test);
   procedure Test_Get_Output_File_762f34 (Gnattest_T : in out Test) renames Test_Get_Output_File;
--  id:2.2/762f34e807656cc2/Get_Output_File/1/0/
   procedure Test_Get_Output_File (Gnattest_T : in out Test) is
   --  merge-cmd_line.ads:33:4:Get_Output_File
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
   procedure Test_Get_Include_Path (Gnattest_T : in out Test);
   procedure Test_Get_Include_Path_a61f46 (Gnattest_T : in out Test) renames Test_Get_Include_Path;
--  id:2.2/a61f46d53b52eaa1/Get_Include_Path/1/0/
   procedure Test_Get_Include_Path (Gnattest_T : in out Test) is
   --  merge-cmd_line.ads:36:4:Get_Include_Path
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String
        := To_Unbounded_String ("/path/to/components");
   begin
      Include_Path := Ref;
      Assert (Condition => Get_Include_Path = Ref,
              Message   => "Include path mismatch");
--  begin read only
   end Test_Get_Include_Path;
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
end Merge.Cmd_Line.Test_Data.Tests;
