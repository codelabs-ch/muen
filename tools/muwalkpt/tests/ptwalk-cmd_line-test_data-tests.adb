--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ptwalk.Cmd_Line.Test_Data.

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
package body Ptwalk.Cmd_Line.Test_Data.Tests is

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
                    Message   => "Exception expected (1)");

         exception
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Invalid_Switch;

      ----------------------------------------------------------------------

      procedure No_PT_File
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-p"),
               2 => new String'("16#1ef000#"),
               3 => new String'("-t"),
               4 => new String'("EPT"),
               5 => new String'("16#beefa0f#"));
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
                    Message   => "Exception expected (2)");

         exception
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end No_PT_File;

      ----------------------------------------------------------------------

      procedure No_PT_Pointer
      is
         Args : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-t"),
               2 => new String'("EPT"),
               3 => new String'("16#beefa0f#"),
               4 => new String'("data/gfx_linux_pt"));
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
                    Message   => "Exception expected (3)");

         exception
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end No_PT_Pointer;

      ----------------------------------------------------------------------

      procedure No_PT_Type
      is
         Args : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-p"),
               2 => new String'("16#1ef000#"),
               3 => new String'("16#beefa0f#"),
               4 => new String'("data/gfx_linux_pt"));
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
                    Message   => "Exception expected (4)");

         exception
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end No_PT_Type;

      ----------------------------------------------------------------------

      procedure No_Virtual_Address
      is
         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-p"),
               2 => new String'("16#1ef000#"),
               3 => new String'("-t"),
               4 => new String'("EPT"),
               5 => new String'("data/gfx_linux_pt"));
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
                    Message   => "Exception expected (5)");

         exception
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end No_Virtual_Address;

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
                    Message   => "Exception expected (6)");

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
         use type Interfaces.Unsigned_64;
         use type Paging.Paging_Mode_Type;

         Args        : aliased GNAT.OS_Lib.Argument_List
           := (1 => new String'("-p"),
               2 => new String'("16#1ef000#"),
               3 => new String'("-t"),
               4 => new String'("EPT"),
               5 => new String'("16#beefa0f#"),
               6 => new String'("data/gfx_linux_pt"));
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

         Assert (Condition => PT_File = "data/gfx_linux_pt",
                 Message   => "PT file mismatch");
         Assert (Condition => PT_Type = Paging.EPT_Mode,
                 Message   => "PT type mismatch");
         Assert (Condition => PT_Pointer = 16#1ef000#,
                 Message   => "PT pointer mismatch");
         Assert (Condition => Virtual_Addr = 16#beefa0f#,
                 Message   => "Virtual address mismatch");
      end;
   begin
      Invalid_Switch;
      Null_Argument;
      No_PT_File;
      No_PT_Pointer;
      No_PT_Type;
      No_Virtual_Address;
      Positive_Test;
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Get_PT_File (Gnattest_T : in out Test);
   procedure Test_Get_PT_File_cc58d5 (Gnattest_T : in out Test) renames Test_Get_PT_File;
--  id:2.2/cc58d55863766133/Get_PT_File/1/0/
   procedure Test_Get_PT_File (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : constant Unbounded_String := To_Unbounded_String ("nic_lnx_pt");
   begin
      PT_File := Ref;
      Assert (Condition => Get_PT_File = Ref,
              Message   => "PT file mismatch");
--  begin read only
   end Test_Get_PT_File;
--  end read only


--  begin read only
   procedure Test_Get_PT_Type (Gnattest_T : in out Test);
   procedure Test_Get_PT_Type_83606b (Gnattest_T : in out Test) renames Test_Get_PT_Type;
--  id:2.2/83606b3462ba8697/Get_PT_Type/1/0/
   procedure Test_Get_PT_Type (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Paging.Paging_Mode_Type;

      Ref : constant Paging.Paging_Mode_Type := Paging.EPT_Mode;
   begin
      PT_Type := Ref;
      Assert (Condition => Get_PT_Type = Ref,
              Message   => "PT type mismatch");
--  begin read only
   end Test_Get_PT_Type;
--  end read only


--  begin read only
   procedure Test_Get_PT_Pointer (Gnattest_T : in out Test);
   procedure Test_Get_PT_Pointer_1f0725 (Gnattest_T : in out Test) renames Test_Get_PT_Pointer;
--  id:2.2/1f072507c5f8d014/Get_PT_Pointer/1/0/
   procedure Test_Get_PT_Pointer (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Ref : constant Interfaces.Unsigned_64 := 16#cafe_beef#;
   begin
      PT_Pointer := Ref;
      Assert (Condition => Get_PT_Pointer = Ref,
              Message   => "PT pointer mismatch");
--  begin read only
   end Test_Get_PT_Pointer;
--  end read only


--  begin read only
   procedure Test_Get_Virtual_Address (Gnattest_T : in out Test);
   procedure Test_Get_Virtual_Address_73d105 (Gnattest_T : in out Test) renames Test_Get_Virtual_Address;
--  id:2.2/73d1052dbac5c7aa/Get_Virtual_Address/1/0/
   procedure Test_Get_Virtual_Address (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Ref : constant Interfaces.Unsigned_64 := 16#feed_beef#;
   begin
      Virtual_Addr := Ref;
      Assert (Condition => Get_Virtual_Address = Ref,
              Message   => "Virtual address mismatch");
--  begin read only
   end Test_Get_Virtual_Address;
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
end Ptwalk.Cmd_Line.Test_Data.Tests;
