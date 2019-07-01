--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cmd_Stream.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Directories;

with GNAT.OS_Lib;

with Test_Utils;
--  begin read only
--  end read only
package body Cmd_Stream.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Reverse_Commands (Gnattest_T : in out Test);
   procedure Test_Reverse_Commands_0f416b (Gnattest_T : in out Test) renames Test_Reverse_Commands;
--  id:2.2/0f416b8b19a9b0d8/Reverse_Commands/1/0/
   procedure Test_Reverse_Commands (Gnattest_T : in out Test) is
   --  cmd_stream-utils.ads:42:4:Reverse_Commands
--  end read only

      pragma Unreferenced (Gnattest_T);

      Buffer : Command_Buffer_Type;
   begin
      Append_Command (Buffer => Buffer,
                      Name   => "first");
      Append_Command (Buffer => Buffer,
                      Name   => "second");
      Assert (Condition => Buffer.Cmds.First_Element = "  <first/>",
              Message   => "First mismatch (1)");
      Assert (Condition => Buffer.Cmds.Last_Element = "  <second/>",
              Message   => "Last mismatch (1)");

      Reverse_Commands (Buffer => Buffer);
      Assert (Condition => Buffer.Cmds.First_Element = "  <second/>",
              Message   => "First mismatch (2)");
      Assert (Condition => Buffer.Cmds.Last_Element = "  <first/>",
              Message   => "Last mismatch (2)");
--  begin read only
   end Test_Reverse_Commands;
--  end read only


--  begin read only
   procedure Test_2_Append_Command (Gnattest_T : in out Test);
   procedure Test_Append_Command_287f11 (Gnattest_T : in out Test) renames Test_2_Append_Command;
--  id:2.2/287f117437a4f034/Append_Command/0/0/
   procedure Test_2_Append_Command (Gnattest_T : in out Test) is
   --  cmd_stream-utils.ads:46:4:Append_Command
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_2_Append_Command;
--  end read only


--  begin read only
   procedure Test_Create (Gnattest_T : in out Test);
   procedure Test_Create_930d48 (Gnattest_T : in out Test) renames Test_Create;
--  id:2.2/930d482a1a8a8261/Create/1/0/
   procedure Test_Create (Gnattest_T : in out Test) is
   --  cmd_stream-utils.ads:55:4:Create
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive
      is
         Stream : Stream_Document_Type;
         Fn     : constant String := "create_stream.xml";
         Fn_Obj : constant String := "obj/" & Fn;
      begin
         Create (Stream_Doc => Stream,
                 Filename   => Fn_Obj);
         Close (Stream_Doc => Stream);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/" & Fn,
                  Filename2 => Fn_Obj),
                 Message   => "Files differ");
         Ada.Directories.Delete_File (Name => Fn_Obj);
      end Positive;

      ----------------------------------------------------------------------

      procedure No_Access
      is
         Stream : Stream_Document_Type;
         Fn     : constant String := "/stream_no_access.xml";
      begin
         Assert (Condition => not GNAT.OS_Lib.Is_Owner_Writable_File
                 (Name => Fn),
                 Message   => "Unexpected access");

         begin
            Create (Stream_Doc => Stream,
                    Filename   => Fn);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when IO_Error => null;
         end;
      end No_Access;
   begin
      Positive;
      No_Access;
--  begin read only
   end Test_Create;
--  end read only


--  begin read only
   procedure Test_1_Append_Command (Gnattest_T : in out Test);
   procedure Test_Append_Command_093943 (Gnattest_T : in out Test) renames Test_1_Append_Command;
--  id:2.2/093943b5361f4e05/Append_Command/1/0/
   procedure Test_1_Append_Command (Gnattest_T : in out Test) is
   --  cmd_stream-utils.ads:61:4:Append_Command
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_1_Append_Command;
--  end read only


--  begin read only
   procedure Test_Append_Commands (Gnattest_T : in out Test);
   procedure Test_Append_Commands_e32107 (Gnattest_T : in out Test) renames Test_Append_Commands;
--  id:2.2/e32107367a60779d/Append_Commands/1/0/
   procedure Test_Append_Commands (Gnattest_T : in out Test) is
   --  cmd_stream-utils.ads:68:4:Append_Commands
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Append_Commands;
--  end read only


--  begin read only
   procedure Test_Clear_Region (Gnattest_T : in out Test);
   procedure Test_Clear_Region_8a06f5 (Gnattest_T : in out Test) renames Test_Clear_Region;
--  id:2.2/8a06f510ad0687ce/Clear_Region/1/0/
   procedure Test_Clear_Region (Gnattest_T : in out Test) is
   --  cmd_stream-utils.ads:74:4:Clear_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Clear_Region;
--  end read only


--  begin read only
   procedure Test_Close (Gnattest_T : in out Test);
   procedure Test_Close_fb1398 (Gnattest_T : in out Test) renames Test_Close;
--  id:2.2/fb13981e576aa0cb/Close/1/0/
   procedure Test_Close (Gnattest_T : in out Test) is
   --  cmd_stream-utils.ads:80:4:Close
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Close;
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
end Cmd_Stream.Utils.Test_Data.Tests;
