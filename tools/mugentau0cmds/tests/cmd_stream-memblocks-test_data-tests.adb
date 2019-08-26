--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cmd_Stream.Memblocks.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Directories;

with Muxml.Utils;

with Test_Utils;
--  begin read only
--  end read only
package body Cmd_Stream.Memblocks.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Create_Memory_Blocks (Gnattest_T : in out Test);
   procedure Test_Create_Memory_Blocks_0b4e17 (Gnattest_T : in out Test) renames Test_Create_Memory_Blocks;
--  id:2.2/0b4e17c39cdb7a2d/Create_Memory_Blocks/1/0/
   procedure Test_Create_Memory_Blocks (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Fn     : constant String := "create_memory_blocks.xml";
      Fn_Obj : constant String := "obj/" & Fn;
      Stream : Utils.Stream_Document_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Utils.Create (Stream_Doc => Stream,
                    Filename   => Fn_Obj);

      Create_Memory_Blocks
        (Policy     => Policy,
         Stream_Doc => Stream);
      Utils.Close (Stream_Doc => Stream);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/" & Fn,
               Filename2 => Fn_Obj),
              Message   => "Files differ");
      Ada.Directories.Delete_File (Name => Fn_Obj);
--  begin read only
   end Test_Create_Memory_Blocks;
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
end Cmd_Stream.Memblocks.Test_Data.Tests;
