--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cmd_Stream.Roots.Memory.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Directories;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Test_Utils;
--  begin read only
--  end read only
package body Cmd_Stream.Roots.Memory.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Create_Memory_Regions (Gnattest_T : in out Test);
   procedure Test_Create_Memory_Regions_1f09d8 (Gnattest_T : in out Test) renames Test_Create_Memory_Regions;
--  id:2.2/1f09d8c1f760ab5e/Create_Memory_Regions/1/0/
   procedure Test_Create_Memory_Regions (Gnattest_T : in out Test) is
   --  cmd_stream-roots-memory.ads:28:4:Create_Memory_Regions
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive
      is
         Policy : Muxml.XML_Data_Type;
         Fn     : constant String := "create_memory_regions.xml";
         Fn_Obj : constant String := "obj/" & Fn;
         Stream : Utils.Stream_Document_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Utils.Create (Stream_Doc => Stream,
                       Filename   => Fn_Obj);

         Create_Memory_Regions
           (Stream_Doc  => Stream,
            Phys_Memory => McKae.XML.XPath.XIA.XPath_Query
              (N     => Policy.Doc,
               XPath => "/system/memory/memory"));
         Utils.Close (Stream_Doc => Stream);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/" & Fn,
                  Filename2 => Fn_Obj),
                 Message   => "Files differ");
         Ada.Directories.Delete_File (Name => Fn_Obj);
      end;

      ----------------------------------------------------------------------

      procedure No_Size_Attribute
      is
         Policy : Muxml.XML_Data_Type;
         Fn     : constant String := "create_memory_region_nosize.xml";
         Fn_Obj : constant String := "obj/" & Fn;
         Stream : Utils.Stream_Document_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory/file[@size]",
            Name  => "size",
            Value => "");
         Utils.Create (Stream_Doc => Stream,
                       Filename   => Fn_Obj);

         begin
            Create_Memory_Regions
              (Stream_Doc  => Stream,
               Phys_Memory => McKae.XML.XPath.XIA.XPath_Query
                 (N     => Policy.Doc,
                  XPath => "/system/memory/memory"));
            Utils.Close (Stream_Doc => Stream);
            Assert (Condition => False,
                    Message   => "Exception expected");
            Utils.Close (Stream_Doc => Stream);

         exception
            when Missing_Filesize =>
               Utils.Close (Stream_Doc => Stream);
               Ada.Directories.Delete_File (Name => Fn_Obj);
         end;
      end;
   begin
      Positive;
      No_Size_Attribute;
--  begin read only
   end Test_Create_Memory_Regions;
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
end Cmd_Stream.Roots.Memory.Test_Data.Tests;
