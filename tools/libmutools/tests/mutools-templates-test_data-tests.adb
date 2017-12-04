--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Templates.Test_Data.

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
package body Mutools.Templates.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Create (Gnattest_T : in out Test);
   procedure Test_Create_4652d7 (Gnattest_T : in out Test) renames Test_Create;
--  id:2.2/4652d7f723cf959d/Create/1/0/
   procedure Test_Create (Gnattest_T : in out Test) is
   --  mutools-templates.ads:27:4:Create
--  end read only

      pragma Unreferenced (Gnattest_T);

      T : Template_Type;
   begin
      T := Create (Content => Tmpl);
      Assert (Condition => Ada.Strings.Unbounded.Length
              (Source => T.Data) = 55,
              Message   => "Template size mismatch");
--  begin read only
   end Test_Create;
--  end read only


--  begin read only
   procedure Test_Replace (Gnattest_T : in out Test);
   procedure Test_Replace_a74ecb (Gnattest_T : in out Test) renames Test_Replace;
--  id:2.2/a74ecbbbacf1d1be/Replace/1/0/
   procedure Test_Replace (Gnattest_T : in out Test) is
   --  mutools-templates.ads:30:4:Replace
--  end read only

      pragma Unreferenced (Gnattest_T);

      Out_File : constant String := "obj/template";
      T        : Template_Type;
   begin
      T.Data := Ada.Strings.Unbounded.To_Unbounded_String (Source => Tmpl);

      begin
         Replace (Template => T,
                  Pattern  => "NONEXISTENT_PATTERN",
                  Content  => "foobar");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Pattern_Not_Found => null;
      end;

      Replace (Template => T,
               Pattern  => "PATTERN1",
               Content  => "processed");
      Replace (Template => T,
               Pattern  => "PATTERN2",
               Content  => "file");
      Write (Template => T,
             Filename => Out_File);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/template.ref",
               Filename2 => Out_File),
              Message   => "Template mismatch");

      Ada.Directories.Delete_File (Name => Out_File);
--  begin read only
   end Test_Replace;
--  end read only


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_53091d (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/53091d21d5c910db/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  mutools-templates.ads:36:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Replace");
--  begin read only
   end Test_Write;
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
end Mutools.Templates.Test_Data.Tests;
