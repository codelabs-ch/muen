--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Templates.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Spec.Templates.Test_Data.Tests is


--  begin read only
   procedure Test_Create (Gnattest_T : in out Test);
   procedure Test_Create_fbd165 (Gnattest_T : in out Test) renames Test_Create;
--  id:2.2/fbd165d850fee6bf/Create/1/0/
   procedure Test_Create (Gnattest_T : in out Test) is
   --  spec-templates.ads:27:4:Create
--  end read only

      pragma Unreferenced (Gnattest_T);

      T : Template_Type;
   begin
      T := Create (Content => Tmpl);
      Assert (Condition => Ada.Strings.Unbounded.Length
              (Source => T.Data) = 37,
              Message   => "Template size mismatch");
--  begin read only
   end Test_Create;
--  end read only


--  begin read only
   procedure Test_Replace (Gnattest_T : in out Test);
   procedure Test_Replace_d85dde (Gnattest_T : in out Test) renames Test_Replace;
--  id:2.2/d85dde1373bd8ec9/Replace/1/0/
   procedure Test_Replace (Gnattest_T : in out Test) is
   --  spec-templates.ads:31:4:Replace
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
   procedure Test_Write_220c26 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/220c26b6a1044317/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  spec-templates.ads:37:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Replace");
--  begin read only
   end Test_Write;
--  end read only

end Spec.Templates.Test_Data.Tests;
