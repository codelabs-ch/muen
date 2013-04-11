with Ada.Directories;

with Skp.Templates;

with Test_Utils;

package body Templates_Tests
is

   use Ahven;
   use Skp;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Template tests");
      T.Add_Test_Routine
        (Routine => Load_Template'Access,
         Name    => "Load template");
      T.Add_Test_Routine
        (Routine => Replace_Patterns'Access,
         Name    => "Replace patterns");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Load_Template
   is
      T : Templates.Template_Type;
   begin
      begin
         T := Templates.Load (Filename => "template");
         Fail (Message => "Exception expected");

      exception
         when IO_Error => null;
      end;

      T := Templates.Load (Filename  => "data/template",
                           Use_Store => False);
      Assert (Condition => Templates.Get_Size (Template => T) = 37,
              Message   => "Template size mismatch (1)");

      Templates.Set_Template_Dir (Path => "./data");
      T := Templates.Load (Filename => "template");

      Assert (Condition => Templates.Get_Size (Template => T) = 37,
              Message   => "Template size mismatch (2)");
   end Load_Template;

   -------------------------------------------------------------------------

   procedure Replace_Patterns
   is
      Out_File : constant String := "obj/template";
      T        : Templates.Template_Type;
   begin
      Templates.Set_Template_Dir (Path => "./data");
      T := Templates.Load (Filename => "template");

      begin
         Templates.Replace (Template => T,
                            Pattern  => "NONEXISTENT_PATTERN",
                            Content  => "foobar");
         Fail (Message => "Exception expected");

      exception
         when Templates.Pattern_Not_Found => null;
      end;

      Templates.Replace (Template => T,
                         Pattern  => "PATTERN1",
                         Content  => "processed");
      Templates.Replace (Template => T,
                         Pattern  => "PATTERN2",
                         Content  => "file");
      Templates.Write (Template => T,
                       Filename => Out_File);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/template.ref",
               Filename2 => Out_File),
              Message   => "Template mismatch");

      Ada.Directories.Delete_File (Name => Out_File);
   end Replace_Patterns;

end Templates_Tests;
