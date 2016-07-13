--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Merge.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Merge.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Concatenate (Gnattest_T : in out Test);
   procedure Test_Concatenate_f4291a (Gnattest_T : in out Test) renames Test_Concatenate;
--  id:2.2/f4291a7a671fcc46/Concatenate/1/0/
   procedure Test_Concatenate (Gnattest_T : in out Test) is
   --  merge-utils.ads:30:4:"&"
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_1 : constant String_Array
        := (1 => U ("foo"));
      Ref_2 : constant String_Array
        := (1 => U ("foo"),
            2 => U ("bar"));
   begin
      Assert (Condition => No_Strings & "foo" = Ref_1,
              Message   => "String array mismatch (1)");
      Assert (Condition => Ref_1 & "bar" = Ref_2,
              Message   => "String array mismatch (2)");
--  begin read only
   end Test_Concatenate;
--  end read only


--  begin read only
   procedure Test_Tokenize (Gnattest_T : in out Test);
   procedure Test_Tokenize_9caf86 (Gnattest_T : in out Test) renames Test_Tokenize;
--  id:2.2/9caf8602c5385e64/Tokenize/1/0/
   procedure Test_Tokenize (Gnattest_T : in out Test) is
   --  merge-utils.ads:37:4:Tokenize
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_1 : constant String_Array
        := (1 => U ("foo"),
            2 => U ("bar"),
            3 => U ("foobar"));
      Ref_2 : constant String_Array
        := (1 => Ada.Strings.Unbounded.Null_Unbounded_String,
            2 => Ada.Strings.Unbounded.Null_Unbounded_String,
            3 => Ada.Strings.Unbounded.Null_Unbounded_String);
      Ref_3 : constant String_Array
        := (1 => U ("foobar"));
      Ref_4 : constant String_Array
        := (1 => Ada.Strings.Unbounded.Null_Unbounded_String);
      Ref_5 : constant String_Array
        := (1 => U ("foo"),
            2 => U ("bar"),
            3 => U ("foo:bar"));
   begin
      Assert (Condition => Tokenize (Str => "foo:bar:foobar") = Ref_1,
              Message   => "Tokenized string mismatch (1)");
      Assert (Condition => Tokenize (Str => "::") = Ref_2,
              Message   => "Tokenized string mismatch (2)");
      Assert (Condition => Tokenize (Str => "foobar") = Ref_3,
              Message   => "Tokenized string mismatch (3)");
      Assert (Condition => Tokenize (Str => "") = Ref_4,
              Message   => "Tokenized string mismatch (4)");
      Assert (Condition => Tokenize (Str       => "foo,bar,foo:bar",
                                     Separator => ',') = Ref_5,
              Message   => "Tokenized string mismatch (5)");
--  begin read only
   end Test_Tokenize;
--  end read only


--  begin read only
   procedure Test_Lookup_File (Gnattest_T : in out Test);
   procedure Test_Lookup_File_1ff7d2 (Gnattest_T : in out Test) renames Test_Lookup_File;
--  id:2.2/1ff7d26f363b4287/Lookup_File/1/0/
   procedure Test_Lookup_File (Gnattest_T : in out Test) is
   --  merge-utils.ads:45:4:Lookup_File
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Lookup_File
              (Filename    => "run.xml",
               Directories => (1 => U ("nonexistent/path"),
                               2 => U ("obj"),
                               3 => U ("data"),
                               4 => U ("src"))) = "data/run.xml",
              Message   => "Directory mismatch (1)");

      Ada.Directories.Copy_File
        (Source_Name => "data/run.xml",
         Target_Name => "obj/run.xml");
      Assert (Condition => Lookup_File
              (Filename    => "run.xml",
               Directories => (1 => U ("nonexistent/path"),
                               2 => U ("obj"),
                               3 => U ("data"),
                               4 => U ("src"))) = "obj/run.xml",
              Message   => "Directory mismatch (2)");

      begin
         declare
            Dummy : constant String
              := Lookup_File
                (Filename    => "run.xml",
                 Directories => (1 => U ("nonexistent/path"),
                                 2 => U ("some/other/path"),
                                 3 => U ("src")));
         begin
            Assert (Condition => False,
                    Message   => "Exception expected (1)");
         end;

      exception
         when E : File_Not_Found =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "File 'run.xml' not found in any of the specified "
                    & "directories",
                    Message   => "Exception message mismatch (1)");
      end;

      begin
         declare
            Dummy : constant String
              := Lookup_File
                (Filename    => "nonexistent.xml",
                 Directories => (1 => U ("data"),
                                 2 => U ("src")));
         begin
            Assert (Condition => False,
                    Message   => "Exception expected (2)");
         end;

      exception
         when E : File_Not_Found =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "File 'nonexistent.xml' not found in any of the"
                    & " specified directories",
                    Message   => "Exception message mismatch (2)");
      end;

      Ada.Directories.Delete_File (Name => "obj/run.xml");
--  begin read only
   end Test_Lookup_File;
--  end read only

end Merge.Utils.Test_Data.Tests;