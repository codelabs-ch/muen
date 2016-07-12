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
   procedure Test_Tokenize (Gnattest_T : in out Test);
   procedure Test_Tokenize_9caf86 (Gnattest_T : in out Test) renames Test_Tokenize;
--  id:2.2/9caf8602c5385e64/Tokenize/1/0/
   procedure Test_Tokenize (Gnattest_T : in out Test) is
   --  merge-utils.ads:29:4:Tokenize
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U
        (Source : String)
         return Ada.Strings.Unbounded.Unbounded_String
         renames Ada.Strings.Unbounded.To_Unbounded_String;

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

end Merge.Utils.Test_Data.Tests;