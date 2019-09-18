--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Strings.Test_Data.

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
package body Mutools.Strings.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Concatenate (Gnattest_T : in out Test);
   procedure Test_Concatenate_59d0be (Gnattest_T : in out Test) renames Test_Concatenate;
--  id:2.2/59d0bec9a90312aa/Concatenate/1/0/
   procedure Test_Concatenate (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      No_Strings : constant String_Array (1 .. 0) := (others => <>);

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
   procedure Test_Tokenize_5cd120 (Gnattest_T : in out Test) renames Test_Tokenize;
--  id:2.2/5cd120e012aef233/Tokenize/1/0/
   procedure Test_Tokenize (Gnattest_T : in out Test) is
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
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Mutools.Strings.Test_Data.Tests;
