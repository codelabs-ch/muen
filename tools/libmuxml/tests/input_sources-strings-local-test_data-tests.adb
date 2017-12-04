--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Input_Sources.Strings.Local.Test_Data.

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
package body Input_Sources.Strings.Local.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Open (Gnattest_T : in out Test);
   procedure Test_Open_36083b (Gnattest_T : in out Test) renames Test_Open;
--  id:2.2/36083bb42c6d59cb/Open/1/0/
   procedure Test_Open (Gnattest_T : in out Test) is
   --  input_sources-strings-local.ads:24:4:Open
--  end read only

      pragma Unreferenced (Gnattest_T);

      Str_Input : Input_Sources.Strings.String_Input;
   begin
      Open (Str      => "<doc>the doc</doc>",
            Encoding => Unicode.CES.Utf8.Utf8_Encoding,
            Input    => Str_Input);

      Assert (Condition => Str_Input.Index = Str_Input.Buffer'First
              + Str_Input.Prolog_Size,
              Message   => "Index mismatch");
--  begin read only
   end Test_Open;
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
end Input_Sources.Strings.Local.Test_Data.Tests;
