--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Muxml.Grammar.Test_Data.

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
package body Muxml.Grammar.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_Grammar (Gnattest_T : in out Test);
   procedure Test_Get_Grammar_aa2e3c (Gnattest_T : in out Test) renames Test_Get_Grammar;
--  id:2.2/aa2e3cc2b342fb63/Get_Grammar/1/0/
   procedure Test_Get_Grammar (Gnattest_T : in out Test) is
   --  muxml-grammar.ads:25:4:Get_Grammar
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Schema.Validators.XML_Grammar;

      G : Schema.Validators.XML_Grammar;
   begin
      for I in Valid_Schema_Kind loop
         G := Schema.Validators.No_Grammar;
         G := Get_Grammar (Kind => Format_Src);
         Assert (Condition => G /= Schema.Validators.No_Grammar,
                 Message   => "Grammar is null");
      end loop;
--  begin read only
   end Test_Get_Grammar;
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
end Muxml.Grammar.Test_Data.Tests;
