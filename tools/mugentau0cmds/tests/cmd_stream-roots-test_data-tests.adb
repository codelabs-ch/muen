--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cmd_Stream.Roots.Test_Data.

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
package body Cmd_Stream.Roots.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Allocate_Root (Gnattest_T : in out Test);
   procedure Test_Allocate_Root_1cdab9 (Gnattest_T : in out Test) renames Test_Allocate_Root;
--  id:2.2/1cdab91416646933/Allocate_Root/1/0/
   procedure Test_Allocate_Root (Gnattest_T : in out Test) is
   --  cmd_stream-roots.ads:23:4:Allocate_Root
--  end read only

      pragma Unreferenced (Gnattest_T);

      State : constant Positive := Allocate_Root;
   begin
      Current_Root := 0;
      Assert (Condition => Allocate_Root = 0,
              Message   => "Root not 0");
      Assert (Condition => Allocate_Root = 1,
              Message   => "Root not 1");

      Current_Root := State;

   exception
      when others =>
         Current_Root := State;
         raise;
--  begin read only
   end Test_Allocate_Root;
--  end read only


--  begin read only
   procedure Test_Allocate_Page_Table (Gnattest_T : in out Test);
   procedure Test_Allocate_Page_Table_264bea (Gnattest_T : in out Test) renames Test_Allocate_Page_Table;
--  id:2.2/264beaf765ac2410/Allocate_Page_Table/1/0/
   procedure Test_Allocate_Page_Table (Gnattest_T : in out Test) is
   --  cmd_stream-roots.ads:26:4:Allocate_Page_Table
--  end read only

      pragma Unreferenced (Gnattest_T);

      State : constant Positive := Allocate_Page_Table;
   begin
      Current_PT := 0;
      Assert (Condition => Allocate_Page_Table = 0,
              Message   => "PT not 0");
      Assert (Condition => Allocate_Page_Table = 1,
              Message   => "PT not 1");

      Current_PT := State;

   exception
      when others =>
         Current_PT := State;
         raise;
--  begin read only
   end Test_Allocate_Page_Table;
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
end Cmd_Stream.Roots.Test_Data.Tests;
