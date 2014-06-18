--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.Tables.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body VTd.Tables.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Entry_c1683a (Gnattest_T : in out Test) renames Test_Add_Entry;
--  id:2.2/c1683ae3e6351283/Add_Entry/1/0/
   procedure Test_Add_Entry (Gnattest_T : in out Test) is
   --  vtd-tables.ads:37:4:Add_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      Root_Table : Root_Table_Type;
   begin
      Add_Entry (RT  => Root_Table,
                 Bus => 12,
                 CTP => 16#0010_0000#);
      Assert (Condition => Root_Table.Entries (12).Present = 1,
              Message   => "Entry not present");
      Assert (Condition => Root_Table.Entries (12).CTP = 16#0010_0000#,
              Message   => "Entry CTP mismatch");
--  begin read only
   end Test_Add_Entry;
--  end read only

end VTd.Tables.Test_Data.Tests;
