--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Msrstore.Tables.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Msrstore.Tables.Test_Data.Tests is


--  begin read only
   procedure Test_Is_Full (Gnattest_T : in out Test);
   procedure Test_Is_Full_aaff6f (Gnattest_T : in out Test) renames Test_Is_Full;
--  id:2.2/aaff6f63df362f53/Is_Full/1/0/
   procedure Test_Is_Full (Gnattest_T : in out Test) is
   --  msrstore-tables.ads:30:4:Is_Full
--  end read only

      pragma Unreferenced (Gnattest_T);

      Store : MSR_Store_Type (Size => 2);
   begin
      Assert (Condition => Store.Size = 2,
              Message   => "Invalid MSR store size");
      Assert (Condition => Store.Next_Idx = 1,
              Message   => "Empty MSR store contains entry");

      Assert (Condition => not Is_Full (Store => Store),
              Message   => "Empty MSR store is full");

      Store.Next_Idx := 2;
      Assert (Condition => not Is_Full (Store => Store),
              Message   => "MSR store is full");

      Store.Next_Idx := 3;
      Assert (Condition => Is_Full (Store => Store),
              Message   => "MSR store not full");
--  begin read only
   end Test_Is_Full;
--  end read only


--  begin read only
   procedure Test_Append_Entry (Gnattest_T : in out Test);
   procedure Test_Append_Entry_ce511a (Gnattest_T : in out Test) renames Test_Append_Entry;
--  id:2.2/ce511a06b30ed5cd/Append_Entry/1/0/
   procedure Test_Append_Entry (Gnattest_T : in out Test) is
   --  msrstore-tables.ads:33:4:Append_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_64;

      Store : MSR_Store_Type (Size => 2);
   begin
      Append_Entry (Store => Store,
                    Index => 16#c000_0080#,
                    Data  => 16#cafe_beef#);
      Assert (Condition => Store.Next_Idx = 2,
              Message   => "Entry not appended (1)");
      Assert (Condition => Store.Data (1).Index = 16#c000_0080#,
              Message   => "Index mismatch (1)");
      Assert (Condition => Store.Data (1).Reserved = 0,
              Message   => "Reserved field mismatch (1)");
      Assert (Condition => Store.Data (1).Data = 16#cafe_beef#,
              Message   => "Data mismatch (1)");

      Append_Entry (Store => Store,
                    Index => 16#beef_beef#,
                    Data  => 16#0123_4567#);
      Assert (Condition => Store.Next_Idx = 3,
              Message   => "Entry not appended (2)");
      Assert (Condition => Store.Data (2).Index = 16#beef_beef#,
              Message   => "Index mismatch (2)");
      Assert (Condition => Store.Data (2).Reserved = 0,
              Message   => "Reserved field mismatch (2)");
      Assert (Condition => Store.Data (2).Data = 16#0123_4567#,
              Message   => "Data mismatch (2)");
--  begin read only
   end Test_Append_Entry;
--  end read only

end Msrstore.Tables.Test_Data.Tests;
