--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Test_Data.

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
package body Paging.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_Indexes (Gnattest_T : in out Test);
   procedure Test_Get_Indexes_9d42e6 (Gnattest_T : in out Test) renames Test_Get_Indexes;
--  id:2.2/9d42e63e2adac92c/Get_Indexes/0/0/
   procedure Test_Get_Indexes (Gnattest_T : in out Test) is
   --  paging.ads:60:4:Get_Indexes
--  end read only

      pragma Unreferenced (Gnattest_T);
      Indexes : Table_Index_Array (Paging_Level);
   begin
      Get_Indexes (Address => 0,
                   Indexes => Indexes);
      Assert (Condition => Indexes (1) = 0,
              Message   => "PML4 index mismatch (1)");
      Assert (Condition => Indexes (2) = 0,
              Message   => "PDPT index mismatch (1)");
      Assert (Condition => Indexes (3) = 0,
              Message   => "PD index mismatch (1)");
      Assert (Condition => Indexes (4) = 0,
              Message   => "PT index mismatch (1)");

      Get_Indexes (Address => Interfaces.Unsigned_64'Last,
                   Indexes => Indexes);
      Assert (Condition => Indexes (1) = Entry_Range'Last,
              Message   => "PML4 index mismatch (2)");
      Assert (Condition => Indexes (2) = Entry_Range'Last,
              Message   => "PDPT index mismatch (2)");
      Assert (Condition => Indexes (3) = Entry_Range'Last,
              Message   => "PD index mismatch (2)");
      Assert (Condition => Indexes (4) = Entry_Range'Last,
              Message   => "PT index mismatch (2)");

      Get_Indexes (Address => 16#000f_ffc8_0200_f000#,
                   Indexes => Indexes);
      Assert (Condition => Indexes (1) = 511,
              Message   => "PML4 index mismatch (3)");
      Assert (Condition => Indexes (2) = 288,
              Message   => "PDPT index mismatch (3)");
      Assert (Condition => Indexes (3) = 16,
              Message   => "PD index mismatch (3)");
      Assert (Condition => Indexes (4) = 15,
              Message   => "PT index mismatch (3)");

      declare
         Lvl3_Indexes : Table_Index_Array (1 .. 3);
      begin
         Get_Indexes (Address => 16#00c8_0200_f000#,
                      Indexes => Lvl3_Indexes);
         Assert (Condition => Lvl3_Indexes (1) = 288,
                 Message   => "PDPT index mismatch (4)");
         Assert (Condition => Lvl3_Indexes (2) = 16,
                 Message   => "PD index mismatch (4)");
         Assert (Condition => Lvl3_Indexes (3) = 15,
                 Message   => "PT index mismatch (4)");
      end;
--  begin read only
   end Test_Get_Indexes;
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
end Paging.Test_Data.Tests;
