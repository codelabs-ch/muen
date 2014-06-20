--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Paging.Test_Data.Tests is


--  begin read only
   procedure Test_1_Get_Indexes (Gnattest_T : in out Test);
   procedure Test_Get_Indexes_b21229 (Gnattest_T : in out Test) renames Test_1_Get_Indexes;
--  id:2.2/b21229569172abbf/Get_Indexes/1/0/
   procedure Test_1_Get_Indexes (Gnattest_T : in out Test) is
   --  paging.ads:52:4:Get_Indexes
--  end read only

      pragma Unreferenced (Gnattest_T);

      PML4, PDPT, PD, PT : Table_Range;
   begin
      Get_Indexes (Address    => 0,
                   PML4_Index => PML4,
                   PDPT_Index => PDPT,
                   PD_Index   => PD,
                   PT_Index   => PT);
      Assert (Condition => PML4 = 0,
              Message   => "PML4 index mismatch (1)");
      Assert (Condition => PDPT = 0,
              Message   => "PDPT index mismatch (1)");
      Assert (Condition => PD = 0,
              Message   => "PD index mismatch (1)");
      Assert (Condition => PT = 0,
              Message   => "PT index mismatch (1)");

      Get_Indexes (Address    => Interfaces.Unsigned_64'Last,
                   PML4_Index => PML4,
                   PDPT_Index => PDPT,
                   PD_Index   => PD,
                   PT_Index   => PT);
      Assert (Condition => PML4 = Table_Range'Last,
              Message   => "PML4 index mismatch (2)");
      Assert (Condition => PDPT = Table_Range'Last,
              Message   => "PDPT index mismatch (2)");
      Assert (Condition => PD = Table_Range'Last,
              Message   => "PD index mismatch (2)");
      Assert (Condition => PT = Table_Range'Last,
              Message   => "PT index mismatch (2)");

      Get_Indexes (Address    => 16#fffc80200f000#,
                   PML4_Index => PML4,
                   PDPT_Index => PDPT,
                   PD_Index   => PD,
                   PT_Index   => PT);
      Assert (Condition => PML4 = 511,
              Message   => "PML4 index mismatch (3)");
      Assert (Condition => PDPT = 288,
              Message   => "PDPT index mismatch (3)");
      Assert (Condition => PD = 16,
              Message   => "PD index mismatch (3)");
      Assert (Condition => PT = 15,
              Message   => "PT index mismatch (3)");
--  begin read only
   end Test_1_Get_Indexes;
--  end read only


--  begin read only
   procedure Test_2_Get_Indexes (Gnattest_T : in out Test);
   procedure Test_Get_Indexes_9d42e6 (Gnattest_T : in out Test) renames Test_2_Get_Indexes;
--  id:2.2/9d42e63e2adac92c/Get_Indexes/0/0/
   procedure Test_2_Get_Indexes (Gnattest_T : in out Test) is
   --  paging.ads:69:4:Get_Indexes
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
      Assert (Condition => Indexes (1) = Table_Range'Last,
              Message   => "PML4 index mismatch (2)");
      Assert (Condition => Indexes (2) = Table_Range'Last,
              Message   => "PDPT index mismatch (2)");
      Assert (Condition => Indexes (3) = Table_Range'Last,
              Message   => "PD index mismatch (2)");
      Assert (Condition => Indexes (4) = Table_Range'Last,
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
   end Test_2_Get_Indexes;
--  end read only

end Paging.Test_Data.Tests;
