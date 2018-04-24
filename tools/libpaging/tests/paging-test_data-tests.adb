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
   --  paging.ads:62:4:Get_Indexes
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
   procedure Test_Get_Index (Gnattest_T : in out Test);
   procedure Test_Get_Index_085052 (Gnattest_T : in out Test) renames Test_Get_Index;
--  id:2.2/0850521be1519b4e/Get_Index/1/0/
   procedure Test_Get_Index (Gnattest_T : in out Test) is
   --  paging.ads:68:4:Get_Index
--  end read only

      pragma Unreferenced (Gnattest_T);
   begin
      Assert (Condition => Get_Index (Address => 16#090a_8fef_f000#,
                                      Level   => 1) = 18,
              Message   => "PML4 index mismatch");
      Assert (Condition => Get_Index (Address => 16#090a_8fef_f000#,
                                      Level   => 2) = 42,
              Message   => "PDPT index mismatch");
      Assert (Condition => Get_Index (Address => 16#090a_8fef_f000#,
                                      Level   => 3) = 127,
              Message   => "PD index mismatch");
      Assert (Condition => Get_Index (Address => 16#090a_8fef_f000#,
                                      Level   => 4) = 255,
              Message   => "PT index mismatch");
--  begin read only
   end Test_Get_Index;
--  end read only


--  begin read only
   procedure Test_Get_Offset (Gnattest_T : in out Test);
   procedure Test_Get_Offset_64b49e (Gnattest_T : in out Test) renames Test_Get_Offset;
--  id:2.2/64b49e6ebf507313/Get_Offset/1/0/
   procedure Test_Get_Offset (Gnattest_T : in out Test) is
   --  paging.ads:75:4:Get_Offset
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Lvl2_Offset_Mask : constant Interfaces.Unsigned_64 := 16#3fffffff#;
      Lvl3_Offset_Mask : constant Interfaces.Unsigned_64 := 16#001fffff#;
      Lvl4_Offset_Mask : constant Interfaces.Unsigned_64 := 16#00000fff#;

      VAddr : constant Interfaces.Unsigned_64 := 16#090a_beef_cafe#;
   begin
      Assert (Condition => Get_Offset
              (Address => 0,
               Level   => 2) = 0,
              Message   => "PDPT offset mismatch (1)");
      Assert (Condition => Get_Offset
              (Address => 0,
               Level   => 3) = 0,
              Message   => "PD offset mismatch (1)");
      Assert (Condition => Get_Offset
              (Address => 0,
               Level   => 4) = 0,
              Message   => "PT offset mismatch (1)");
      Assert (Condition => Get_Offset
              (Address => VAddr,
               Level   => 2) = (VAddr and Lvl2_Offset_Mask),
              Message   => "PDPT offset mismatch (2)");
      Assert (Condition => Get_Offset
              (Address => VAddr,
               Level   => 3) = (VAddr and Lvl3_Offset_Mask),
              Message   => "PD offset mismatch (2)");
      Assert (Condition => Get_Offset
              (Address => VAddr,
               Level   => 4) = (VAddr and Lvl4_Offset_Mask),
              Message   => "PT offset mismatch (2)");
--  begin read only
   end Test_Get_Offset;
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
