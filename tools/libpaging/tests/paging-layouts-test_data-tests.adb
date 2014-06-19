--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Layouts.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Paging.Layouts.Test_Data.Tests is


--  begin read only
   procedure Test_Set_Address (Gnattest_T : in out Test);
   procedure Test_Set_Address_9d0225 (Gnattest_T : in out Test) renames Test_Set_Address;
--  id:2.2/9d0225d7a0c1251e/Set_Address/1/0/
   procedure Test_Set_Address (Gnattest_T : in out Test) is
   --  paging-layouts.ads:34:4:Set_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Ref_Addr : constant Interfaces.Unsigned_64 := 16#cafe_beef_0000#;
      Layout   : Memory_Layout_Type (Levels => 4);
   begin
      Set_Address (Mem_Layout => Layout,
                   Address    => Ref_Addr);

      Assert (Condition => Pagetables.Get_Physical_Address
              (Table => Layout.Level_1_Table) = Ref_Addr,
              Message   => "Layout address mismatch");
--  begin read only
   end Test_Set_Address;
--  end read only


--  begin read only
   procedure Test_Get_Address (Gnattest_T : in out Test);
   procedure Test_Get_Address_963b67 (Gnattest_T : in out Test) renames Test_Get_Address;
--  id:2.2/963b678c82b80686/Get_Address/1/0/
   procedure Test_Get_Address (Gnattest_T : in out Test) is
   --  paging-layouts.ads:39:4:Get_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Ref_Addr : constant Interfaces.Unsigned_64 := 16#fefe_1234_0000#;
      Layout   : Memory_Layout_Type (Levels => 4);
   begin
      Pagetables.Set_Physical_Address (Table   => Layout.Level_1_Table,
                                       Address => Ref_Addr);

      Assert (Condition => Get_Address (Mem_Layout => Layout) = Ref_Addr,
              Message   => "Layout address mismatch");
--  begin read only
   end Test_Get_Address;
--  end read only


--  begin read only
   procedure Test_Set_Large_Page_Support (Gnattest_T : in out Test);
   procedure Test_Set_Large_Page_Support_ade5cf (Gnattest_T : in out Test) renames Test_Set_Large_Page_Support;
--  id:2.2/ade5cfaf8657f33e/Set_Large_Page_Support/1/0/
   procedure Test_Set_Large_Page_Support (Gnattest_T : in out Test) is
   --  paging-layouts.ads:44:4:Set_Large_Page_Support
--  end read only

      pragma Unreferenced (Gnattest_T);

      Layout : Memory_Layout_Type (Levels => 4);
   begin
      Assert (Condition => Layout.Use_Large_Pages,
              Message   => "Large page support not default");

      Set_Large_Page_Support (Mem_Layout => Layout,
                              State      => False);
      Assert (Condition => not Layout.Use_Large_Pages,
              Message   => "Large page support enabled");

      Set_Large_Page_Support (Mem_Layout => Layout,
                              State      => True);
      Assert (Condition => Layout.Use_Large_Pages,
              Message   => "Large page support disabled");
--  begin read only
   end Test_Set_Large_Page_Support;
--  end read only


--  begin read only
   procedure Test_Add_Memory_Region (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Region_9a2b02 (Gnattest_T : in out Test) renames Test_Add_Memory_Region;
--  id:2.2/9a2b0204b5616592/Add_Memory_Region/1/0/
   procedure Test_Add_Memory_Region (Gnattest_T : in out Test) is
   --  paging-layouts.ads:49:4:Add_Memory_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Add_PD_Region
      is
         Layout : Memory_Layout_Type (Levels => 4);
      begin
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#0020_0000#,
            Virtual_Address  => 16#0040_0000#,
            Size             => PD_Page_Size,
            Caching          => WB,
            Writable         => True,
            Executable       => False);

         Assert (Condition => Pagetables.Contains
                 (Table => Layout.Level_1_Table,
                  Index => 0),
                 Message   => "Level 1 entry not created");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (2),
                  Table_Number => 0,
                  Entry_Index  => 0),
                 Message   => "Level 2 entry not created");
         Assert (Condition => Maps.Length (Map => Layout.Structures (2)) = 1,
                 Message   => "More than one level 2 table");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (3),
                  Table_Number => 0,
                  Entry_Index  => 2),
                 Message   => "Level 3 entry not created");
         Assert (Condition => Maps.Length (Map => Layout.Structures (3)) = 1,
                 Message   => "More than one level 3 table");
         Assert (Condition => Maps.Length (Map => Layout.Structures (4)) = 0,
                 Message   => "Level 4 table created");
      end Add_PD_Region;

      ----------------------------------------------------------------------

      procedure Add_PDPT_Region
      is
         Layout : Memory_Layout_Type (Levels => 4);
      begin
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#0000#,
            Virtual_Address  => 16#4000_0000#,
            Size             => PDPT_Page_Size,
            Caching          => WB,
            Writable         => True,
            Executable       => False);

         Assert (Condition => Pagetables.Contains
                 (Table => Layout.Level_1_Table,
                  Index => 0),
                 Message   => "Level 1 entry not created");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (2),
                  Table_Number => 0,
                  Entry_Index  => 1),
                 Message   => "Level 2 entry not created");
         Assert (Condition => Maps.Length (Map => Layout.Structures (2)) = 1,
                 Message   => "More than one level 2 table");
         Assert (Condition => Maps.Length (Map => Layout.Structures (3)) = 0,
                 Message   => "Level 3 table created");
         Assert (Condition => Maps.Length (Map => Layout.Structures (4)) = 0,
                 Message   => "Level 4 table created");
      end Add_PDPT_Region;

      ----------------------------------------------------------------------

      procedure Add_PT_Region
      is
         Layout : Memory_Layout_Type (Levels => 4);
      begin
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#1000#,
            Virtual_Address  => 16#0caf_ebee_f000#,
            Size             => Page_Size,
            Caching          => WB,
            Writable         => True,
            Executable       => False);

         Assert (Condition => Pagetables.Contains
                 (Table => Layout.Level_1_Table,
                  Index => 25),
                 Message   => "Level 1 entry not created");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (2),
                  Table_Number => 25,
                  Entry_Index  => 191),
                 Message   => "Level 2 entry not created");
         Assert (Condition => Maps.Length (Map => Layout.Structures (2)) = 1,
                 Message   => "More than one level 2 table");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (3),
                  Table_Number => 191,
                  Entry_Index  => 351),
                 Message   => "Level 3 entry not created");
         Assert (Condition => Maps.Length (Map => Layout.Structures (3)) = 1,
                 Message   => "More than one level 3 table");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (4),
                  Table_Number => 351,
                  Entry_Index  => 239),
                 Message   => "Level 4 entry not created");
         Assert (Condition => Maps.Length (Map => Layout.Structures (4)) = 1,
                 Message   => "More than one level 4 table");
      end Add_PT_Region;
   begin
      Add_PT_Region;
      Add_PD_Region;
      Add_PDPT_Region;
--  begin read only
   end Test_Add_Memory_Region;
--  end read only


--  begin read only
   procedure Test_Update_References (Gnattest_T : in out Test);
   procedure Test_Update_References_e977e1 (Gnattest_T : in out Test) renames Test_Update_References;
--  id:2.2/e977e15fcefa86bd/Update_References/1/0/
   procedure Test_Update_References (Gnattest_T : in out Test) is
   --  paging-layouts.ads:60:4:Update_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Layout : Memory_Layout_Type (Levels => 4);
   begin
      Set_Address (Mem_Layout => Layout,
                   Address    => 16#001f_0000#);
      Add_Memory_Region
        (Mem_Layout       => Layout,
         Physical_Address => 16#1000#,
         Virtual_Address  => 16#0caf_ebee_f000#,
         Size             => Page_Size,
         Caching          => WB,
         Writable         => True,
         Executable       => False);

      Assert (Condition => Maps.Get_Table_Address
              (Map          => Layout.Structures (2),
               Table_Number => 25) = 0,
              Message   => "Level 2 table address set");
      Assert (Condition => Maps.Get_Table_Address
              (Map          => Layout.Structures (3),
               Table_Number => 191) = 0,
              Message   => "Level 3 table address set");
      Assert (Condition => Maps.Get_Table_Address
              (Map          => Layout.Structures (4),
               Table_Number => 351) = 0,
              Message   => "Level 4 table address set");

      Update_References (Mem_Layout => Layout);
      Assert (Condition => Maps.Get_Table_Address
              (Map          => Layout.Structures (2),
               Table_Number => 25) = 16#001f_3000#,
              Message   => "Level 2 table address mismatch");
      Assert (Condition => Maps.Get_Table_Address
              (Map          => Layout.Structures (3),
               Table_Number => 191) = 16#001f_2000#,
              Message   => "Level 3 table address mismatch");
      Assert (Condition => Maps.Get_Table_Address
              (Map          => Layout.Structures (4),
               Table_Number => 351) = 16#001f_1000#,
              Message   => "Level 4 table address mismatch");
--  begin read only
   end Test_Update_References;
--  end read only


--  begin read only
   procedure Test_Get_Table_Count (Gnattest_T : in out Test);
   procedure Test_Get_Table_Count_035af5 (Gnattest_T : in out Test) renames Test_Get_Table_Count;
--  id:2.2/035af58847312281/Get_Table_Count/1/0/
   procedure Test_Get_Table_Count (Gnattest_T : in out Test) is
   --  paging-layouts.ads:66:4:Get_Table_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

      Layout : Memory_Layout_Type (Levels => 4) := Null_Layout;
   begin
      declare
         Counts : constant Table_Count_Array
           := Get_Table_Count (Mem_Layout => Layout);
      begin
         for I in Counts'Range loop
            Assert (Condition => Counts (I) = 0,
              Message   => "Level" & I'Img & " count not 0");
         end loop;
      end;

      Add_Memory_Region
        (Mem_Layout       => Layout,
         Physical_Address => 16#000f_f000#,
         Virtual_Address  => 16#000f_f000#,
         Size             => 16#0001_0020_2000#,
         Caching          => UC,
         Writable         => True,
         Executable       => True);

      declare
         Counts : constant Table_Count_Array
           := Get_Table_Count (Mem_Layout => Layout);
      begin
         Assert (Condition => Counts (1) = 1,
                 Message   => "Level 1 count not 1");
         Assert (Condition => Counts (2) = 1,
                 Message   => "Level 2 count not 1");
         Assert (Condition => Counts (3) = 2,
                 Message   => "Level 3 count not 2");
         Assert (Condition => Counts (4) = 2,
                 Message   => "Level 4 count not 2");
      end;
--  begin read only
   end Test_Get_Table_Count;
--  end read only

end Paging.Layouts.Test_Data.Tests;
