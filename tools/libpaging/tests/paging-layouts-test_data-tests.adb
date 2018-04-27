--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Layouts.Test_Data.

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
package body Paging.Layouts.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Set_Address (Gnattest_T : in out Test);
   procedure Test_Set_Address_9d0225 (Gnattest_T : in out Test) renames Test_Set_Address;
--  id:2.2/9d0225d7a0c1251e/Set_Address/1/0/
   procedure Test_Set_Address (Gnattest_T : in out Test) is
   --  paging-layouts.ads:37:4:Set_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Ref_Addr : constant Interfaces.Unsigned_64 := 16#cafe_beef_0000#;
      Layout   : Memory_Layout_Type (Levels => 4);
   begin
      Set_Address (Mem_Layout => Layout,
                   Address    => Ref_Addr);

      Assert (Condition => Tables.Get_Physical_Address
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
   --  paging-layouts.ads:42:4:Get_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Ref_Addr : constant Interfaces.Unsigned_64 := 16#fefe_1234_0000#;
      Layout   : Memory_Layout_Type (Levels => 4);
   begin
      Tables.Set_Physical_Address (Table   => Layout.Level_1_Table,
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
   --  paging-layouts.ads:47:4:Set_Large_Page_Support
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
   --  paging-layouts.ads:52:4:Add_Memory_Region
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

         Assert (Condition => Tables.Contains
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

         Assert (Condition => Tables.Contains
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

         Assert (Condition => Tables.Contains
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
                  Table_Number => 25 * 512 + 191,
                  Entry_Index  => 351),
                 Message   => "Level 3 entry not created");
         Assert (Condition => Maps.Length (Map => Layout.Structures (3)) = 1,
                 Message   => "More than one level 3 table");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (4),
                  Table_Number => (25 * 512 + 191) * 512 + 351,
                  Entry_Index  => 239),
                 Message   => "Level 4 entry not created");
         Assert (Condition => Maps.Length (Map => Layout.Structures (4)) = 1,
                 Message   => "More than one level 4 table");
      end Add_PT_Region;

      ----------------------------------------------------------------------

      procedure Add_Multiple_PT_Regions
      is
         Layout : Memory_Layout_Type (Levels => 4);
      begin
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#3c79_0000#,
            Virtual_Address  => 16#e000_0000#,
            Size             => Page_Size,
            Caching          => WB,
            Writable         => False,
            Executable       => False);

         Assert (Condition => Tables.Contains
                 (Table => Layout.Level_1_Table,
                  Index => 0),
                 Message   => "Level 1 entry not created");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (2),
                  Table_Number => 0,
                  Entry_Index  => 3),
                 Message   => "Level 2 entry not created (1)");
         Assert (Condition => Maps.Length (Map => Layout.Structures (2)) = 1,
                 Message   => "More than one level 2 table");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (3),
                  Table_Number => 3,
                  Entry_Index  => 256),
                 Message   => "Level 3 entry not created (1)");
         Assert (Condition => Maps.Length (Map => Layout.Structures (3)) = 1,
                 Message   => "More than one level 3 table");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (4),
                  Table_Number => 3 * 512 + 256,
                  Entry_Index  => 0),
                 Message   => "Level 4 entry not created (1)");
         Assert (Condition => Maps.Length (Map => Layout.Structures (4)) = 1,
                 Message   => "More than one level 4 table");

         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#3c79_8000#,
            Virtual_Address  => 16#a000_0000#,
            Size             => Page_Size,
            Caching          => WB,
            Writable         => False,
            Executable       => False);

         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (2),
                  Table_Number => 0,
                  Entry_Index  => 2),
                 Message   => "Level 2 entry not created (2)");
         Assert (Condition => Maps.Length (Map => Layout.Structures (2)) = 1,
                 Message   => "More than one level 2 table");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (3),
                  Table_Number => 2,
                  Entry_Index  => 256),
                 Message   => "Level 3 entry not created (1)");
         Assert (Condition => Maps.Length (Map => Layout.Structures (3)) = 2,
                 Message   => "Level 3 table count mismatch");
         Assert (Condition => Maps.Contains
                 (Map          => Layout.Structures (4),
                  Table_Number => 2 * 512 + 256,
                  Entry_Index  => 0),
                 Message   => "Level 4 entry not created (2)");
         Assert (Condition => Maps.Length (Map => Layout.Structures (4)) = 2,
                 Message   => "Level 4 table count mismatch");
      end Add_Multiple_PT_Regions;

      ----------------------------------------------------------------------

      procedure Add_Duplicate_PT_Regions
      is
         Layout : Memory_Layout_Type (Levels => 4);
      begin
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#3c79_0000#,
            Virtual_Address  => 16#e000_0000#,
            Size             => Page_Size,
            Caching          => WB,
            Writable         => False,
            Executable       => False);

         --  Adding the identical mapping multiple times must not raise an
         --  exception.

         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#3c79_0000#,
            Virtual_Address  => 16#e000_0000#,
            Size             => Page_Size,
            Caching          => WB,
            Writable         => False,
            Executable       => False);

         begin
            Add_Memory_Region
              (Mem_Layout       => Layout,
               Physical_Address => 16#1000#,
               Virtual_Address  => 16#e000_0000#,
               Size             => Page_Size,
               Caching          => WB,
               Writable         => False,
               Executable       => True);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Mapping_Present =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Multiple mappings of VMA 16#e000_0000# with "
                       & "different attributes present",
                       Message   => "Exception mismatch");
         end;
      end Add_Duplicate_PT_Regions;

      ----------------------------------------------------------------------

      procedure Add_Large_Region_Three_Levels
      is
         Layout : Memory_Layout_Type (Levels => 3);
      begin
         Set_Large_Page_Support (Mem_Layout => Layout,
                                 State      => True);
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#0000#,
            Virtual_Address  => 16#0000#,
            Size             => PDPT_Page_Size,
            Caching          => WB,
            Writable         => True,
            Executable       => False);

         Assert (Condition => Tables.Contains
                 (Table => Layout.Level_1_Table,
                  Index => 0),
                 Message   => "Level 1 entry not created");

         --  All entries in the table must be filled.

         for I in Entry_Range loop
            Assert (Condition => Maps.Contains
                    (Map          => Layout.Structures (2),
                     Table_Number => 0,
                     Entry_Index  => I),
                    Message   => "Level 2 table has no entry" & I'Img);
         end loop;
      end Add_Large_Region_Three_Levels;
   begin
      Add_PT_Region;
      Add_PD_Region;
      Add_PDPT_Region;
      Add_Multiple_PT_Regions;
      Add_Duplicate_PT_Regions;
      Add_Large_Region_Three_Levels;
--  begin read only
   end Test_Add_Memory_Region;
--  end read only


--  begin read only
   procedure Test_Update_References (Gnattest_T : in out Test);
   procedure Test_Update_References_e977e1 (Gnattest_T : in out Test) renames Test_Update_References;
--  id:2.2/e977e15fcefa86bd/Update_References/1/0/
   procedure Test_Update_References (Gnattest_T : in out Test) is
   --  paging-layouts.ads:63:4:Update_References
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
               Table_Number => 25 * 512 + 191) = 0,
              Message   => "Level 3 table address set");
      Assert (Condition => Maps.Get_Table_Address
              (Map          => Layout.Structures (4),
               Table_Number => (25 * 512 + 191) * 512 + 351) = 0,
              Message   => "Level 4 table address set");

      Update_References (Mem_Layout => Layout);
      Assert (Condition => Maps.Get_Table_Address
              (Map          => Layout.Structures (2),
               Table_Number => 25) = 16#001f_3000#,
              Message   => "Level 2 table address mismatch");
      Assert (Condition => Maps.Get_Table_Address
              (Map          => Layout.Structures (3),
               Table_Number => 25 * 512 + 191) = 16#001f_2000#,
              Message   => "Level 3 table address mismatch");
      Assert (Condition => Maps.Get_Table_Address
              (Map          => Layout.Structures (4),
               Table_Number => (25 * 512 + 191) * 512 + 351) = 16#001f_1000#,
              Message   => "Level 4 table address mismatch");
--  begin read only
   end Test_Update_References;
--  end read only


--  begin read only
   procedure Test_Get_Table_Count (Gnattest_T : in out Test);
   procedure Test_Get_Table_Count_035af5 (Gnattest_T : in out Test) renames Test_Get_Table_Count;
--  id:2.2/035af58847312281/Get_Table_Count/1/0/
   procedure Test_Get_Table_Count (Gnattest_T : in out Test) is
   --  paging-layouts.ads:69:4:Get_Table_Count
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


--  begin read only
   procedure Test_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_129d4a (Gnattest_T : in out Test) renames Test_Serialize;
--  id:2.2/129d4a279ba3ac99/Serialize/1/0/
   procedure Test_Serialize (Gnattest_T : in out Test) is
   --  paging-layouts.ads:85:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure EPT_Generate_Paging_Structures
      is
         Layout : Memory_Layout_Type := Null_Layout;
      begin
         Set_Address (Mem_Layout => Layout,
                      Address    => 16#001f_4000#);

         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#4000_0000#,
            Virtual_Address  => 16#0#,
            Size             => 16#1_0000_0000#,
            Caching          => UC,
            Writable         => True,
            Executable       => True);

         Update_References (Mem_Layout => Layout);

         declare
            use Ada.Streams.Stream_IO;

            File : File_Type;
         begin
            Mutools.Files.Open (Filename => "obj/ept",
                                File     => File);
            Serialize (Stream      => Stream (File => File),
                       Mem_Layout  => Layout,
                       Serializers =>
                         (1 => EPT.Serialize_PML4'Access,
                          2 => EPT.Serialize_PDPT'Access,
                          3 => EPT.Serialize_PD'Access,
                          4 => EPT.Serialize_PT'Access));
            Close (File => File);
         end;

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/ept.ref",
                  Filename2 => "obj/ept"),
                 Message   => "EPT paging structures mismatch");
      end EPT_Generate_Paging_Structures;

      ----------------------------------------------------------------------

      procedure EPT_Generate_Paging_Structures_No_Large_Pages
      is
         Layout : Memory_Layout_Type := Null_Layout;
      begin
         Set_Large_Page_Support (Mem_Layout => Layout,
                                 State      => False);
         Set_Address (Mem_Layout => Layout,
                      Address    => 16#001f_4000#);

          Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#4000_0000#,
            Virtual_Address  => 16#0#,
            Size             => 16#0020_0000#,
            Caching          => UC,
            Writable         => True,
            Executable       => True);

         Update_References (Mem_Layout => Layout);

         declare
            use Ada.Streams.Stream_IO;

            File : File_Type;
         begin
            Mutools.Files.Open (Filename => "obj/ept_no_large_pages",
                                File     => File);
            Serialize (Stream      => Stream (File => File),
                       Mem_Layout  => Layout,
                       Serializers =>
                         (1 => EPT.Serialize_PML4'Access,
                          2 => EPT.Serialize_PDPT'Access,
                          3 => EPT.Serialize_PD'Access,
                          4 => EPT.Serialize_PT'Access));
            Close (File => File);
         end;

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/ept_no_large_pages.ref",
                  Filename2 => "obj/ept_no_large_pages"),
                 Message   => "EPT paging structures without large pages"
                 & " mismatch");
      end EPT_Generate_Paging_Structures_No_Large_Pages;

      ----------------------------------------------------------------------

      procedure EPT_Generate_Paging_Structures_Three_Levels
      is
         Layout : Memory_Layout_Type (Levels => 3);
      begin
         Set_Large_Page_Support (Mem_Layout => Layout,
                                 State      => False);
         Set_Address (Mem_Layout => Layout,
                      Address    => 16#001f_4000#);

         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#4000_0000#,
            Virtual_Address  => 16#0#,
            Size             => 16#0020_0000#,
            Caching          => UC,
            Writable         => True,
            Executable       => True);

         Update_References (Mem_Layout => Layout);

         declare
            use Ada.Streams.Stream_IO;

            File : File_Type;
         begin
            Mutools.Files.Open (Filename => "obj/ept_three_levels",
                                File     => File);
            Serialize (Stream      => Stream (File => File),
                       Mem_Layout  => Layout,
                       Serializers =>
                         (1 => EPT.Serialize_PDPT'Access,
                          2 => EPT.Serialize_PD'Access,
                          3 => EPT.Serialize_PT'Access));
            Close (File => File);
         end;

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/ept_three_levels.ref",
                  Filename2 => "obj/ept_three_levels"),
                 Message   => "Three level EPT paging structures"
                 & " mismatch");
      end EPT_Generate_Paging_Structures_Three_Levels;

      ----------------------------------------------------------------------

      procedure IA32e_Generate_Paging_Structures
      is
         Layout : Memory_Layout_Type := Null_Layout;
      begin
         Set_Address (Mem_Layout => Layout,
                      Address    => 16#20_0000#);

         --  Text
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#0010_0000#,
            Virtual_Address  => 16#0010_0000#,
            Size             => 16#0001_2000#,
            Caching          => UC,
            Writable         => True,
            Executable       => True);

         --  Stack
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#0011_2000#,
            Virtual_Address  => 16#0011_2000#,
            Size             => 16#2000#,
            Caching          => WB,
            Writable         => True,
            Executable       => False);

         --  Store
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#0011_6000#,
            Virtual_Address  => 16#0011_6000#,
            Size             => 16#1000#,
            Caching          => WB,
            Writable         => True,
            Executable       => False);

         --  Data
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#0011_8000#,
            Virtual_Address  => 16#0011_8000#,
            Size             => 16#6000#,
            Caching          => UC,
            Writable         => True,
            Executable       => True);

         --  Tau0 interface
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#001f_f000#,
            Virtual_Address  => 16#001f_f000#,
            Size             => 16#1000#,
            Caching          => UC,
            Writable         => False,
            Executable       => False);

         Update_References (Mem_Layout => Layout);

         declare
            use Ada.Streams.Stream_IO;

            File : File_Type;
         begin
            Mutools.Files.Open (Filename => "obj/ia32e",
                                File     => File);
            Serialize (Stream      => Stream (File => File),
                       Mem_Layout  => Layout,
                       Serializers =>
                         (1 => IA32e.Serialize_PML4'Access,
                          2 => IA32e.Serialize_PDPT'Access,
                          3 => IA32e.Serialize_PD'Access,
                          4 => IA32e.Serialize_PT'Access));
            Close (File => File);
         end;

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/ia32e.ref",
                  Filename2 => "obj/ia32e"),
                 Message   => "IA-32e paging structures mismatch");
      end IA32e_Generate_Paging_Structures;

      ----------------------------------------------------------------------

      procedure IA32e_Generate_Multiple_PTs
      is
         Layout : Memory_Layout_Type := Null_Layout;
      begin
         Set_Address (Mem_Layout => Layout,
                      Address    => 16#20_0000#);

         --  Entry 0 in PT 0.
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#0000#,
            Virtual_Address  => 16#0000#,
            Size             => 16#1000#,
            Caching          => WB,
            Writable         => True,
            Executable       => False);

         --  Entry 50 in PT 53.
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#06a8_0000#,
            Virtual_Address  => 16#06a8_0000#,
            Size             => 16#1000#,
            Caching          => WC,
            Writable         => True,
            Executable       => True);

         --  Entry 511 in PT 511.
         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#3fff_f000#,
            Virtual_Address  => 16#3fff_f000#,
            Size             => 16#1000#,
            Caching          => UC,
            Writable         => False,
            Executable       => True);

         Update_References (Mem_Layout => Layout);

         declare
            use Ada.Streams.Stream_IO;

            File : File_Type;
         begin
            Mutools.Files.Open (Filename => "obj/ia32e_multi_pt",
                                File     => File);
            Serialize (Stream      => Stream (File => File),
                       Mem_Layout  => Layout,
                       Serializers =>
                         (1 => IA32e.Serialize_PML4'Access,
                          2 => IA32e.Serialize_PDPT'Access,
                          3 => IA32e.Serialize_PD'Access,
                          4 => IA32e.Serialize_PT'Access));
            Close (File => File);
         end;

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/ia32e_multi_pt.ref",
                  Filename2 => "obj/ia32e_multi_pt"),
                 Message   => "IA-32e multiple PTs mismatch");
      end Ia32e_Generate_Multiple_Pts;

      ----------------------------------------------------------------------

      procedure IA32e_Generate_Multiple_Structures
      is
         Layout : Memory_Layout_Type := Null_Layout;
      begin
         Set_Address (Mem_Layout => Layout,
                      Address    => 16#1_0000#);

         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#001f_f000#,
            Virtual_Address  => 16#001f_f000#,
            Size             => 16#0040_2000#,
            Caching          => WB,
            Writable         => True,
            Executable       => False);

         Add_Memory_Region
           (Mem_Layout       => Layout,
            Physical_Address => 16#4000_0000#,
            Virtual_Address  => 16#4000_0000#,
            Size             => 16#4000_0000#,
            Caching          => UC,
            Writable         => False,
            Executable       => True);

         Update_References (Mem_Layout => Layout);

         declare
            use Ada.Streams.Stream_IO;

            File : File_Type;
         begin
            Mutools.Files.Open (Filename => "obj/ia32e_multi",
                                File     => File);
            Serialize (Stream      => Stream (File => File),
                       Mem_Layout  => Layout,
                       Serializers =>
                         (1 => IA32e.Serialize_PML4'Access,
                          2 => IA32e.Serialize_PDPT'Access,
                          3 => IA32e.Serialize_PD'Access,
                          4 => IA32e.Serialize_PT'Access));
            Close (File => File);
         end;

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/ia32e_multi.ref",
                  Filename2 => "obj/ia32e_multi"),
                 Message   => "IA-32e multiple paging structures mismatch");
      end IA32e_Generate_Multiple_Structures;

      ----------------------------------------------------------------------

      procedure Serialize_Empty_Layout
      is
         use type Ada.Streams.Stream_Element_Offset;

         Mem_Stream : aliased Memory_Stream_Type;
      begin
         Serialize (Stream      => Mem_Stream'Access,
                    Mem_Layout  => Null_Layout,
                    Serializers => (1 => IA32e.Serialize_PML4'Access,
                                    2 => IA32e.Serialize_PDPT'Access,
                                    3 => IA32e.Serialize_PD'Access,
                                    4 => IA32e.Serialize_PT'Access));

         --  Serializing an empty layout should not generate any output.

         Assert (Condition => Mem_Stream.Write_Idx = 1,
                 Message   => "Serialized null layout mismatch");
      end Serialize_Empty_Layout;
   begin
      EPT_Generate_Paging_Structures;
      EPT_Generate_Paging_Structures_No_Large_Pages;
      EPT_Generate_Paging_Structures_Three_Levels;
      IA32e_Generate_Paging_Structures;
      IA32e_Generate_Multiple_Structures;
      IA32e_Generate_Multiple_PTs;
      Serialize_Empty_Layout;
--  begin read only
   end Test_Serialize;
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
end Paging.Layouts.Test_Data.Tests;
