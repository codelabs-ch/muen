--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Memory.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Paging.Memory.Test_Data.Tests is


--  begin read only
   procedure Test_Set_Address (Gnattest_T : in out Test);
   procedure Test_Set_Address_ab20a9 (Gnattest_T : in out Test) renames Test_Set_Address;
--  id:2.2/ab20a94a93cb5f28/Set_Address/1/0/
   procedure Test_Set_Address (Gnattest_T : in out Test) is
   --  paging-memory.ads:35:4:Set_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Ref_Addr : constant Interfaces.Unsigned_64 := 16#deadbeef000#;
      Layout   : Memory_Layout_Type;
   begin
      Set_Address (Mem_Layout => Layout,
                   Address    => Ref_Addr);

      Assert (Condition => Get_Address (Mem_Layout => Layout) = Ref_Addr,
              Message   => "Layout address mismatch");
--  begin read only
   end Test_Set_Address;
--  end read only


--  begin read only
   procedure Test_Get_Address (Gnattest_T : in out Test);
   procedure Test_Get_Address_d6d3ab (Gnattest_T : in out Test) renames Test_Get_Address;
--  id:2.2/d6d3abd64332bc20/Get_Address/1/0/
   procedure Test_Get_Address (Gnattest_T : in out Test) is
   --  paging-memory.ads:40:4:Get_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Already tested in Test_Set_Address");
--  begin read only
   end Test_Get_Address;
--  end read only


--  begin read only
   procedure Test_Get_Table_Count (Gnattest_T : in out Test);
   procedure Test_Get_Table_Count_7ace11 (Gnattest_T : in out Test) renames Test_Get_Table_Count;
--  id:2.2/7ace1103ac91ec93/Get_Table_Count/1/0/
   procedure Test_Get_Table_Count (Gnattest_T : in out Test) is
   --  paging-memory.ads:45:4:Get_Table_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

      Layout : Memory_Layout_Type := Null_Layout;

      PML4s, PDPTs, PDs, PTs : Natural;
   begin
      Get_Table_Count (Mem_Layout => Layout,
                       PML4_Count => PML4s,
                       PDPT_Count => PDPTs,
                       PD_Count   => PDs,
                       PT_Count   => PTs);
      Assert (Condition => PML4s = 0,
              Message   => "PML4 count not 0");
      Assert (Condition => PDPTs = 0,
              Message   => "PDPT count not 0");
      Assert (Condition => PDs = 0,
              Message   => "PD count not 0");
      Assert (Condition => PTs = 0,
              Message   => "PT count not 0");
--  begin read only
   end Test_Get_Table_Count;
--  end read only


--  begin read only
   procedure Test_Add_Memory_Region (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Region_dc872f (Gnattest_T : in out Test) renames Test_Add_Memory_Region;
--  id:2.2/dc872fffa6fef812/Add_Memory_Region/1/0/
   procedure Test_Add_Memory_Region (Gnattest_T : in out Test) is
   --  paging-memory.ads:53:4:Add_Memory_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

      Layout : Memory_Layout_Type := Null_Layout;

      PML4s, PDPTs, PDs, PTs : Natural;
   begin
      Add_Memory_Region
        (Mem_Layout       => Layout,
         Physical_Address => 16#1000#,
         Virtual_Address  => 16#deafbeef000#,
         Size             => Page_Size,
         Caching          => WB,
         Writable         => True,
         Executable       => False);

      Get_Table_Count (Mem_Layout => Layout,
                       PML4_Count => PML4s,
                       PDPT_Count => PDPTs,
                       PD_Count   => PDs,
                       PT_Count   => PTs);
      Assert (Condition => PML4s = 1,
              Message   => "PML4 count not 1");
      Assert (Condition => PDPTs = 1,
              Message   => "PDPT count not 1");
      Assert (Condition => PDs = 1,
              Message   => "PD count not 1");
      Assert (Condition => PTs = 1,
              Message   => "PT count not 1");
--  begin read only
   end Test_Add_Memory_Region;
--  end read only


--  begin read only
   procedure Test_Update_References (Gnattest_T : in out Test);
   procedure Test_Update_References_aa5caf (Gnattest_T : in out Test) renames Test_Update_References;
--  id:2.2/aa5caf74f385848c/Update_References/1/0/
   procedure Test_Update_References (Gnattest_T : in out Test) is
   --  paging-memory.ads:64:4:Update_References
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Test_Serialize");
--  begin read only
   end Test_Update_References;
--  end read only


--  begin read only
   procedure Test_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_bf151f (Gnattest_T : in out Test) renames Test_Serialize;
--  id:2.2/bf151f63d6dffd0a/Serialize/1/0/
   procedure Test_Serialize (Gnattest_T : in out Test) is
   --  paging-memory.ads:69:4:Serialize
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

            Serialize
              (Stream         => Stream (File => File),
               Mem_Layout     => Layout,
               Serialize_PML4 => EPT.Serialize'Access,
               Serialize_PDPT => EPT.Serialize'Access,
               Serialize_PD   => EPT.Serialize'Access,
               Serialize_PT   => EPT.Serialize'Access);

            Close (File => File);
         end;

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/ept.ref",
                  Filename2 => "obj/ept"),
                 Message   => "EPT paging structures mismatch");
      end EPT_Generate_Paging_Structures;

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

            Serialize
              (Stream         => Stream (File => File),
               Mem_Layout     => Layout,
               Serialize_PML4 => IA32e.Serialize'Access,
               Serialize_PDPT => IA32e.Serialize'Access,
               Serialize_PD   => IA32e.Serialize'Access,
               Serialize_PT   => IA32e.Serialize'Access);

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

            Serialize
              (Stream         => Stream (File => File),
               Mem_Layout     => Layout,
               Serialize_PML4 => IA32e.Serialize'Access,
               Serialize_PDPT => IA32e.Serialize'Access,
               Serialize_PD   => IA32e.Serialize'Access,
               Serialize_PT   => IA32e.Serialize'Access);

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

            Serialize
              (Stream         => Stream (File => File),
               Mem_Layout     => Layout,
               Serialize_PML4 => IA32e.Serialize'Access,
               Serialize_PDPT => IA32e.Serialize'Access,
               Serialize_PD   => IA32e.Serialize'Access,
               Serialize_PT   => IA32e.Serialize'Access);

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
         Serialize
           (Stream         => Mem_Stream'Access,
            Mem_Layout     => Null_Layout,
            Serialize_PML4 => IA32e.Serialize'Access,
            Serialize_PDPT => IA32e.Serialize'Access,
            Serialize_PD   => IA32e.Serialize'Access,
            Serialize_PT   => IA32e.Serialize'Access);

         --  Serializing an empty layout should not generate any output.

         Assert (Condition => Mem_Stream.Write_Idx = 1,
                 Message   => "Serialized null layout mismatch");
      end Serialize_Empty_Layout;
   begin
      EPT_Generate_Paging_Structures;
      IA32e_Generate_Paging_Structures;
      IA32e_Generate_Multiple_Structures;
      IA32e_Generate_Multiple_PTs;
      Serialize_Empty_Layout;
--  begin read only
   end Test_Serialize;
--  end read only


--  begin read only
   procedure Test_Set_Large_Page_Support (Gnattest_T : in out Test);
   procedure Test_Set_Large_Page_Support_f61057 (Gnattest_T : in out Test) renames Test_Set_Large_Page_Support;
--  id:2.2/f61057a95eba2569/Set_Large_Page_Support/1/0/
   procedure Test_Set_Large_Page_Support (Gnattest_T : in out Test) is
   --  paging-memory.ads:86:4:Set_Large_Page_Support
--  end read only

      pragma Unreferenced (Gnattest_T);

      Layout : Memory_Layout_Type;
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

end Paging.Memory.Test_Data.Tests;
