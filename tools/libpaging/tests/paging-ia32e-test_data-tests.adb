--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.IA32e.Test_Data.

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
package body Paging.IA32e.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Serialize_PML4 (Gnattest_T : in out Test);
   procedure Test_Serialize_PML4_2c71ff (Gnattest_T : in out Test) renames Test_Serialize_PML4;
--  id:2.2/2c71ff1918c64f4a/Serialize_PML4/1/0/
   procedure Test_Serialize_PML4 (Gnattest_T : in out Test) is
   --  paging-ia32e.ads:28:4:Serialize_PML4
--  end read only

      pragma Unreferenced (Gnattest_T);

      PML4 : Tables.Page_Table_Type;
   begin
      Tables.Set_Physical_Address (Table   => PML4,
                                   Address => 16#001f_0000#);
      Tables.Add_Entry (Table => PML4,
                        Index => 0,
                        E     => Ref_PML4_Entry);

      declare
         use Ada.Streams.Stream_IO;

         File : File_Type;
      begin
         Mutools.Files.Open (Filename => "obj/ia32e_pml4",
                             File     => File);
         Serialize_PML4 (Stream => Stream (File => File),
                         Table  => PML4);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ia32e_pml4.ref",
               Filename2 => "obj/ia32e_pml4"),
              Message   => "IA-32e PML4 table mismatch");
--  begin read only
   end Test_Serialize_PML4;
--  end read only


--  begin read only
   procedure Test_Serialize_PDPT (Gnattest_T : in out Test);
   procedure Test_Serialize_PDPT_94a8de (Gnattest_T : in out Test) renames Test_Serialize_PDPT;
--  id:2.2/94a8de34a628967f/Serialize_PDPT/1/0/
   procedure Test_Serialize_PDPT (Gnattest_T : in out Test) is
   --  paging-ia32e.ads:34:4:Serialize_PDPT
--  end read only

      pragma Unreferenced (Gnattest_T);

      PDPT : Tables.Page_Table_Type;
   begin
      Tables.Set_Physical_Address (Table   => PDPT,
                                   Address => 16#001f_1000#);
      Tables.Add_Entry (Table => PDPT,
                        Index => 0,
                        E     => Entries.Create
                          (Dst_Index   => 0,
                           Dst_Address => 16#001f_2000#,
                           Readable    => True,
                           Writable    => True,
                           Executable  => True,
                           Maps_Page   => False,
                           Global      => False,
                           Caching     => UC));

      declare
         use Ada.Streams.Stream_IO;

         File : File_Type;
      begin
         Mutools.Files.Open (Filename => "obj/ia32e_pdpt",
                             File     => File);

         Serialize_PDPT (Stream => Stream (File => File),
                         Table  => PDPT);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ia32e_pdpt.ref",
               Filename2 => "obj/ia32e_pdpt"),
              Message   => "IA-32e PDP table mismatch");
--  begin read only
   end Test_Serialize_PDPT;
--  end read only


--  begin read only
   procedure Test_Serialize_PD (Gnattest_T : in out Test);
   procedure Test_Serialize_PD_8965c8 (Gnattest_T : in out Test) renames Test_Serialize_PD;
--  id:2.2/8965c843086bc1ea/Serialize_PD/1/0/
   procedure Test_Serialize_PD (Gnattest_T : in out Test) is
   --  paging-ia32e.ads:40:4:Serialize_PD
--  end read only

      pragma Unreferenced (Gnattest_T);

      PD : Tables.Page_Table_Type;
   begin
      Tables.Set_Physical_Address (Table   => PD,
                                   Address => 16#001f_2000#);
      Tables.Add_Entry (Table => PD,
                        Index => 0,
                        E     => Entries.Create
                          (Dst_Index   => 0,
                           Dst_Address => 16#001f_3000#,
                           Readable    => True,
                           Writable    => True,
                           Executable  => True,
                           Maps_Page   => False,
                           Global      => False,
                           Caching     => UC));

      declare
         use Ada.Streams.Stream_IO;

         File : File_Type;
      begin
         Mutools.Files.Open (Filename => "obj/ia32e_pd",
                             File     => File);

         Serialize_PD (Stream => Stream (File => File),
                       Table  => PD);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ia32e_pd.ref",
               Filename2 => "obj/ia32e_pd"),
              Message   => "IA-32e page directory mismatch");
--  begin read only
   end Test_Serialize_PD;
--  end read only


--  begin read only
   procedure Test_Serialize_PT (Gnattest_T : in out Test);
   procedure Test_Serialize_PT_21f341 (Gnattest_T : in out Test) renames Test_Serialize_PT;
--  id:2.2/21f3412381d84015/Serialize_PT/1/0/
   procedure Test_Serialize_PT (Gnattest_T : in out Test) is
   --  paging-ia32e.ads:46:4:Serialize_PT
--  end read only

      pragma Unreferenced (Gnattest_T);

      PT : Tables.Page_Table_Type;
   begin
      Tables.Set_Physical_Address (Table   => PT,
                                   Address => 16#001f_3000#);
      Tables.Add_Entry (Table => PT,
                        Index => 0,
                        E     => Entries.Create
                          (Dst_Index   => 0,
                           Dst_Address => 16#0024_0000#,
                           Readable    => True,
                           Writable    => True,
                           Executable  => True,
                           Maps_Page   => False,
                           Global      => False,
                           Caching     => WB));
      Tables.Add_Entry (Table => PT,
                        Index => 256,
                        E     => Entries.Create
                          (Dst_Index   => 0,
                           Dst_Address => 16#001f_f000#,
                           Readable    => True,
                           Writable    => False,
                           Executable  => False,
                           Maps_Page   => False,
                           Global      => False,
                           Caching     => UC));

      declare
         use Ada.Streams.Stream_IO;

         File : File_Type;
      begin
         Mutools.Files.Open (Filename => "obj/ia32e_pt",
                             File     => File);

         Serialize_PT (Stream => Stream (File => File),
                       Table  => PT);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ia32e_pt.ref",
               Filename2 => "obj/ia32e_pt"),
              Message   => "IA-32e page table mismatch");
--  begin read only
   end Test_Serialize_PT;
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
end Paging.IA32e.Test_Data.Tests;
