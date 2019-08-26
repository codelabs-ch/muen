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
--  end read only

      pragma Unreferenced (Gnattest_T);

      PDPT : Tables.Page_Table_Type;
   begin
      Tables.Set_Physical_Address (Table   => PDPT,
                                   Address => 16#001f_1000#);
      Tables.Add_Entry (Table => PDPT,
                        Index => 0,
                        E     => Ref_PDPT_Entry);

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
--  end read only

      pragma Unreferenced (Gnattest_T);

      PD : Tables.Page_Table_Type;
   begin
      Tables.Set_Physical_Address (Table   => PD,
                                   Address => 16#001f_2000#);
      Tables.Add_Entry (Table => PD,
                        Index => 0,
                        E     => Ref_PD_Entry);

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
--  end read only

      pragma Unreferenced (Gnattest_T);

      PT : Tables.Page_Table_Type;
   begin
      Tables.Set_Physical_Address (Table   => PT,
                                   Address => 16#001f_3000#);
      Tables.Add_Entry (Table => PT,
                        Index => 0,
                        E     => Ref_PT_Entry_0);
      Tables.Add_Entry (Table => PT,
                        Index => 256,
                        E     => Ref_PT_Entry_256);

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
   procedure Test_Deserialze_PML4_Entry (Gnattest_T : in out Test);
   procedure Test_Deserialze_PML4_Entry_9da287 (Gnattest_T : in out Test) renames Test_Deserialze_PML4_Entry;
--  id:2.2/9da2878c857c6e24/Deserialze_PML4_Entry/1/0/
   procedure Test_Deserialze_PML4_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (File => File,
         Mode => Ada.Streams.Stream_IO.In_File,
         Name => "data/ia32e_pml4.ref");

      declare
         use type Entries.Table_Entry_Type;

         PML4_Entry : Entries.Table_Entry_Type;
      begin
         Deserialze_PML4_Entry (Stream      => Stream (File => File),
                                Table_Entry => PML4_Entry);
         Close (File => File);

         Assert (Condition => PML4_Entry = Ref_PML4_Entry,
                 Message   => "Deserialized PML4 entry mismatch");

      exception
         when others =>
            if Is_Open (File => File) then
               Close (File => File);
            end if;
            raise;
      end;
--  begin read only
   end Test_Deserialze_PML4_Entry;
--  end read only


--  begin read only
   procedure Test_Deserialze_PDPT_Entry (Gnattest_T : in out Test);
   procedure Test_Deserialze_PDPT_Entry_f53807 (Gnattest_T : in out Test) renames Test_Deserialze_PDPT_Entry;
--  id:2.2/f5380744c07dff21/Deserialze_PDPT_Entry/1/0/
   procedure Test_Deserialze_PDPT_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (File => File,
         Mode => Ada.Streams.Stream_IO.In_File,
         Name => "data/ia32e_pdpt.ref");

      declare
         use type Entries.Table_Entry_Type;

         PDPT_Entry : Entries.Table_Entry_Type;
      begin
         Deserialze_PDPT_Entry (Stream      => Stream (File => File),
                                Table_Entry => PDPT_Entry);
         Close (File => File);

         Assert (Condition => PDPT_Entry = Ref_PDPT_Entry,
                 Message   => "Deserialized PDPT entry mismatch");

      exception
         when others =>
            if Is_Open (File => File) then
               Close (File => File);
            end if;
            raise;
      end;
--  begin read only
   end Test_Deserialze_PDPT_Entry;
--  end read only


--  begin read only
   procedure Test_Deserialze_PD_Entry (Gnattest_T : in out Test);
   procedure Test_Deserialze_PD_Entry_cdf777 (Gnattest_T : in out Test) renames Test_Deserialze_PD_Entry;
--  id:2.2/cdf7771059204699/Deserialze_PD_Entry/1/0/
   procedure Test_Deserialze_PD_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (File => File,
         Mode => Ada.Streams.Stream_IO.In_File,
         Name => "data/ia32e_pd.ref");

      declare
         use type Entries.Table_Entry_Type;

         PD_Entry : Entries.Table_Entry_Type;
      begin
         Deserialze_PD_Entry (Stream      => Stream (File => File),
                              Table_Entry => PD_Entry);
         Close (File => File);

         Assert (Condition => PD_Entry = Ref_PD_Entry,
                 Message   => "Deserialized PD entry mismatch");

      exception
         when others =>
            if Is_Open (File => File) then
               Close (File => File);
            end if;
            raise;
      end;
--  begin read only
   end Test_Deserialze_PD_Entry;
--  end read only


--  begin read only
   procedure Test_Deserialze_PT_Entry (Gnattest_T : in out Test);
   procedure Test_Deserialze_PT_Entry_3a3c51 (Gnattest_T : in out Test) renames Test_Deserialze_PT_Entry;
--  id:2.2/3a3c5176bca7d21e/Deserialze_PT_Entry/1/0/
   procedure Test_Deserialze_PT_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Open (File => File,
            Mode => In_File,
            Name => "data/ia32e_pt.ref");

      declare
         use type Entries.Table_Entry_Type;

         PT_Entry : Entries.Table_Entry_Type;
      begin
         Deserialze_PT_Entry (Stream      => Stream (File => File),
                              Table_Entry => PT_Entry);
         Assert (Condition => PT_Entry = Ref_PT_Entry_0,
                 Message   => "Deserialized PT entry 0 mismatch");

         Set_Index (File => File,
                    To   => 256 * 8 + 1);
         Deserialze_PT_Entry (Stream      => Stream (File => File),
                              Table_Entry => PT_Entry);
         Close (File => File);

         Assert (Condition => PT_Entry = Ref_PT_Entry_256,
                 Message   => "Deserialized PT entry 256 mismatch");

      exception
         when others =>
            if Is_Open (File => File) then
               Close (File => File);
            end if;
            raise;
      end;
--  begin read only
   end Test_Deserialze_PT_Entry;
--  end read only


--  begin read only
   procedure Test_Cache_Mapping (Gnattest_T : in out Test);
   procedure Test_Cache_Mapping_c80d4a (Gnattest_T : in out Test) renames Test_Cache_Mapping;
--  id:2.2/c80d4a6401bc7d6a/Cache_Mapping/1/0/
   procedure Test_Cache_Mapping (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Cache_Mapping (IA32E_Mem_Type => 0) = WB,
              Message   => "WB caching mismatch");
      Assert (Condition => Cache_Mapping (IA32E_Mem_Type => 1) = WT,
              Message   => "WT caching mismatch");
      Assert (Condition => Cache_Mapping (IA32E_Mem_Type => 2) = WC,
              Message   => "WC caching mismatch");
      Assert (Condition => Cache_Mapping (IA32E_Mem_Type => 3) = UC,
              Message   => "UC caching mismatch");
      Assert (Condition => Cache_Mapping (IA32E_Mem_Type => 4) = WP,
              Message   => "WP caching mismatch");

      declare
         Dummy : Caching_Type;
      begin
         Dummy := Cache_Mapping (IA32E_Mem_Type => 5);
         Assert (Condition => True,
                 Message   => "Exception expected");

      exception
         when E : Constraint_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid IA32-e memory type: 5",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_Cache_Mapping;
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
