--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.EPT.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Paging.EPT.Test_Data.Tests is


--  begin read only
   procedure Test_1_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_0929b4 (Gnattest_T : in out Test) renames Test_1_Serialize;
--  id:2.2/0929b4c278710f34/Serialize/1/0/
   procedure Test_1_Serialize (Gnattest_T : in out Test) is
   --  paging-ept.ads:30:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      PML4 : Tables.PML4.Page_Table_Type;
   begin
      Tables.PML4.Set_Physical_Address (Table   => PML4,
                                        Address => 16#1f4000#);
      Tables.PML4.Add_Entry (Table => PML4,
                             Index => 0,
                             E     => Entries.Create
                               (Dst_Offset  => 0,
                                Dst_Address => 16#1f5000#,
                                Readable    => True,
                                Writable    => True,
                                Executable  => True,
                                Maps_Page   => False,
                                Global      => False,
                                Caching     => WC));

      declare
         use Ada.Streams.Stream_IO;

         File : File_Type;
      begin
         Mutools.Files.Open (Filename => "obj/ept_pml4",
                             File     => File);

         Serialize (Stream => Stream (File => File),
                    PML4   => PML4);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ept_pml4.ref",
               Filename2 => "obj/ept_pml4"),
              Message   => "EPT PML4 table mismatch");
--  begin read only
   end Test_1_Serialize;
--  end read only


--  begin read only
   procedure Test_2_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_5b10db (Gnattest_T : in out Test) renames Test_2_Serialize;
--  id:2.2/5b10db2ed3f164c2/Serialize/0/0/
   procedure Test_2_Serialize (Gnattest_T : in out Test) is
   --  paging-ept.ads:34:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      PDPT : Tables.PDPT.Page_Table_Type;
   begin
      Tables.PDPT.Set_Physical_Address (Table   => PDPT,
                                        Address => 16#1f5000#);
      Tables.PDPT.Add_Entry (Table => PDPT,
                             Index => 0,
                             E     => Entries.Create
                               (Dst_Offset  => 0,
                                Dst_Address => 16#4000_0000#,
                                Readable    => True,
                                Writable    => True,
                                Executable  => True,
                                Maps_Page   => True,
                                Global      => False,
                                Caching     => UC));
      Tables.PDPT.Add_Entry (Table => PDPT,
                             Index => 1,
                             E     => Entries.Create
                               (Dst_Offset  => 0,
                                Dst_Address => 16#8000_0000#,
                                Readable    => True,
                                Writable    => True,
                                Executable  => True,
                                Maps_Page   => True,
                                Global      => False,
                                Caching     => UC));
      Tables.PDPT.Add_Entry (Table => PDPT,
                             Index => 2,
                             E     => Entries.Create
                               (Dst_Offset  => 0,
                                Dst_Address => 16#c000_0000#,
                                Readable    => True,
                                Writable    => True,
                                Executable  => True,
                                Maps_Page   => True,
                                Global      => False,
                                Caching     => UC));
      Tables.PDPT.Add_Entry (Table => PDPT,
                             Index => 3,
                             E     => Entries.Create
                               (Dst_Offset  => 0,
                                Dst_Address => 16#1_0000_0000#,
                                Readable    => True,
                                Writable    => True,
                                Executable  => True,
                                Maps_Page   => True,
                                Global      => False,
                                Caching     => UC));

      declare
         use Ada.Streams.Stream_IO;

         File : File_Type;
      begin
         Mutools.Files.Open (Filename => "obj/ept_pdpt",
                             File     => File);

         Serialize (Stream => Stream (File => File),
                    PDPT   => PDPT);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ept_pdpt.ref",
               Filename2 => "obj/ept_pdpt"),
              Message   => "EPT PDPT table mismatch");
--  begin read only
   end Test_2_Serialize;
--  end read only


--  begin read only
   procedure Test_3_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_16bab4 (Gnattest_T : in out Test) renames Test_3_Serialize;
--  id:2.2/16bab447958518fa/Serialize/0/0/
   procedure Test_3_Serialize (Gnattest_T : in out Test) is
   --  paging-ept.ads:38:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      PD : Tables.PD.Page_Table_Type;
   begin
      Tables.PD.Set_Physical_Address (Table   => PD,
                                      Address => 16#1f6000#);
      Tables.PD.Add_Entry (Table => PD,
                           Index => 0,
                           E     => Entries.Create
                             (Dst_Offset  => 0,
                              Dst_Address => 16#1f7000#,
                              Readable    => True,
                              Writable    => True,
                              Executable  => True,
                              Maps_Page   => False,
                              Global      => False,
                              Caching     => WC));

      declare
         use Ada.Streams.Stream_IO;

         File : File_Type;
      begin
         Mutools.Files.Open (Filename => "obj/ept_pd",
                             File     => File);

         Serialize (Stream => Stream (File => File),
                    PD     => PD);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ept_pd.ref",
               Filename2 => "obj/ept_pd"),
              Message   => "EPT page directory mismatch");
--  begin read only
   end Test_3_Serialize;
--  end read only


--  begin read only
   procedure Test_4_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_6a5ffb (Gnattest_T : in out Test) renames Test_4_Serialize;
--  id:2.2/6a5ffbf13c87f30d/Serialize/0/0/
   procedure Test_4_Serialize (Gnattest_T : in out Test) is
   --  paging-ept.ads:42:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      PT : Tables.PT.Page_Table_Type;
   begin
      Tables.PT.Set_Physical_Address (Table   => PT,
                                      Address => 16#1f7000#);
      Tables.PT.Add_Entry (Table => PT,
                           Index => 184,
                           E     => Entries.Create
                             (Dst_Offset  => 0,
                              Dst_Address => 16#000b_8000#,
                              Readable    => True,
                              Writable    => True,
                              Executable  => False,
                              Maps_Page   => True,
                              Global      => False,
                              Caching     => WC));

      declare
         use Ada.Streams.Stream_IO;

         File : File_Type;
      begin
         Mutools.Files.Open (Filename => "obj/ept_pt",
                             File     => File);

         Serialize (Stream => Stream (File => File),
                    PT     => PT);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ept_pt.ref",
               Filename2 => "obj/ept_pt"),
              Message   => "EPT page table mismatch");
--  begin read only
   end Test_4_Serialize;
--  end read only


--  begin read only
   procedure Test_Serialize_PML4 (Gnattest_T : in out Test);
   procedure Test_Serialize_PML4_e35f5c (Gnattest_T : in out Test) renames Test_Serialize_PML4;
--  id:2.2/e35f5ceb25bfba78/Serialize_PML4/1/0/
   procedure Test_Serialize_PML4 (Gnattest_T : in out Test) is
   --  paging-ept.ads:46:4:Serialize_PML4
--  end read only

      pragma Unreferenced (Gnattest_T);

      PML4 : Pagetables.Page_Table_Type;
   begin
      Pagetables.Set_Physical_Address (Table   => PML4,
                                       Address => 16#1f4000#);
      Pagetables.Add_Entry (Table => PML4,
                            Index => 0,
                            E     => Entries.Create
                              (Dst_Offset  => 0,
                               Dst_Address => 16#1f5000#,
                               Readable    => True,
                               Writable    => True,
                               Executable  => True,
                               Maps_Page   => False,
                               Global      => False,
                               Caching     => WC));

      declare
         use Ada.Streams.Stream_IO;

         File : File_Type;
      begin
         Mutools.Files.Open (Filename => "obj/ept_pml4",
                             File     => File);
         Serialize_PML4 (Stream => Stream (File => File),
                         Table  => PML4);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ept_pml4.ref",
               Filename2 => "obj/ept_pml4"),
              Message   => "EPT PML4 table mismatch");
--  begin read only
   end Test_Serialize_PML4;
--  end read only

end Paging.EPT.Test_Data.Tests;
