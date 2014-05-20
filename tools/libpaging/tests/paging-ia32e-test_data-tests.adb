--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.IA32e.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Paging.IA32e.Test_Data.Tests is


--  begin read only
   procedure Test_1_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_0929b4 (Gnattest_T : in out Test) renames Test_1_Serialize;
--  id:2.2/0929b4c278710f34/Serialize/1/0/
   procedure Test_1_Serialize (Gnattest_T : in out Test) is
   --  paging-ia32e.ads:28:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      PML4 : Tables.PML4.Page_Table_Type;
   begin
      Tables.PML4.Set_Physical_Address (Table   => PML4,
                                        Address => 16#1f0000#);
      Tables.PML4.Add_Entry (Table => PML4,
                             Index => 0,
                             E     => Entries.Create
                               (Dst_Offset  => 0,
                                Dst_Address => 16#1f1000#,
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
         Mutools.Files.Open (Filename => "obj/ia32e_pml4",
                             File     => File);

         Serialize (Stream => Stream (File => File),
                    PML4   => PML4);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ia32e_pml4.ref",
               Filename2 => "obj/ia32e_pml4"),
              Message   => "IA-32e PML4 table mismatch");
--  begin read only
   end Test_1_Serialize;
--  end read only


--  begin read only
   procedure Test_2_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_5b10db (Gnattest_T : in out Test) renames Test_2_Serialize;
--  id:2.2/5b10db2ed3f164c2/Serialize/0/0/
   procedure Test_2_Serialize (Gnattest_T : in out Test) is
   --  paging-ia32e.ads:34:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      PDPT : Tables.PDPT.Page_Table_Type;
   begin
      Tables.PDPT.Set_Physical_Address (Table   => PDPT,
                                        Address => 16#1f1000#);
      Tables.PDPT.Add_Entry (Table => PDPT,
                             Index => 0,
                             E     => Entries.Create
                               (Dst_Offset  => 0,
                                Dst_Address => 16#1f2000#,
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

         Serialize (Stream => Stream (File => File),
                    PDPT   => PDPT);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ia32e_pdpt.ref",
               Filename2 => "obj/ia32e_pdpt"),
              Message   => "IA-32e PDP table mismatch");
--  begin read only
   end Test_2_Serialize;
--  end read only


--  begin read only
   procedure Test_3_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_16bab4 (Gnattest_T : in out Test) renames Test_3_Serialize;
--  id:2.2/16bab447958518fa/Serialize/0/0/
   procedure Test_3_Serialize (Gnattest_T : in out Test) is
   --  paging-ia32e.ads:40:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      PD : Tables.PD.Page_Table_Type;
   begin
      Tables.PD.Set_Physical_Address (Table   => PD,
                                      Address => 16#1f2000#);
      Tables.PD.Add_Entry (Table => PD,
                           Index => 0,
                           E     => Entries.Create
                             (Dst_Offset  => 0,
                              Dst_Address => 16#1f3000#,
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

         Serialize (Stream => Stream (File => File),
                    PD     => PD);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ia32e_pd.ref",
               Filename2 => "obj/ia32e_pd"),
              Message   => "IA-32e page directory mismatch");
--  begin read only
   end Test_3_Serialize;
--  end read only


--  begin read only
   procedure Test_4_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_6a5ffb (Gnattest_T : in out Test) renames Test_4_Serialize;
--  id:2.2/6a5ffbf13c87f30d/Serialize/0/0/
   procedure Test_4_Serialize (Gnattest_T : in out Test) is
   --  paging-ia32e.ads:46:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      PT : Tables.PT.Page_Table_Type;
   begin
      Tables.PT.Set_Physical_Address (Table   => PT,
                                      Address => 16#1f3000#);
      Tables.PT.Add_Entry (Table => PT,
                           Index => 0,
                           E     => Entries.Create
                             (Dst_Offset  => 0,
                              Dst_Address => 16#240000#,
                              Readable    => True,
                              Writable    => True,
                              Executable  => True,
                              Maps_Page   => False,
                              Global      => False,
                              Caching     => WB));
      Tables.PT.Add_Entry (Table => PT,
                           Index => 256,
                           E     => Entries.Create
                             (Dst_Offset  => 0,
                              Dst_Address => 16#1ff000#,
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

         Serialize (Stream => Stream (File => File),
                    PT     => PT);
         Close (File => File);
      end;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ia32e_pt.ref",
               Filename2 => "obj/ia32e_pt"),
              Message   => "IA-32e page table mismatch");
--  begin read only
   end Test_4_Serialize;
--  end read only

end Paging.IA32e.Test_Data.Tests;
