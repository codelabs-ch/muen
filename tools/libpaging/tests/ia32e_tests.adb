--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Streams.Stream_IO;

with Interfaces;

with Test_Utils;

with Mutools.Files;

with Paging.Tables;
with Paging.Entries;
with Paging.IA32e;

package body IA32e_Tests
is

   use Ahven;
   use Paging;
   use Paging.IA32e;
   use type Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "IA-32e paging tests");
      T.Add_Test_Routine
        (Routine => PML4E_To_Unsigned64'Access,
         Name    => "PML4E to unsigned 64");
      T.Add_Test_Routine
        (Routine => PDPTE_To_Unsigned64'Access,
         Name    => "PDPTE to unsigned 64");
      T.Add_Test_Routine
        (Routine => PDE_To_Unsigned64'Access,
         Name    => "PD to unsigned 64");
      T.Add_Test_Routine
        (Routine => PTE_To_Unsigned64'Access,
         Name    => "PT to unsigned 64");
      T.Add_Test_Routine
        (Routine => PML4_Serialization'Access,
         Name    => "PML4 serialization");
      T.Add_Test_Routine
        (Routine => PDPT_Serialization'Access,
         Name    => "PDPT serialization");
      T.Add_Test_Routine
        (Routine => PD_Serialization'Access,
         Name    => "PD serialization");
      T.Add_Test_Routine
        (Routine => PT_Serialization'Access,
         Name    => "PT serialization");
   end Initialize;

   -------------------------------------------------------------------------

   procedure PD_Serialization
   is
      PD : Tables.PD.Page_Table_Type := Tables.PD.Create_Table (Number => 0);
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
   end PD_Serialization;

   -------------------------------------------------------------------------

   procedure PDE_To_Unsigned64
   is
      Ref : constant Interfaces.Unsigned_64 := 16#8000fffc800f000b#;
      PDE : constant Entries.PD_Entry_Type  := Entries.Create
        (Dst_Offset  => 0,
         Dst_Address => 16#fffc800f0000#,
         Readable    => False,
         Writable    => True,
         Executable  => False,
         Maps_Page   => False,
         Global      => False,
         Caching     => WC);
   begin
      Assert (Condition => To_Unsigned64 (E => PDE) = Ref,
              Message   => "PD entry unsigned 64 value mismatch");
   end PDE_To_Unsigned64;

   -------------------------------------------------------------------------

   procedure PDPT_Serialization
   is
      PDPT : Tables.PDPT.Page_Table_Type := Tables.PDPT.Create_Table
        (Number => 0);
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
   end PDPT_Serialization;

   -------------------------------------------------------------------------

   procedure PDPTE_To_Unsigned64
   is
      Ref   : constant Interfaces.Unsigned_64  := 16#8000002b3c00400b#;
      PDPTE : constant Entries.PDPT_Entry_Type := Entries.Create
        (Dst_Offset  => 0,
         Dst_Address => 16#2b3c004000#,
         Readable    => False,
         Writable    => True,
         Executable  => False,
         Maps_Page   => False,
         Global      => False,
         Caching     => WC);
   begin
      Assert (Condition => To_Unsigned64 (E => PDPTE) = Ref,
              Message   => "PDPT entry unsigned 64 value mismatch");
   end PDPTE_To_Unsigned64;

   -------------------------------------------------------------------------

   procedure PML4_Serialization
   is
      PML4 : Tables.PML4.Page_Table_Type := Tables.PML4.Create_Table
        (Number => 0);
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
   end PML4_Serialization;

   -------------------------------------------------------------------------

   procedure PML4E_To_Unsigned64
   is
      Ref   : constant Interfaces.Unsigned_64  := 16#80000000001f100b#;
      PML4E : constant Entries.PML4_Entry_Type := Entries.Create
        (Dst_Offset  => 0,
         Dst_Address => 16#1f1000#,
         Readable    => False,
         Writable    => True,
         Executable  => False,
         Maps_Page   => False,
         Global      => True,
         Caching     => WC);
   begin
      Assert (Condition => To_Unsigned64 (E => PML4E) = Ref,
              Message   => "PML4 entry unsigned 64 value mismatch");
   end PML4E_To_Unsigned64;

   -------------------------------------------------------------------------

   procedure PT_Serialization
   is
      PT : Tables.PT.Page_Table_Type := Tables.PT.Create_Table (Number => 0);
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
   end PT_Serialization;

   -------------------------------------------------------------------------

   procedure PTE_To_Unsigned64
   is
      Ref : constant Interfaces.Unsigned_64 := 16#100043f10b#;
      PTE : constant Entries.PT_Entry_Type  := Entries.Create
        (Dst_Offset  => 0,
         Dst_Address => 16#100043f000#,
         Readable    => False,
         Writable    => True,
         Executable  => True,
         Maps_Page   => False,
         Global      => True,
         Caching     => WC);
   begin
      Assert (Condition => To_Unsigned64 (E => PTE) = Ref,
              Message   => "PT entry unsigned 64 value mismatch");
   end PTE_To_Unsigned64;

end IA32e_Tests;
