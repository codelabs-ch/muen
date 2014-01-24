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
with Paging.EPT;

package body EPT_Tests
is

   use Ahven;
   use Paging;
   use Paging.EPT;
   use type Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "EPT paging tests");
      T.Add_Test_Routine
        (Routine => PML4_Serialization'Access,
         Name    => "PML4 serialization");
      T.Add_Test_Routine
        (Routine => PDPT_Serialization'Access,
         Name    => "PDPT serialization");
   end Initialize;

   -------------------------------------------------------------------------

   procedure PDPT_Serialization
   is
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
   end PDPT_Serialization;

   -------------------------------------------------------------------------

   procedure PML4_Serialization
   is
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
   end PML4_Serialization;

end EPT_Tests;
