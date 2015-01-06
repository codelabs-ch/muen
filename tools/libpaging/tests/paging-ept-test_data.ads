--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Ada.Exceptions;
with Ada.Streams.Stream_IO;

with Mutools.Files;

with Paging.Entries;

with Test_Utils;

package Paging.EPT.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   Ref_PDPT_Entry_0 : constant Entries.Table_Entry_Type
     := Entries.Create
       (Dst_Index   => 0,
        Dst_Address => 16#4000_0000#,
        Readable    => True,
        Writable    => True,
        Executable  => True,
        Maps_Page   => True,
        Global      => False,
        Caching     => UC);

   Ref_PDPT_Entry_1 : constant Entries.Table_Entry_Type
     := Entries.Create
       (Dst_Index   => 0,
        Dst_Address => 16#8000_0000#,
        Readable    => True,
        Writable    => True,
        Executable  => True,
        Maps_Page   => True,
        Global      => False,
        Caching     => UC);

   Ref_PDPT_Entry_2 : constant Entries.Table_Entry_Type
     := Entries.Create
       (Dst_Index   => 0,
        Dst_Address => 16#c000_0000#,
        Readable    => True,
        Writable    => True,
        Executable  => True,
        Maps_Page   => True,
        Global      => False,
        Caching     => UC);

   Ref_PDPT_Entry_3 : constant Entries.Table_Entry_Type
     := Entries.Create
       (Dst_Index   => 0,
        Dst_Address => 16#1_0000_0000#,
        Readable    => True,
        Writable    => True,
        Executable  => True,
        Maps_Page   => True,
        Global      => False,
        Caching     => UC);

   Ref_PT_Entry     : constant Entries.Table_Entry_Type
     := Entries.Create
       (Dst_Index   => 0,
        Dst_Address => 16#000b_8000#,
        Readable    => True,
        Writable    => True,
        Executable  => False,
        Maps_Page   => True,
        Global      => False,
        Caching     => WC);

end Paging.EPT.Test_Data;
