--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Ada.Streams.Stream_IO;

with Mutools.Files;

with Paging.Entries;

with Test_Utils;

package Paging.IA32e.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   Ref_PML4_Entry : constant Entries.Table_Entry_Type
     := Entries.Create
       (Dst_Index   => 0,
        Dst_Address => 16#001f_1000#,
        Readable    => True,
        Writable    => True,
        Executable  => True,
        Maps_Page   => False,
        Global      => False,
        Caching     => WC);

end Paging.IA32e.Test_Data;
