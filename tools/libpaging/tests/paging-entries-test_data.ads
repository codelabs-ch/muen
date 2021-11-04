--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

package Paging.Entries.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   Test_Entry : constant Table_Entry_Type
     := Create (Dst_Index   => 42,
                Dst_Address => 16#1f_f000#,
                Present     => True,
                Readable    => True,
                Writable    => False,
                Executable  => True,
                Maps_Page   => True,
                Global      => True,
                Caching     => Paging.WB);

end Paging.Entries.Test_Data;
