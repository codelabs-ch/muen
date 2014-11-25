--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.


with AUnit.Test_Fixtures;

with GNATtest_Generated;

package Paging.Entries.Table_Entry_Type_Test_Data is

   type Table_Entry_Type_Access is access all GNATtest_Generated.GNATtest_Standard.Paging.Entries.Table_Entry_Type'Class;

--  begin read only
   type Test_Table_Entry_Type is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : Table_Entry_Type_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Tear_Down (Gnattest_T : in out Test_Table_Entry_Type);

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

end Paging.Entries.Table_Entry_Type_Test_Data;
