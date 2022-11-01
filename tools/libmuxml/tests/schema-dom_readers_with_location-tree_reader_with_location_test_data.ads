--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.


with AUnit.Test_Fixtures;

with GNATtest_Generated;

package Schema.Dom_Readers_With_Location.Tree_Reader_With_Location_Test_Data is

   type Tree_Reader_With_Location_Access is access all GNATtest_Generated.GNATtest_Standard.Schema.Dom_Readers_With_Location.Tree_Reader_With_Location'Class;

--  begin read only
   type Test_Tree_Reader_With_Location is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : Tree_Reader_With_Location_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_Tree_Reader_With_Location);
   procedure Tear_Down (Gnattest_T : in out Test_Tree_Reader_With_Location);

end Schema.Dom_Readers_With_Location.Tree_Reader_With_Location_Test_Data;
