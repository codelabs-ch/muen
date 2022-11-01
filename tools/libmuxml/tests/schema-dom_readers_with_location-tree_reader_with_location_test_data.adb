--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Schema.Dom_Readers_With_Location.Tree_Reader_With_Location_Test_Data is

   Local_Tree_Reader_With_Location : aliased GNATtest_Generated.GNATtest_Standard.Schema.Dom_Readers_With_Location.Tree_Reader_With_Location;
   procedure Set_Up (Gnattest_T : in out Test_Tree_Reader_With_Location) is
   begin
      Gnattest_T.Fixture := Local_Tree_Reader_With_Location'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_Tree_Reader_With_Location) is
   begin
      null;
   end Tear_Down;

end Schema.Dom_Readers_With_Location.Tree_Reader_With_Location_Test_Data;
