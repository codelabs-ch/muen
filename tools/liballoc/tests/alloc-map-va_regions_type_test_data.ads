--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.


with AUnit.Test_Fixtures;

with GNATtest_Generated;

package Alloc.Map.VA_Regions_Type_Test_Data is

   type VA_Regions_Type_Access is access all GNATtest_Generated.GNATtest_Standard.Alloc.Map.VA_Regions_Type'Class;

--  begin read only
   type Test_VA_Regions_Type is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : VA_Regions_Type_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_VA_Regions_Type);
   procedure Tear_Down (Gnattest_T : in out Test_VA_Regions_Type);

end Alloc.Map.VA_Regions_Type_Test_Data;
