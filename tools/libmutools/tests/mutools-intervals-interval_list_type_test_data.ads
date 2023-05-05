--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.


with AUnit.Test_Fixtures;

with GNATtest_Generated;

package Mutools.Intervals.Interval_List_Type_Test_Data is

   type Interval_List_Type_Access is access all GNATtest_Generated.GNATtest_Standard.Mutools.Intervals.Interval_List_Type'Class;

--  begin read only
   type Test_Interval_List_Type is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : Interval_List_Type_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_Interval_List_Type);
   procedure Tear_Down (Gnattest_T : in out Test_Interval_List_Type);

end Mutools.Intervals.Interval_List_Type_Test_Data;
