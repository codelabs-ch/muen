--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Expanders.Subjects.Test_Data;

with Test_Utils.Expander;

package Expanders.Events.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   --  Prepare 'asap' events expansion step.
   procedure Prepare_Asap_Events (Data : in out Muxml.XML_Data_Type);

end Expanders.Events.Test_Data;
