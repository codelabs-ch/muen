--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Test_Utils.Expander;

package Expanders.Subjects.Profiles.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   --  Prepare subject for profile expansion step with existing BIOS mapping.
   procedure Prepare_Profile_BIOS (Data: in out Muxml.XML_Data_Type);

end Expanders.Subjects.Profiles.Test_Data;
