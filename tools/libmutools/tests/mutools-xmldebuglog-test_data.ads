--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;
with Muxml;

package Mutools.Xmldebuglog.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   --  does most merging steps to fill the logs in a consistent way
   procedure Merge_All_Steps (Data : in out Muxml.XML_Data_Type);

end Mutools.Xmldebuglog.Test_Data;
