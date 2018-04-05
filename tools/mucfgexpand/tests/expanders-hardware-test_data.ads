--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Expanders.Subjects;

with Test_Utils.Expander;

package Expanders.Hardware.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   --  Prepare policy for PCI Device MSI expander.
   procedure Pre_PCI_Device_MSI (Data : in out Muxml.XML_Data_Type);

end Expanders.Hardware.Test_Data;
