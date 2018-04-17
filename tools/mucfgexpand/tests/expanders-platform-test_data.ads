--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Test_Utils.Expander;

package Expanders.Platform.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   --  Remove platform section from policy.
   procedure Remove_Platform_Section (Data : in out Muxml.XML_Data_Type);

   --  Remove resources of subject logical device 'wlan' and add additional
   --  resources to physical device.
   procedure Adjust_Subj_Device_Alias_Resources
     (Data : in out Muxml.XML_Data_Type);

   --  Remove all resources from 'network_adapters' device class.
   procedure Remove_Network_Adapters_Device_Class_Resources
     (Data : in out Muxml.XML_Data_Type);

   --  Enable RMRR for 'xhci' device class.
   procedure Map_Reserved_Memory_Xhci_Device_Class
     (Data : in out Muxml.XML_Data_Type);

end Expanders.Platform.Test_Data;
