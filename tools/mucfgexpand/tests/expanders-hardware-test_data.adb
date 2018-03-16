--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Muxml.Utils;

with Expanders.Components;

package body Expanders.Hardware.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;

   -------------------------------------------------------------------------

   procedure Pre_PCI_Device_MSI (Data : in out Muxml.XML_Data_Type)
   is
   begin

      --  Remove all PCI msi attributes.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device/pci",
         Name  => "msi",
         Value => "");

      Components.Add_Devices (Data => Data);
      Add_MSI_IRQ_Numbers (Data => Data);
   end Pre_PCI_Device_MSI;

end Expanders.Hardware.Test_Data;
