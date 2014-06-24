--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Device_Domains.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Device_Domains.Test_Data.Tests is


--  begin read only
   procedure Test_Device_Reference_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Device_Reference_Uniqueness_a4b310 (Gnattest_T : in out Test) renames Test_Device_Reference_Uniqueness;
--  id:2.2/a4b31016bf911e13/Device_Reference_Uniqueness/1/0/
   procedure Test_Device_Reference_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device_domains.ads:25:4:Device_Reference_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/deviceDomains/domain/devices/device"
           & "[@physical='xhci']",
         Name  => "physical",
         Value => "wireless");

      begin
         Device_Reference_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device domains 'linux_domain' and 'wireless_domain' "
                    & "reference same physical device 'wireless'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Device_Reference_Uniqueness;
--  end read only

end Mucfgcheck.Device_Domains.Test_Data.Tests;
