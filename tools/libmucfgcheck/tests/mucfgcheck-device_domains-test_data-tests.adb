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


--  begin read only
   procedure Test_IOMMU_Presence (Gnattest_T : in out Test);
   procedure Test_IOMMU_Presence_6c934e (Gnattest_T : in out Test) renames Test_IOMMU_Presence;
--  id:2.2/6c934e0540bf7353/IOMMU_Presence/1/0/
   procedure Test_IOMMU_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-device_domains.ads:28:4:IOMMU_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/devices/device[@name='iommu_1']",
         Name  => "name",
         Value => "foo");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/devices/device[@name='iommu_2']",
         Name  => "name",
         Value => "bar");

      begin
         IOMMU_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device domains specified but no IOMMU device provided "
                    & "by platform",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_IOMMU_Presence;
--  end read only


--  begin read only
   procedure Test_Domain_Memory_Overlap (Gnattest_T : in out Test);
   procedure Test_Domain_Memory_Overlap_99bf8c (Gnattest_T : in out Test) renames Test_Domain_Memory_Overlap;
--  id:2.2/99bf8c89fba72094/Domain_Memory_Overlap/1/0/
   procedure Test_Domain_Memory_Overlap (Gnattest_T : in out Test) is
   --  mucfgcheck-device_domains.ads:31:4:Domain_Memory_Overlap
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Mem_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/deviceDomains/domain[@name='linux_domain']"
            & "/memory");
      begin
         Muxml.Utils.Append_Child
           (Node      => Mem_Node,
            New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
              (Policy        => Data,
               Logical_Name  => "dma",
               Physical_Name => "kernel_data",
               Address       => "16#00a0_1000#",
               Writable      => True,
               Executable    => False));

         Domain_Memory_Overlap (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of domain memory region 'linux|ram' and 'dma'"
                    & " of device domain 'linux_domain'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Domain_Memory_Overlap;
--  end read only

end Mucfgcheck.Device_Domains.Test_Data.Tests;
