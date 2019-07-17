--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Device_Domains.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Mucfgcheck.Device_Domains.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

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
   procedure Test_Domain_Memory_Overlap (Gnattest_T : in out Test);
   procedure Test_Domain_Memory_Overlap_99bf8c (Gnattest_T : in out Test) renames Test_Domain_Memory_Overlap;
--  id:2.2/99bf8c89fba72094/Domain_Memory_Overlap/1/0/
   procedure Test_Domain_Memory_Overlap (Gnattest_T : in out Test) is
   --  mucfgcheck-device_domains.ads:28:4:Domain_Memory_Overlap
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
               Physical_Name => "kernel_data_0",
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


--  begin read only
   procedure Test_Memory_Reference_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Memory_Reference_Uniqueness_8ea57c (Gnattest_T : in out Test) renames Test_Memory_Reference_Uniqueness;
--  id:2.2/8ea57cd45d32c239/Memory_Reference_Uniqueness/1/0/
   procedure Test_Memory_Reference_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device_domains.ads:31:4:Memory_Reference_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/deviceDomains/domain/memory/memory"
         & "[@physical='linux|ram']",
         Name  => "physical",
         Value => "wireless_dma");

      begin
         Memory_Reference_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device domains 'linux_domain' and 'wireless_domain' "
                    & "reference same physical memory region 'wireless_dma'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Memory_Reference_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Memory_Mapping_Address_Equality (Gnattest_T : in out Test);
   procedure Test_Memory_Mapping_Address_Equality_7d613d (Gnattest_T : in out Test) renames Test_Memory_Mapping_Address_Equality;
--  id:2.2/7d613d8e75d60137/Memory_Mapping_Address_Equality/1/0/
   procedure Test_Memory_Mapping_Address_Equality (Gnattest_T : in out Test) is
   --  mucfgcheck-device_domains.ads:35:4:Memory_Mapping_Address_Equality
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/deviceDomains/domain/memory/memory"
         & "[@physical='linux|ram']",
         Name  => "virtualAddress",
         Value => "16#1000#");

      begin
         Memory_Mapping_Address_Equality (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical memory region 'linux|ram' referenced by device"
                    &" domain 'linux_domain' and subject 'linux' not mapped at"
                    & " the same address: 16#1000# /= 16#0090_0000#",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Memory_Mapping_Address_Equality;
--  end read only


--  begin read only
   procedure Test_Domain_Memory_Type (Gnattest_T : in out Test);
   procedure Test_Domain_Memory_Type_4a5128 (Gnattest_T : in out Test) renames Test_Domain_Memory_Type;
--  id:2.2/4a5128dd3c649f3f/Domain_Memory_Type/1/0/
   procedure Test_Domain_Memory_Type (Gnattest_T : in out Test) is
   --  mucfgcheck-device_domains.ads:38:4:Domain_Memory_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='wireless_dma']",
         Name  => "type",
         Value => "system_pt");

      begin
         Domain_Memory_Type (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device domain memory 'wireless_dma' has invalid memory"
                    & " type SYSTEM_PT",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Domain_Memory_Type;
--  end read only


--  begin read only
   procedure Test_PCI_Device_References (Gnattest_T : in out Test);
   procedure Test_PCI_Device_References_76ba6c (Gnattest_T : in out Test) renames Test_PCI_Device_References;
--  id:2.2/76ba6cac9424ec00/PCI_Device_References/1/0/
   procedure Test_PCI_Device_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device_domains.ads:41:4:PCI_Device_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/deviceDomains/domain/devices/"
         & "device[@physical='xhci']",
         Name  => "physical",
         Value => "keyboard");

      begin
         PCI_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device 'keyboard' referenced by device domain "
                    & "'linux_domain' is not a PCI device",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_PCI_Device_References;
--  end read only


--  begin read only
   procedure Test_Domain_PT_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Domain_PT_Region_Presence_393bd6 (Gnattest_T : in out Test) renames Test_Domain_PT_Region_Presence;
--  id:2.2/393bd68fe235e762/Domain_PT_Region_Presence/1/0/
   procedure Test_Domain_PT_Region_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-device_domains.ads:44:4:Domain_PT_Region_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/deviceDomains/domain[@name='linux_domain']",
         Name  => "name",
         Value => "domain_without_pt");

      begin
         Domain_PT_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No file-backed PT region for device domain "
                    & "'domain_without_pt' found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Domain_PT_Region_Presence;
--  end read only


--  begin read only
   procedure Test_PCI_Bus_Context_Region_Presence (Gnattest_T : in out Test);
   procedure Test_PCI_Bus_Context_Region_Presence_aa1427 (Gnattest_T : in out Test) renames Test_PCI_Bus_Context_Region_Presence;
--  id:2.2/aa142726d91f9ac7/PCI_Bus_Context_Region_Presence/1/0/
   procedure Test_PCI_Bus_Context_Region_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-device_domains.ads:47:4:PCI_Bus_Context_Region_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vtd_context_3']",
         Name  => "type",
         Value => "subject");

      begin
         PCI_Bus_Context_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No file-backed VT-d context table memory region found "
                    & "for PCI bus 16#03#",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_PCI_Bus_Context_Region_Presence;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Mucfgcheck.Device_Domains.Test_Data.Tests;
