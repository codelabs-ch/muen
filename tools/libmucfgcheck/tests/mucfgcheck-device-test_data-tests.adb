--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Device.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Device.Test_Data.Tests is


--  begin read only
   procedure Test_Physical_Device_References (Gnattest_T : in out Test);
   procedure Test_Physical_Device_References_b4cc94 (Gnattest_T : in out Test) renames Test_Physical_Device_References;
--  id:2.2/b4cc947cfd4d6ff0/Physical_Device_References/1/0/
   procedure Test_Physical_Device_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:25:4:Physical_Device_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/devices/device[@physical='ioapic']",
         Name  => "physical",
         Value => "nonexistent");

      begin
         Physical_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device 'nonexistent' referenced by logical"
                    & " device 'ioapic' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Physical_Device_References;
--  end read only


--  begin read only
   procedure Test_Physical_Device_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Physical_Device_Name_Uniqueness_fa4110 (Gnattest_T : in out Test) renames Test_Physical_Device_Name_Uniqueness;
--  id:2.2/fa4110c6a29dd204/Physical_Device_Name_Uniqueness/1/0/
   procedure Test_Physical_Device_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:28:4:Physical_Device_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/devices/device[@name='serial']",
         Name  => "name",
         Value => "vga");

      begin
         Physical_Device_Name_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple physical devices with name 'vga'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Physical_Device_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Physical_IRQ_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Physical_IRQ_Uniqueness_11c442 (Gnattest_T : in out Test) renames Test_Physical_IRQ_Uniqueness;
--  id:2.2/11c442b92552adf4/Physical_IRQ_Uniqueness/1/0/
   procedure Test_Physical_IRQ_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:31:4:Physical_IRQ_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Serial : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/platform/devices/device[@name='serial']");
         Node   : constant DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "irq");
      begin
         DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                          Name  => "number",
                                          Value => "1");
         Muxml.Utils.Append_Child (Node      => Serial,
                                   New_Child => Node);

         Physical_IRQ_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Devices 'serial' and 'keyboard' share IRQ 1",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Physical_IRQ_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Physical_IRQ_References (Gnattest_T : in out Test);
   procedure Test_Physical_IRQ_References_b54993 (Gnattest_T : in out Test) renames Test_Physical_IRQ_References;
--  id:2.2/b5499347878df1ba/Physical_IRQ_References/1/0/
   procedure Test_Physical_IRQ_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:34:4:Physical_IRQ_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/device/irq"
         & "[@physical='kbd_irq']",
         Name  => "physical",
         Value => "nonexistent");

      begin
         Physical_IRQ_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical IRQ 'nonexistent' referenced by logical IRQ"
                    & " 'kbd_irq' of logical device 'keyboard' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Physical_IRQ_References;
--  end read only


--  begin read only
   procedure Test_Device_IRQ_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Device_IRQ_Name_Uniqueness_0150bf (Gnattest_T : in out Test) renames Test_Device_IRQ_Name_Uniqueness;
--  id:2.2/0150bf5273c9a2cb/Device_IRQ_Name_Uniqueness/1/0/
   procedure Test_Device_IRQ_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:37:4:Device_IRQ_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Kbd  : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/platform/devices/device[@name='keyboard']");
         Node : constant DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "irq");
      begin
         DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                          Name  => "name",
                                          Value => "kbd_irq");
         Muxml.Utils.Append_Child (Node      => Kbd,
                                   New_Child => Node);

         Device_IRQ_Name_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device 'keyboard' has multiple IRQs with name "
                    & "'kbd_irq'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Device_IRQ_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_IO_Port_Start_Smaller_End (Gnattest_T : in out Test);
   procedure Test_IO_Port_Start_Smaller_End_c12eaa (Gnattest_T : in out Test) renames Test_IO_Port_Start_Smaller_End;
--  id:2.2/c12eaa9dd1b2f74e/IO_Port_Start_Smaller_End/1/0/
   procedure Test_IO_Port_Start_Smaller_End (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:40:4:IO_Port_Start_Smaller_End
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/platform/devices/device/ioPort[@name='ports']");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => "16#ffff#");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "end",
            Value => "16#50b8#");

         IO_Port_Start_Smaller_End (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "I/O port 'ports' start 16#ffff# larger than "
                    & "end 16#50b8#",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_IO_Port_Start_Smaller_End;
--  end read only


--  begin read only
   procedure Test_IO_Port_References (Gnattest_T : in out Test);
   procedure Test_IO_Port_References_5e0653 (Gnattest_T : in out Test) renames Test_IO_Port_References;
--  id:2.2/5e0653dce539594f/IO_Port_References/1/0/
   procedure Test_IO_Port_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:43:4:IO_Port_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/device[@logical='vga']"
         & "/ioPort",
         Name  => "physical",
         Value => "nonexistent");

      begin
         IO_Port_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical I/O port 'nonexistent' referenced by logical"
                    & " I/O port 'ports' of logical device 'vga' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_IO_Port_References;
--  end read only


--  begin read only
   procedure Test_Device_IO_Port_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Device_IO_Port_Name_Uniqueness_3e600f (Gnattest_T : in out Test) renames Test_Device_IO_Port_Name_Uniqueness;
--  id:2.2/3e600f38d0777032/Device_IO_Port_Name_Uniqueness/1/0/
   procedure Test_Device_IO_Port_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:46:4:Device_IO_Port_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/devices/device/ioPort[@name='port_64']",
         Name  => "name",
         Value => "port_60");

      begin
         Device_IO_Port_Name_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device 'keyboard' has multiple I/O ports with name "
                    & "'port_60'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Device_IO_Port_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Device_Memory_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Device_Memory_Name_Uniqueness_6a4d02 (Gnattest_T : in out Test) renames Test_Device_Memory_Name_Uniqueness;
--  id:2.2/6a4d025abc9b72fc/Device_Memory_Name_Uniqueness/1/0/
   procedure Test_Device_Memory_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:49:4:Device_Memory_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Dev  : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/platform/devices/device[@name='vga']");
         Node : constant DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "memory");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "name",
            Value => "buffer");
         Muxml.Utils.Append_Child
           (Node      => Dev,
            New_Child => Node);

         Device_Memory_Name_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device 'vga' has multiple memory regions with name"
                    & " 'buffer'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Device_Memory_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Device_Memory_References (Gnattest_T : in out Test);
   procedure Test_Device_Memory_References_6481e3 (Gnattest_T : in out Test) renames Test_Device_Memory_References;
--  id:2.2/6481e34bd4cbc943/Device_Memory_References/1/0/
   procedure Test_Device_Memory_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:52:4:Device_Memory_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/devices/device/memory",
         Name  => "physical",
         Value => "nonexistent");

      begin
         Device_Memory_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device memory 'nonexistent' referenced by"
                    & " logical device memory 'mmio' of logical device "
                    & "'ioapic' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Device_Memory_References;
--  end read only


--  begin read only
   procedure Test_Device_Sharing (Gnattest_T : in out Test);
   procedure Test_Device_Sharing_288f44 (Gnattest_T : in out Test) renames Test_Device_Sharing;
--  id:2.2/288f44a12a8ccac8/Device_Sharing/1/0/
   procedure Test_Device_Sharing (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:55:4:Device_Sharing
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/"
         & "device[@physical='port80']",
         Name  => "physical",
         Value => "cmos_rtc");

      begin
         Device_Sharing (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Non-shareable device 'cmos_rtc' is referenced by "
                    & "multiple logical devices 'time->port80', "
                    & "'linux->cmos_rtc'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Device_Sharing;
--  end read only


--  begin read only
   procedure Test_PCI_Device_BDF_Uniqueness (Gnattest_T : in out Test);
   procedure Test_PCI_Device_BDF_Uniqueness_bef97c (Gnattest_T : in out Test) renames Test_PCI_Device_BDF_Uniqueness;
--  id:2.2/bef97c6f1475ed8d/PCI_Device_BDF_Uniqueness/1/0/
   procedure Test_PCI_Device_BDF_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:58:4:PCI_Device_BDF_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/devices/device/pci[@device='16#19#']",
         Name  => "device",
         Value => "16#14#");

      begin
         PCI_Device_BDF_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "PCI devices 'xhci' and 'ethernet' have identical BDF",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_PCI_Device_BDF_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Device_Reference_BDF_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Device_Reference_BDF_Uniqueness_639981 (Gnattest_T : in out Test) renames Test_Device_Reference_BDF_Uniqueness;
--  id:2.2/63998159ef33e880/Device_Reference_BDF_Uniqueness/1/0/
   procedure Test_Device_Reference_BDF_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:62:4:Device_Reference_BDF_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Device_Reference_BDF_Uniqueness (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/device/"
         & "pci[@device='16#19#']",
         Name  => "device",
         Value => "16#14#");

      begin
         Device_Reference_BDF_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical PCI devices 'xhci' and 'eth0' of subject "
                    & "'linux' have identical BDF",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Device_Reference_BDF_Uniqueness;
--  end read only


--  begin read only
   procedure Test_PCI_Device_References (Gnattest_T : in out Test);
   procedure Test_PCI_Device_References_76ba6c (Gnattest_T : in out Test) renames Test_PCI_Device_References;
--  id:2.2/76ba6cac9424ec00/PCI_Device_References/1/0/
   procedure Test_PCI_Device_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:66:4:PCI_Device_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      PCI_Device_References (XML_Data => Data);

      Muxml.Utils.Remove_Child
        (Node       => Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/platform/devices/device[@name='xhci']"),
         Child_Name => "pci");

      begin
         PCI_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical PCI device 'xhci' of subject 'linux' references"
                    & " physical non-PCI device 'xhci'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_PCI_Device_References;
--  end read only


--  begin read only
   procedure Test_Legacy_Device_References (Gnattest_T : in out Test);
   procedure Test_Legacy_Device_References_73e649 (Gnattest_T : in out Test) renames Test_Legacy_Device_References;
--  id:2.2/73e6491f4fa978a4/Legacy_Device_References/1/0/
   procedure Test_Legacy_Device_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:70:4:Legacy_Device_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Legacy_Device_References (XML_Data => Data);

      Muxml.Utils.Remove_Child
        (Node       => Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/devices/"
            & "device[@physical='ethernet']"),
         Child_Name => "pci");

      begin
         Legacy_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical legacy device 'eth0' of subject 'linux'"
                    & " references physical non-legacy device 'ethernet'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Legacy_Device_References;
--  end read only


--  begin read only
   procedure Test_Device_References_PCI_Bus_Number (Gnattest_T : in out Test);
   procedure Test_Device_References_PCI_Bus_Number_994df0 (Gnattest_T : in out Test) renames Test_Device_References_PCI_Bus_Number;
--  id:2.2/994df063b163349f/Device_References_PCI_Bus_Number/1/0/
   procedure Test_Device_References_PCI_Bus_Number (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:73:4:Device_References_PCI_Bus_Number
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Device_References_PCI_Bus_Number;
--  end read only


--  begin read only
   procedure Test_Debugconsole_Presence (Gnattest_T : in out Test);
   procedure Test_Debugconsole_Presence_b13687 (Gnattest_T : in out Test) renames Test_Debugconsole_Presence;
--  id:2.2/b13687f7ed7372fc/Debugconsole_Presence/1/0/
   procedure Test_Debugconsole_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:76:4:Debugconsole_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/devices/device[@name='debugconsole']",
         Name  => "name",
         Value => "foobar");

      begin
         Debugconsole_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device 'debugconsole' with I/O port resource"
                    & " 'port' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Debugconsole_Presence;
--  end read only


--  begin read only
   procedure Test_IOMMU_Region_Size (Gnattest_T : in out Test);
   procedure Test_IOMMU_Region_Size_7f9036 (Gnattest_T : in out Test) renames Test_IOMMU_Region_Size;
--  id:2.2/7f903633b01e1f7b/IOMMU_Region_Size/1/0/
   procedure Test_IOMMU_Region_Size (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:79:4:IOMMU_Region_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/devices/device[@name='iommu_1']/memory",
         Name  => "size",
         Value => "16#2000#");

      begin
         IOMMU_Region_Size (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#2000#' of 'mmio' IOMMU memory "
                    & "region element not 4K",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_IOMMU_Region_Size;
--  end read only

end Mucfgcheck.Device.Test_Data.Tests;
