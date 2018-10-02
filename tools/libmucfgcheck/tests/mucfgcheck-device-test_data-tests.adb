--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Device.Test_Data.

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
package body Mucfgcheck.Device.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

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
   --  mucfgcheck-device.ads:29:4:Physical_Device_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;

      procedure Positive_Test;
      procedure Duplicate_Physical_Device_Name;
      procedure Duplicate_Alias_Name;
      procedure Duplicate_Class_Name;
      procedure Identical_Alias_And_Physical_Device_Name;

      ----------------------------------------------------------------------

      procedure Duplicate_Alias_Name
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/platform/mappings/aliases/alias[@name='wlan']",
            Name  => "name",
            Value => "nic");

         begin
            Physical_Device_Name_Uniqueness (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Multiple physical devices, aliases or classes with "
                       & "name 'nic'",
                       Message   => "Exception mismatch");
         end;
      end Duplicate_Alias_Name;

      ----------------------------------------------------------------------

      procedure Duplicate_Class_Name
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/platform/mappings/classes/class[@name='usb']",
            Name  => "name",
            Value => "network_devices");

         begin
            Physical_Device_Name_Uniqueness (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Multiple physical devices, aliases or classes with "
                       & "name 'network_devices'",
                       Message   => "Exception mismatch");
         end;
      end Duplicate_Class_Name;

      ----------------------------------------------------------------------

      procedure Duplicate_Physical_Device_Name
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device[@name='serial']",
            Name  => "name",
            Value => "vga");

         begin
            Physical_Device_Name_Uniqueness (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Multiple physical devices, aliases or classes with "
                       & "name 'vga'",
                       Message   => "Exception mismatch");
         end;
      end Duplicate_Physical_Device_Name;

      ----------------------------------------------------------------------

      procedure Identical_Alias_And_Class_Name
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/platform/mappings/aliases/alias[@name='wlan']",
            Name  => "name",
            Value => "usb");

         begin
            Physical_Device_Name_Uniqueness (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Multiple physical devices, aliases or classes with "
                       & "name 'usb'",
                       Message   => "Exception mismatch");
         end;
      end Identical_Alias_And_Class_Name;

      ----------------------------------------------------------------------

      procedure Identical_Alias_And_Physical_Device_Name
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/platform/mappings/aliases/alias[@name='wlan']",
            Name  => "name",
            Value => "ethernet");

         begin
            Physical_Device_Name_Uniqueness (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Multiple physical devices, aliases or classes with "
                       & "name 'ethernet'",
                       Message   => "Exception mismatch");
         end;
      end Identical_Alias_And_Physical_Device_Name;

      ----------------------------------------------------------------------

      procedure Identical_Class_And_Physical_Device_Name
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/platform/mappings/classes/class[@name='usb']",
            Name  => "name",
            Value => "xhci");

         begin
            Physical_Device_Name_Uniqueness (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Multiple physical devices, aliases or classes with "
                       & "name 'xhci'",
                       Message   => "Exception mismatch");
         end;
      end Identical_Class_And_Physical_Device_Name;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         -- Positive test, must not raise an exception.

         Physical_Device_Name_Uniqueness (XML_Data => Data);
      end Positive_Test;
   begin
      Positive_Test;
      Duplicate_Physical_Device_Name;
      Duplicate_Alias_Name;
      Duplicate_Class_Name;
      Identical_Alias_And_Physical_Device_Name;
      Identical_Class_And_Physical_Device_Name;
      Identical_Alias_And_Class_Name;
--  begin read only
   end Test_Physical_Device_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Physical_IRQ_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Physical_IRQ_Uniqueness_11c442 (Gnattest_T : in out Test) renames Test_Physical_IRQ_Uniqueness;
--  id:2.2/11c442b92552adf4/Physical_IRQ_Uniqueness/1/0/
   procedure Test_Physical_IRQ_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:32:4:Physical_IRQ_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Physical_IRQ_Uniqueness (XML_Data => Data);

      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device[@name='wireless']/irq",
            Name  => "number",
            Value => "20");

         Physical_IRQ_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Devices 'wireless' and 'ethernet' share IRQ 20",
                    Message   => "Exception mismatch");
      end;

      --  Shared IRQs of unreferenced devices must not raise an exception.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices"
         & "/device[@physical='wireless']",
         Name  => "physical",
         Value => "nonexistent");

      Physical_IRQ_Uniqueness (XML_Data => Data);
--  begin read only
   end Test_Physical_IRQ_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Physical_IRQ_References (Gnattest_T : in out Test);
   procedure Test_Physical_IRQ_References_b54993 (Gnattest_T : in out Test) renames Test_Physical_IRQ_References;
--  id:2.2/b5499347878df1ba/Physical_IRQ_References/1/0/
   procedure Test_Physical_IRQ_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:35:4:Physical_IRQ_References
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
   procedure Test_Physical_IRQ_Constraints_ISA (Gnattest_T : in out Test);
   procedure Test_Physical_IRQ_Constraints_ISA_d563b8 (Gnattest_T : in out Test) renames Test_Physical_IRQ_Constraints_ISA;
--  id:2.2/d563b8cdeba41d5c/Physical_IRQ_Constraints_ISA/1/0/
   procedure Test_Physical_IRQ_Constraints_ISA (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:38:4:Physical_IRQ_Constraints_ISA
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Physical_IRQ_Constraints_ISA (XML_Data => Data);
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Count_Constraint
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         declare
            IRQ : DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/hardware/devices/device"
                 & "[@name='keyboard']/irq");
         begin
            for I in Positive range 1 .. 16 loop
               IRQ := DOM.Core.Nodes.Insert_Before
                 (N         => DOM.Core.Nodes.Parent_Node (N => IRQ),
                  New_Child => DOM.Core.Nodes.Clone_Node
                    (N => IRQ, Deep => True),
                  Ref_Child => IRQ);
            end loop;

            Physical_IRQ_Constraints_ISA (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Device 'keyboard' specifies more than 16 ISA IRQ(s)",
                       Message   => "Exception mismatch");
         end;
      end Count_Constraint;

      ----------------------------------------------------------------------

      procedure Range_Constraint
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device[@name='keyboard']/irq",
            Name  => "number",
            Value => "16");

         begin
            Physical_IRQ_Constraints_ISA (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Attribute 'number => 16' of 'kbd_irq' ISA IRQ "
                       & "element not in allowed range 0 .. 15 (device "
                       & "'keyboard')",
                       Message   => "Exception mismatch");
         end;
      end Range_Constraint;
   begin
      Positive_Test;
      Count_Constraint;
      Range_Constraint;
--  begin read only
   end Test_Physical_IRQ_Constraints_ISA;
--  end read only


--  begin read only
   procedure Test_Physical_IRQ_Constraints_PCI_LSI (Gnattest_T : in out Test);
   procedure Test_Physical_IRQ_Constraints_PCI_LSI_6af92d (Gnattest_T : in out Test) renames Test_Physical_IRQ_Constraints_PCI_LSI;
--  id:2.2/6af92dfdb880c918/Physical_IRQ_Constraints_PCI_LSI/1/0/
   procedure Test_Physical_IRQ_Constraints_PCI_LSI (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:41:4:Physical_IRQ_Constraints_PCI_LSI
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Physical_IRQ_Constraints_PCI_LSI (XML_Data => Data);
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Count_Constraint
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         declare
            IRQ : DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/hardware/devices/device"
                 & "[@name='ethernet']/irq");
         begin
            for I in Positive range 1 .. 4 loop
               IRQ := DOM.Core.Nodes.Insert_Before
                 (N         => DOM.Core.Nodes.Parent_Node (N => IRQ),
                  New_Child => DOM.Core.Nodes.Clone_Node
                    (N => IRQ, Deep => True),
                  Ref_Child => IRQ);
            end loop;

            Physical_IRQ_Constraints_PCI_LSI (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Device 'ethernet' specifies more than 4 PCI LSI "
                       & "IRQ(s)",
                       Message   => "Exception mismatch");
         end;
      end Count_Constraint;

      ----------------------------------------------------------------------

      procedure Range_Constraint
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device[@name='ethernet']/irq",
            Name  => "number",
            Value => "24");

         begin
            Physical_IRQ_Constraints_PCI_LSI (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Attribute 'number => 24' of 'irq' PCI LSI IRQ "
                       & "element not in allowed range 0 .. 23 (device "
                       & "'ethernet')",
                 Message   => "Exception mismatch");
         end;
      end Range_Constraint;
   begin
      Positive_Test;
      Count_Constraint;
      Range_Constraint;
--  begin read only
   end Test_Physical_IRQ_Constraints_PCI_LSI;
--  end read only


--  begin read only
   procedure Test_Physical_IRQ_Constraints_PCI_MSI (Gnattest_T : in out Test);
   procedure Test_Physical_IRQ_Constraints_PCI_MSI_0733b1 (Gnattest_T : in out Test) renames Test_Physical_IRQ_Constraints_PCI_MSI;
--  id:2.2/0733b1483bff35d2/Physical_IRQ_Constraints_PCI_MSI/1/0/
   procedure Test_Physical_IRQ_Constraints_PCI_MSI (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:44:4:Physical_IRQ_Constraints_PCI_MSI
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Physical_IRQ_Constraints_PCI_MSI (XML_Data => Data);
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Count_Constraint
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         declare
            IRQ : DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/hardware/devices/device"
                 & "[@name='xhci']/irq");
         begin
            for I in Positive range 1 .. 200 loop
               IRQ := DOM.Core.Nodes.Insert_Before
                 (N         => DOM.Core.Nodes.Parent_Node (N => IRQ),
                  New_Child => DOM.Core.Nodes.Clone_Node
                    (N => IRQ, Deep => True),
                  Ref_Child => IRQ);
            end loop;

            Physical_IRQ_Constraints_PCI_MSI (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Device 'xhci' specifies more than 197 PCI MSI IRQ(s)",
                       Message   => "Exception mismatch");
         end;
      end Count_Constraint;

      ----------------------------------------------------------------------

      procedure Range_Constraint
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device[@name='xhci']/irq",
            Name  => "number",
            Value => "221");

         begin
            Physical_IRQ_Constraints_PCI_MSI (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Attribute 'number => 221' of 'irq1' PCI MSI IRQ "
                       & "element not in allowed range 24 .. 220 (device "
                       & "'xhci')",
                       Message   => "Exception mismatch");
         end;
      end Range_Constraint;
   begin
      Positive_Test;
      Count_Constraint;
      Range_Constraint;
--  begin read only
   end Test_Physical_IRQ_Constraints_PCI_MSI;
--  end read only


--  begin read only
   procedure Test_Physical_IRQ_MSI_Consecutiveness (Gnattest_T : in out Test);
   procedure Test_Physical_IRQ_MSI_Consecutiveness_53da93 (Gnattest_T : in out Test) renames Test_Physical_IRQ_MSI_Consecutiveness;
--  id:2.2/53da934017e5ea72/Physical_IRQ_MSI_Consecutiveness/1/0/
   procedure Test_Physical_IRQ_MSI_Consecutiveness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:47:4:Physical_IRQ_MSI_Consecutiveness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Physical_IRQ_MSI_Consecutiveness (XML_Data => Data);

      declare
         IRQ : DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              XPath => "/system/hardware/devices/device"
              & "[@name='xhci']/irq");
      begin
         IRQ := DOM.Core.Nodes.Insert_Before
           (N         => DOM.Core.Nodes.Parent_Node (N => IRQ),
            New_Child => DOM.Core.Nodes.Clone_Node
              (N => IRQ, Deep => True),
            Ref_Child => IRQ);
         DOM.Core.Elements.Set_Attribute (Elem  => IRQ,
                                          Name  => "number",
                                          Value => "67");

         Physical_IRQ_MSI_Consecutiveness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "MSI IRQ 'irq1' of physical device 'xhci' not adjacent "
                    & "to other IRQs",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Physical_IRQ_MSI_Consecutiveness;
--  end read only


--  begin read only
   procedure Test_Device_IRQ_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Device_IRQ_Name_Uniqueness_0150bf (Gnattest_T : in out Test) renames Test_Device_IRQ_Name_Uniqueness;
--  id:2.2/0150bf5273c9a2cb/Device_IRQ_Name_Uniqueness/1/0/
   procedure Test_Device_IRQ_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:50:4:Device_IRQ_Name_Uniqueness
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
            XPath => "/system/hardware/devices/device[@name='keyboard']");
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
   --  mucfgcheck-device.ads:53:4:IO_Port_Start_Smaller_End
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
            XPath => "/system/hardware/devices/device/ioPort[@name='ports']");
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
   --  mucfgcheck-device.ads:56:4:IO_Port_References
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
   procedure Test_IO_Port_Uniqueness (Gnattest_T : in out Test);
   procedure Test_IO_Port_Uniqueness_73848b (Gnattest_T : in out Test) renames Test_IO_Port_Uniqueness;
--  id:2.2/73848b8e83430aad/IO_Port_Uniqueness/1/0/
   procedure Test_IO_Port_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:59:4:IO_Port_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      IO_Port_Uniqueness (XML_Data => Data);

      --  Single port overlap.

      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device[@name='cmos_rtc']/"
            & "ioPort",
            Name  => "end",
            Value => "16#0080#");

         IO_Port_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Devices 'cmos_rtc' and 'port80' have overlapping I/O "
                    & "port(s)",
                    Message   => "Exception mismatch");
      end;

      --  Full range overlap.

      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device[@name='keyboard']/"
            & "ioPort[@name='port_64']",
            Name  => "end",
            Value => "16#0090#");

         IO_Port_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Devices 'keyboard' and 'cmos_rtc' have overlapping I/O "
                    & "port(s)",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_IO_Port_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Device_IO_Port_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Device_IO_Port_Name_Uniqueness_3e600f (Gnattest_T : in out Test) renames Test_Device_IO_Port_Name_Uniqueness;
--  id:2.2/3e600f38d0777032/Device_IO_Port_Name_Uniqueness/1/0/
   procedure Test_Device_IO_Port_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:62:4:Device_IO_Port_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device/ioPort[@name='port_64']",
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
   --  mucfgcheck-device.ads:65:4:Device_Memory_Name_Uniqueness
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
            XPath => "/system/hardware/devices/device[@name='vga']");
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
   --  mucfgcheck-device.ads:68:4:Device_Memory_References
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
   procedure Test_PCI_Device_BDF_Uniqueness (Gnattest_T : in out Test);
   procedure Test_PCI_Device_BDF_Uniqueness_bef97c (Gnattest_T : in out Test) renames Test_PCI_Device_BDF_Uniqueness;
--  id:2.2/bef97c6f1475ed8d/PCI_Device_BDF_Uniqueness/1/0/
   procedure Test_PCI_Device_BDF_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:71:4:PCI_Device_BDF_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device/pci[@device='16#19#']",
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
   --  mucfgcheck-device.ads:75:4:Device_Reference_BDF_Uniqueness
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
   --  mucfgcheck-device.ads:79:4:PCI_Device_References
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
            XPath => "/system/hardware/devices/device[@name='xhci']"),
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
   procedure Test_PCI_Multifunction_Device_Refs (Gnattest_T : in out Test);
   procedure Test_PCI_Multifunction_Device_Refs_dac944 (Gnattest_T : in out Test) renames Test_PCI_Multifunction_Device_Refs;
--  id:2.2/dac9448ced5afba2/PCI_Multifunction_Device_Refs/1/0/
   procedure Test_PCI_Multifunction_Device_Refs (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:83:4:PCI_Multifunction_Device_Refs
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      PCI_Multifunction_Device_Refs (XML_Data => Data);


      --  Check that no exception is raised when first function is not mapped.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device[@name='ethernet']/pci",
         Name  => "function",
         Value => "2");

      PCI_Multifunction_Device_Refs (XML_Data => Data);

      Logical_Device_Nr_Mismatch:
      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/devices/device"
            & "[@logical='eth1']/pci",
            Name  => "device",
            Value => "16#1f#");

         PCI_Multifunction_Device_Refs (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E) =
                      "Logical devices 'eth1' and 'eth0' are part of a PCI "
                    & "multi-function device and must have the same logical "
                    & "device number: 16#1f# /= 16#19#",
                    Message   => "Exception mismatch");
      end Logical_Device_Nr_Mismatch;

      Subject_Mismatch:
      declare
         Dev_PCI : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device[@name='wireless']/pci");
      begin
         DOM.Core.Elements.Set_Attribute (Elem  => Dev_PCI,
                                          Name  => "bus",
                                          Value => "16#00#");
         DOM.Core.Elements.Set_Attribute (Elem  => Dev_PCI,
                                          Name  => "device",
                                          Value => "16#19#");
         DOM.Core.Elements.Set_Attribute (Elem  => Dev_PCI,
                                          Name  => "function",
                                          Value => "16#1#");

         PCI_Multifunction_Device_Refs (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E) =
                      "Physical devices 'ethernet_2' and 'wireless' are part "
                    & "of a PCI multi-function device and must be assigned "
                    & "to the same subject",
                    Message   => "Exception mismatch");
      end Subject_Mismatch;
--  begin read only
   end Test_PCI_Multifunction_Device_Refs;
--  end read only


--  begin read only
   procedure Test_Legacy_Device_References (Gnattest_T : in out Test);
   procedure Test_Legacy_Device_References_73e649 (Gnattest_T : in out Test) renames Test_Legacy_Device_References;
--  id:2.2/73e6491f4fa978a4/Legacy_Device_References/1/0/
   procedure Test_Legacy_Device_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:87:4:Legacy_Device_References
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
   --  mucfgcheck-device.ads:90:4:Device_References_PCI_Bus_Number
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Device_References_PCI_Bus_Number (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/"
         & "device[@physical='xhci']/pci",
         Name  => "bus",
         Value => "16#04#");

      begin
         Device_References_PCI_Bus_Number (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical PCI device 'xhci' of subject 'linux' specifies"
                    & " invalid bus number 16#04# should be 16#00#",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Device_References_PCI_Bus_Number;
--  end read only


--  begin read only
   procedure Test_Debugconsole_Presence (Gnattest_T : in out Test);
   procedure Test_Debugconsole_Presence_b13687 (Gnattest_T : in out Test) renames Test_Debugconsole_Presence;
--  id:2.2/b13687f7ed7372fc/Debugconsole_Presence/1/0/
   procedure Test_Debugconsole_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:93:4:Debugconsole_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/devices/device[@logical='debugconsole']",
         Name  => "logical",
         Value => "foobar");

      begin
         Debugconsole_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel device 'debugconsole' with I/O port resource"
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
   --  mucfgcheck-device.ads:96:4:IOMMU_Region_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device[@name='iommu_1']/memory",
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
end Mucfgcheck.Device.Test_Data.Tests;
