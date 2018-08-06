--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Hardware.Test_Data.

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
package body Mucfgcheck.Hardware.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Memory_Space (Gnattest_T : in out Test);
   procedure Test_Memory_Space_20d775 (Gnattest_T : in out Test) renames Test_Memory_Space;
--  id:2.2/20d775fbae27b871/Memory_Space/1/0/
   procedure Test_Memory_Space (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:25:4:Memory_Space
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/memory/"
         & "memoryBlock[@name='extended_mem_1']",
         Name  => "size",
         Value => "16#1000#");

      begin
         Memory_Space (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Allocated 16#13ed_9000# bytes of physical memory but "
                    & "only 16#042a_1000# bytes available by the hardware",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Memory_Space;
--  end read only


--  begin read only
   procedure Test_Memory_Block_Overlap (Gnattest_T : in out Test);
   procedure Test_Memory_Block_Overlap_12597b (Gnattest_T : in out Test) renames Test_Memory_Block_Overlap;
--  id:2.2/12597bd947918ca6/Memory_Block_Overlap/1/0/
   procedure Test_Memory_Block_Overlap (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:28:4:Memory_Block_Overlap
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/memory/memoryBlock[@name='base_mem']",
         Name  => "size",
         Value => "16#1000_0000#");

      begin
         Memory_Block_Overlap (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of hardware memory block 'base_mem' and"
                    & " 'extended_mem_1'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Memory_Block_Overlap;
--  end read only


--  begin read only
   procedure Test_Memory_Block_Size (Gnattest_T : in out Test);
   procedure Test_Memory_Block_Size_2aa436 (Gnattest_T : in out Test) renames Test_Memory_Block_Size;
--  id:2.2/2aa436e73a56a43f/Memory_Block_Size/1/0/
   procedure Test_Memory_Block_Size (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:31:4:Memory_Block_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Memory_Block_Size (XML_Data => Data);

      --  Set invalid memory block size.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/memory/memoryBlock",
         Name  => "size",
         Value => "16#0123#");

      begin
         Memory_Block_Size (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0123#' of 'base_mem' hardware "
                    & "memory block element not multiple of page size (4K)",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Memory_Block_Size;
--  end read only


--  begin read only
   procedure Test_PCI_Config_Space_Address (Gnattest_T : in out Test);
   procedure Test_PCI_Config_Space_Address_4663d9 (Gnattest_T : in out Test) renames Test_PCI_Config_Space_Address;
--  id:2.2/4663d97b4d1f43a4/PCI_Config_Space_Address/1/0/
   procedure Test_PCI_Config_Space_Address (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:35:4:PCI_Config_Space_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices",
         Name  => "pciConfigAddress",
         Value => "");

      begin
         PCI_Config_Space_Address (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Missing PCI configuration space address",
                    Message   => "Exception mismatch");
      end;

--  begin read only
   end Test_PCI_Config_Space_Address;
--  end read only


--  begin read only
   procedure Test_CPU_Count (Gnattest_T : in out Test);
   procedure Test_CPU_Count_07d30c (Gnattest_T : in out Test) renames Test_CPU_Count;
--  id:2.2/07d30c7e1521c027/CPU_Count/1/0/
   procedure Test_CPU_Count (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:38:4:CPU_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor",
         Name  => "cpuCores",
         Value => "2");

      begin
         CPU_Count (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "System requires 4 but hardware only provides 2 CPU(s)",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_CPU_Count;
--  end read only


--  begin read only
   procedure Test_CPU_Sub_Elements (Gnattest_T : in out Test);
   procedure Test_CPU_Sub_Elements_986048 (Gnattest_T : in out Test) renames Test_CPU_Sub_Elements;
--  id:2.2/9860488403fe8fc6/CPU_Sub_Elements/1/0/
   procedure Test_CPU_Sub_Elements (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:41:4:CPU_Sub_Elements
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      CPU_Sub_Elements (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor/cpu[@apicId='0']",
         Name  => "apicId",
         Value => "23");

      begin
         CPU_Sub_Elements (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "CPU with APIC ID 0 not present in active CPU set",
                    Message   => "Exception mismatch (1)");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor/cpu[@apicId='23']",
         Name  => "apicId",
         Value => "0");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor",
         Name  => "cpuCores",
         Value => "5");

      begin
         CPU_Sub_Elements (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Hardware processor element requires 5 CPU sub-elements, "
                    & "but 4 given",
                    Message   => "Exception mismatch (2)");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor",
         Name  => "cpuCores",
         Value => "4");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor/cpu[@apicId='6']",
         Name  => "cpuId",
         Value => "22");

      begin
         CPU_Sub_Elements (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Processor CPU IDs not consecutive",
                    Message   => "Exception mismatch (3)");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor/cpu[@apicId='6']",
         Name  => "cpuId",
         Value => "3");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor/cpu[@apicId='4']",
         Name  => "apicId",
         Value => "1");

      begin
         CPU_Sub_Elements (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (4)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Processor CPU sub-element with CPU ID 2 has uneven APIC"
                    & " ID 1",
                    Message   => "Exception mismatch (4)");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor/cpu[@apicId='1']",
         Name  => "apicId",
         Value => "0");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor/cpu[@cpuId='0']",
         Name  => "cpuId",
         Value => "4");

      begin
         CPU_Sub_Elements (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (5)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "CPU sub-element with CPU ID 0 not found",
                    Message   => "Exception mismatch (5)");
      end;
--  begin read only
   end Test_CPU_Sub_Elements;
--  end read only


--  begin read only
   procedure Test_IOAPIC_Presence (Gnattest_T : in out Test);
   procedure Test_IOAPIC_Presence_a2d33d (Gnattest_T : in out Test) renames Test_IOAPIC_Presence;
--  id:2.2/a2d33d83826a54a5/IOAPIC_Presence/1/0/
   procedure Test_IOAPIC_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:44:4:IOAPIC_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      IOAPIC_Presence (XML_Data => Data);

      Missing_Memory:
      begin
         Muxml.Utils.Remove_Elements
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device[@name='ioapic']"
            & "/memory");

         IOAPIC_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "I/O APIC device 'ioapic' has no memory region",
                    Message   => "Exception mismatch (1)");
      end Missing_Memory;

      Too_Few_Devices:
      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device/capabilities/"
            & "capability[@name='ioapic']",
            Name  => "name",
            Value => "foo");

         IOAPIC_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "I/O APIC count is 0 but must be at least 1",
                    Message   => "Exception mismatch (2)");
      end Too_Few_Devices;
--  begin read only
   end Test_IOAPIC_Presence;
--  end read only


--  begin read only
   procedure Test_IOMMU_Presence (Gnattest_T : in out Test);
   procedure Test_IOMMU_Presence_6c934e (Gnattest_T : in out Test) renames Test_IOMMU_Presence;
--  id:2.2/6c934e0540bf7353/IOMMU_Presence/1/0/
   procedure Test_IOMMU_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:47:4:IOMMU_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      IOMMU_Presence (XML_Data => Data);

      Missing_Memory:
      begin
         Muxml.Utils.Remove_Elements
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device[@name='iommu_2']"
            & "/memory");
         IOMMU_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "IOMMU device 'iommu_2' has no memory region",
                    Message   => "Exception mismatch (1)");
      end Missing_Memory;

      Too_Many_Devices:
      declare
         New_Dev : DOM.Core.Node
           := DOM.Core.Documents.Create_Element
             (Doc      => Data.Doc,
              Tag_Name => "device");
         Caps : constant DOM.Core.Node
           := DOM.Core.Documents.Create_Element
             (Doc      => Data.Doc,
              Tag_Name => "capabilities");
         Node : DOM.Core.Node;
      begin
         Node := DOM.Core.Nodes.Append_Child
           (N         => Caps,
            New_Child => DOM.Core.Documents.Create_Element
              (Doc      => Data.Doc,
               Tag_Name => "capability"));
         DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                          Name  => "name",
                                          Value => "iommu");
         Muxml.Utils.Append_Child
           (Node      => New_Dev,
            New_Child => Caps);
         Node := Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "mmio",
            Physical_Name => "mmio",
            Address       => "16#2000#",
            Writable      => True,
            Executable    => False);
         Muxml.Utils.Append_Child
           (Node      => New_Dev,
            New_Child => Node);
         for I in 1 .. 9 loop
            Muxml.Utils.Append_Child
              (Node      => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/hardware/devices"),
               New_Child => DOM.Core.Nodes.Clone_Node
                 (N    => New_Dev,
                  Deep => True));
         end loop;

         IOMMU_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "IOMMU count is 11 but must not be larger than 8",
                    Message   => "Exception mismatch (2)");
      end Too_Many_Devices;

      Too_Few_Devices:
      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/hardware/devices/device/capabilities/"
            & "capability[@name='iommu']",
            Name  => "name",
            Value => "foo");

         IOMMU_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "IOMMU count is 0 but must be at least 1",
                    Message   => "Exception mismatch (3)");
      end Too_Few_Devices;
--  begin read only
   end Test_IOMMU_Presence;
--  end read only


--  begin read only
   procedure Test_IOMMU_Cap_Agaw (Gnattest_T : in out Test);
   procedure Test_IOMMU_Cap_Agaw_f3e91e (Gnattest_T : in out Test) renames Test_IOMMU_Cap_Agaw;
--  id:2.2/f3e91eeb5d9a71cb/IOMMU_Cap_Agaw/1/0/
   procedure Test_IOMMU_Cap_Agaw (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:51:4:IOMMU_Cap_Agaw
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
      Cap  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      IOMMU_Cap_Agaw (XML_Data => Data);

      --  Unknown AGAW value.

      Cap := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device[@name='iommu_1']/"
         & "capabilities/capability[@name='agaw']/text()");

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "51");
      begin
         IOMMU_Cap_Agaw (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "AGAW capability of IOMMU 'iommu_1' set to invalid "
                    & "value '51'",
                    Message   => "Exception mismatch");
      end;

      --  AGAW value not set.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "");
      begin
         IOMMU_Cap_Agaw (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "AGAW capability of IOMMU 'iommu_1' is not set",
                    Message   => "Exception mismatch");
      end;

      --  Differing AGAW values.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "48");
      begin
         IOMMU_Cap_Agaw (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "IOMMUs have different AGAW capabilities set ('48' vs. "
                    & "'39')",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_IOMMU_Cap_Agaw;
--  end read only


--  begin read only
   procedure Test_IOMMU_Cap_Register_Offsets (Gnattest_T : in out Test);
   procedure Test_IOMMU_Cap_Register_Offsets_8d8dd2 (Gnattest_T : in out Test) renames Test_IOMMU_Cap_Register_Offsets;
--  id:2.2/8d8dd224a6cf5960/IOMMU_Cap_Register_Offsets/1/0/
   procedure Test_IOMMU_Cap_Register_Offsets (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:54:4:IOMMU_Cap_Register_Offsets
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
      Cap  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      IOMMU_Cap_Register_Offsets (XML_Data => Data);

      Cap := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device[@name='iommu_1']/"
         & "capabilities/capability[@name='fr_offset']/text()");

      --  FRO value not set.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "");
      begin
         IOMMU_Cap_Register_Offsets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Capability 'fr_offset' of IOMMU 'iommu_1' is not set",
                    Message   => "Exception mismatch (1)");
      end;
      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "512");

      --  FRO invalid.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "16369");
      begin
         IOMMU_Cap_Register_Offsets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Capability 'fr_offset' of IOMMU 'iommu_1' not in "
                    & "allowed range 0 .. 16368",
                    Message   => "Exception mismatch (2)");
      end;
      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "512");

      Cap := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/hardware/devices/device[@name='iommu_1']/"
         & "capabilities/capability[@name='iotlb_invalidate_offset']/text()");

      --  IOTLB invalidate register offset not set.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "");
      begin
         IOMMU_Cap_Register_Offsets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Capability 'iotlb_invalidate_offset' of IOMMU 'iommu_1'"
                    & " is not set",
                    Message   => "Exception mismatch (3)");
      end;
      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "264");

      --  IOTLB invalidate register offset invalid.

      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "16377");
      begin
         IOMMU_Cap_Register_Offsets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (4)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Capability 'iotlb_invalidate_offset' of IOMMU 'iommu_1'"
                    & " not in allowed range 0 .. 16376",
                    Message   => "Exception mismatch");
      end;
      DOM.Core.Nodes.Set_Node_Value (N     => Cap,
                                     Value => "264");
--  begin read only
   end Test_IOMMU_Cap_Register_Offsets;
--  end read only


--  begin read only
   procedure Test_System_Board_Presence (Gnattest_T : in out Test);
   procedure Test_System_Board_Presence_06a6c3 (Gnattest_T : in out Test) renames Test_System_Board_Presence;
--  id:2.2/06a6c3de430e8a9f/System_Board_Presence/1/0/
   procedure Test_System_Board_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-hardware.ads:58:4:System_Board_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      System_Board_Presence (XML_Data => Policy);

      Muxml.Utils.Remove_Child
        (Node       => Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/devices/device[capabilities/"
            & "capability/@name='systemboard']"),
         Child_Name => "capabilities");

      begin
         System_Board_Presence (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "System board device with reset/poweroff configuration "
                    & "missing or incomplete",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_System_Board_Presence;
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
end Mucfgcheck.Hardware.Test_Data.Tests;
