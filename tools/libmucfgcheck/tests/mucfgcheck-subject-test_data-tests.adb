--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Subject.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Mucfgcheck.Validation_Errors;
--  begin read only
--  end read only
package body Mucfgcheck.Subject.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Name_Uniqueness_7f1559 (Gnattest_T : in out Test) renames Test_Name_Uniqueness;
--  id:2.2/7f15594730cfb6f0/Name_Uniqueness/1/0/
   procedure Test_Name_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Name_Uniqueness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']",
         Name  => "name",
         Value => "linux");

      Name_Uniqueness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Subjects with global ID 1 and 4 have identical name "
               & "'linux'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_CPU_ID (Gnattest_T : in out Test);
   procedure Test_CPU_ID_40ceb3 (Gnattest_T : in out Test) renames Test_CPU_ID;
--  id:2.2/40ceb37195f33af8/CPU_ID/1/0/
   procedure Test_CPU_ID (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      CPU_ID (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']",
         Name  => "cpu",
         Value => "7");

      CPU_ID (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Attribute 'cpu => 7' of 'linux' subject element not in "
               & "valid range 0 .. 3"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_CPU_ID;
--  end read only


--  begin read only
   procedure Test_Global_ID_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Global_ID_Uniqueness_c38243 (Gnattest_T : in out Test) renames Test_Global_ID_Uniqueness;
--  id:2.2/c3824316af697788/Global_ID_Uniqueness/1/0/
   procedure Test_Global_ID_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Global_ID_Uniqueness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='sm']",
         Name  => "globalId",
         Value => "0");

      Global_ID_Uniqueness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Subjects 'tau0' and 'sm' have identical global ID 0"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Global_ID_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Local_ID_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Local_ID_Uniqueness_2b85f4 (Gnattest_T : in out Test) renames Test_Local_ID_Uniqueness;
--  id:2.2/2b85f4d3f032afd5/Local_ID_Uniqueness/1/0/
   procedure Test_Local_ID_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Local_ID_Uniqueness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']",
         Name  => "localId",
         Value => "0");

      Local_ID_Uniqueness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Subjects 'sm' and 'linux' running on CPU 1 have "
               & "identical local ID 0"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Local_ID_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Memory_Types (Gnattest_T : in out Test);
   procedure Test_Memory_Types_0a6ddc (Gnattest_T : in out Test) renames Test_Memory_Types;
--  id:2.2/0a6ddce5b8b8256a/Memory_Types/1/0/
   procedure Test_Memory_Types (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Memory_Types (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vt|bin']",
         Name  => "type",
         Value => "system_pt");

      Memory_Types (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Logical memory region 'binary' of subject 'vt' mapping "
               & "physical region 'vt|bin' has invalid type system_pt"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Memory_Types;
--  end read only


--  begin read only
   procedure Test_No_IOMMU_Device_References (Gnattest_T : in out Test);
   procedure Test_No_IOMMU_Device_References_c1578b (Gnattest_T : in out Test) renames Test_No_IOMMU_Device_References;
--  id:2.2/c1578b1a998ee62a/No_IOMMU_Device_References/1/0/
   procedure Test_No_IOMMU_Device_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      No_IOMMU_Device_References (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/device"
         & "[@physical='wireless']",
         Name  => "physical",
         Value => "iommu_1");

      No_IOMMU_Device_References (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "IOMMU device referenced by subject 'vt'"),
               Message   => "Exception mismatch");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/device"
         & "[@physical='ethernet']",
         Name  => "physical",
         Value => "iommu_2");

      No_IOMMU_Device_References (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "IOMMU device referenced by subjects 'vt', 'linux'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_No_IOMMU_Device_References;
--  end read only


--  begin read only
   procedure Test_Runnability (Gnattest_T : in out Test);
   procedure Test_Runnability_97c1af (Gnattest_T : in out Test) renames Test_Runnability;
--  id:2.2/97c1af40316dadad/Runnability/1/0/
   procedure Test_Runnability (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Must not raise an exception.

      Runnability (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='trap_to_sm']",
         Name  => "mode",
         Value => "ipi");

      Runnability (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Subject 'sm' is neither referenced in the scheduling"
               & " plan nor schedulable via switch events"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Runnability;
--  end read only


--  begin read only
   procedure Test_Scheduling_Group_IDs (Gnattest_T : in out Test);
   procedure Test_Scheduling_Group_IDs_a95cac (Gnattest_T : in out Test) renames Test_Scheduling_Group_IDs;
--  id:2.2/a95cac081e2db180/Scheduling_Group_IDs/1/0/
   procedure Test_Scheduling_Group_IDs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Must not raise an exception.

      Scheduling_Group_IDs (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='time']",
         Name  => "schedGroupId",
         Value => "2");

      Scheduling_Group_IDs (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Subject 'time' has unexpected scheduling group ID: "
               & "2 /= 3"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Scheduling_Group_IDs;
--  end read only


--  begin read only
   procedure Test_Logical_Device_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Logical_Device_Name_Uniqueness_aaba9c (Gnattest_T : in out Test) renames Test_Logical_Device_Name_Uniqueness;
--  id:2.2/aaba9caac93471a1/Logical_Device_Name_Uniqueness/1/0/
   procedure Test_Logical_Device_Name_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Logical_Device_Name_Uniqueness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']/devices/"
         & "device[@logical='vga']",
         Name  => "logical",
         Value => "wireless");

      Logical_Device_Name_Uniqueness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Subject 'vt' has devices with identical logical names"
               & " 'wireless'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Logical_Device_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Logical_IRQ_MSI_Consecutiveness (Gnattest_T : in out Test);
   procedure Test_Logical_IRQ_MSI_Consecutiveness_907fb8 (Gnattest_T : in out Test) renames Test_Logical_IRQ_MSI_Consecutiveness;
--  id:2.2/907fb8e6faa0778a/Logical_IRQ_MSI_Consecutiveness/1/0/
   procedure Test_Logical_IRQ_MSI_Consecutiveness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Logical_IRQ_MSI_Consecutiveness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/devices/device"
            & "[@physical='xhci']/irq",
            Name  => "vector",
            Value => "254");

      Logical_IRQ_MSI_Consecutiveness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "MSI IRQ 'xhci_irq1' of logical device 'xhci' of subject"
               & " 'linux' not adjacent to other IRQs"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Logical_IRQ_MSI_Consecutiveness;
--  end read only


--  begin read only
   procedure Test_Logical_Unmask_Event (Gnattest_T : in out Test);
   procedure Test_Logical_Unmask_Event_26684d (Gnattest_T : in out Test) renames Test_Logical_Unmask_Event;
--  id:2.2/26684dd08b276bd8/Logical_Unmask_Event/1/0/
   procedure Test_Logical_Unmask_Event (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Logical_Unmask_Event (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Set mistmatching physical IRQ number as suffix.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/"
         & "event[@logical='unmask_irq_57']/unmask_irq",
         Name  => "number",
         Value => "37");

      Logical_Unmask_Event (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Logical event 'unmask_irq_57' of subject 'vt' "
               & "referencing logical IRQ wireless->irq has unmask action"
               & " number different from physical IRQ wireless->irq: 37, "
               & "expected 21"),
              Message   => "Exception mismatch (IRQ mismatch 1)");

      --  Set mistmatching logical IRQ number as suffix
      --  (unresolvable vector).

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/"
         & "event[@logical='unmask_irq_57']",
         Name  => "logical",
         Value => "unmask_irq_255");

      Logical_Unmask_Event (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Logical event 'unmask_irq_255' of subject 'vt' "
               & "references invalid logical IRQ with vector 255 as "
               & "logical name suffix"),
              Message   => "Exception mismatch (IRQ mismatch 2)");

      --  Mistmatching logical IRQ number as suffix (resolvable vector).

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/"
         & "event[@logical='unmask_irq_255']/unmask_irq",
         Name  => "number",
         Value => "21");

      Logical_Unmask_Event (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Logical event 'unmask_irq_255' of subject 'vt' "
               & "references invalid logical IRQ with vector 255 as "
               & "logical name suffix: expected 57"),
              Message   => "Exception mismatch (IRQ mismatch 3)");

      --  Additionally, make expected logical IRQ number non-resolvable.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/device"
         & "/irq[@logical='wlan_irq']",
         Name  => "physical",
         Value => "nonexistent");

      Logical_Unmask_Event (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Logical event 'unmask_irq_255' of subject 'vt' "
               & "references invalid logical IRQ with vector 255 as "
               & "logical name suffix"),
              Message   => "Exception mismatch (IRQ mismatch 4)");

      --  Set non-numeric suffix.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/"
         & "event[starts-with(@logical,'unmask_irq')]",
         Name  => "logical",
         Value => "unmask_irq_foobar");

      Logical_Unmask_Event (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Logical event 'unmask_irq_foobar' of subject 'vt' "
               & "has invalid suffix 'foobar': must match number of "
               & "corresponding logical IRQ vector"),
              Message   => "Exception mismatch (Non-numeric suffix)");

      --  Set non-matching prefix.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/"
         & "event[starts-with(@logical,'unmask_irq')]",
         Name  => "logical",
         Value => "foobar_irq");

      Logical_Unmask_Event (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Logical event 'foobar_irq' of subject 'vt' has "
               & "unexpected logical name: must have the form "
               & "'unmask_irq_$VECTORNR'"),
              Message   => "Exception mismatch (Unexpected prefix)");
--  begin read only
   end Test_Logical_Unmask_Event;
--  end read only


--  begin read only
   procedure Test_Virtual_Memory_Overlap (Gnattest_T : in out Test);
   procedure Test_Virtual_Memory_Overlap_7973e4 (Gnattest_T : in out Test) renames Test_Virtual_Memory_Overlap;
--  id:2.2/7973e4663e077f6d/Virtual_Memory_Overlap/1/0/
   procedure Test_Virtual_Memory_Overlap (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Virtual_Memory_Overlap (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/memory/"
         & "memory[@physical='linux|bin']",
         Name  => "virtualAddress",
         Value => "16#0000#");

      Virtual_Memory_Overlap (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Overlap of virtual memory region 'binary' and "
               & "'zero_page' of subject 'linux'"),
              Message   => "Exception mismatch");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/memory/"
         & "memory[@physical='vt|bin']",
         Name  => "virtualAddress",
         Value => "16#000b_7000#");

      Virtual_Memory_Overlap (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Overlap of virtual memory region 'binary' and "
               & "'vga->buffer' of subject 'vt'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Virtual_Memory_Overlap;
--  end read only


--  begin read only
   procedure Test_Initramfs_Consecutiveness (Gnattest_T : in out Test);
   procedure Test_Initramfs_Consecutiveness_5df077 (Gnattest_T : in out Test) renames Test_Initramfs_Consecutiveness;
--  id:2.2/5df0773597bd5b45/Initramfs_Consecutiveness/1/0/
   procedure Test_Initramfs_Consecutiveness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Initramfs_Consecutiveness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@type='subject_initrd']",
         Name  => "size",
         Value => "16#2000#");

      Initramfs_Consecutiveness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Initramfs region 'initramfs1' not adjacent to other "
               & "initramfs regions"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Initramfs_Consecutiveness;
--  end read only


--  begin read only
   procedure Test_Crash_Audit_Write_Access (Gnattest_T : in out Test);
   procedure Test_Crash_Audit_Write_Access_73ee35 (Gnattest_T : in out Test) renames Test_Crash_Audit_Write_Access;
--  id:2.2/73ee35add9e33339/Crash_Audit_Write_Access/1/0/
   procedure Test_Crash_Audit_Write_Access (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Crash_Audit_Write_Access (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/memory/memory"
         & "[@logical='crash_audit']",
         Name  => "writable",
         Value => "true");

      Crash_Audit_Write_Access (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Logical memory node 'crash_audit' of subject 'tau0' "
               & "declares illegal write access to crash audit region"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Crash_Audit_Write_Access;
--  end read only


--  begin read only
   procedure Test_Device_Mmconf_Mappings (Gnattest_T : in out Test);
   procedure Test_Device_Mmconf_Mappings_005789 (Gnattest_T : in out Test) renames Test_Device_Mmconf_Mappings;
--  id:2.2/00578947f4562d09/Device_Mmconf_Mappings/1/0/
   procedure Test_Device_Mmconf_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Device_Mmconf_Mappings (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']/devices/device/memory"
         & "[@logical='mmconf']",
         Name  => "virtualAddress",
         Value => "16#dead_beef#");

      Device_Mmconf_Mappings (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "PCI mmconf region of subject 'vt' logical device "
               & "'wireless' is 16#dead_beef# but should be "
               & "16#f80d_0000#"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Device_Mmconf_Mappings;
--  end read only


--  begin read only
   procedure Test_VMX_Controls_Entry_Checks (Gnattest_T : in out Test);
   procedure Test_VMX_Controls_Entry_Checks_6dde9d (Gnattest_T : in out Test) renames Test_VMX_Controls_Entry_Checks;
--  id:2.2/6dde9d8234a9c19e/VMX_Controls_Entry_Checks/1/0/
   procedure Test_VMX_Controls_Entry_Checks (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Dual_Monitor_Treatment
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/entry/DeactiveDualMonitorTreatment",
            Value => "1");
         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'deactivate dual-monitor treatment' of "
                  & "subject 'linux' is 1"),
                 Message   => "Exception mismatch (Dual-Monitor)");
      end Dual_Monitor_Treatment;

      ----------------------------------------------------------------------

      procedure Entry_To_SMM
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/entry/EntryToSMM",
            Value => "1");
         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'entry to SMM' of subject 'linux' is 1"),
                 Message   => "Exception mismatch (Entry to SMM)");
      end Entry_To_SMM;

      ----------------------------------------------------------------------

      procedure IO_Bitmap_Address
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/memory/memory[@name='linux|iobm']",
            Name  => "physicalAddress",
            Value => "16#0001_0001#");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "Address of I/O Bitmap of subject 'linux' invalid: bits "
                  & "11:0 must be zero"),
                 Message   => "Exception mismatch (IOBM)");
      end IO_Bitmap_Address;

      ----------------------------------------------------------------------

      procedure MSR_Bitmap_Address
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/memory/memory[@name='linux|msrbm']",
            Name  => "physicalAddress",
            Value => "16#0001_0001#");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "Address of MSR Bitmap of subject 'linux' invalid: bits "
                  & "11:0 must be zero"),
                 Message   => "Exception mismatch (MSRBM)");
      end MSR_Bitmap_Address;

      ----------------------------------------------------------------------

      procedure MSR_Storage_Address
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/memory/memory[@name='linux|msrstore']",
            Name  => "physicalAddress",
            Value => "16#0001_0001#");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "MSR Store address of subject 'linux' invalid: bits "
                  & "3:0 must be zero"),
                 Message   => "Exception mismatch (MSR Store)");
      end MSR_Storage_Address;

      ----------------------------------------------------------------------

      procedure NMI_Exiting
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/pin/NMIExiting",
            Value => "0");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/pin/VirtualNMIs",
            Value => "1");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'NMI-Exiting' is 0 for subject "
                  & "'linux' but 'Virtual NMIs' is 1"),
                 Message   => "Exception mismatch (NMI Exiting)");
      end NMI_Exiting;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Validation_Errors.Clear;

         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         --  Positive test, must not raise an exception.

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Is_Empty,
                 Message   => "Unexpected error in positive test");
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Posted_Interrupts
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/pin/ProcessPostedInterrupts",
            Value => "1");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'process posted interrupts' is 1 for "
                  & "subject 'linux' but 'virtual-interrupt delivery' is"
                  & " 0"),
                 Message   => "Exception mismatch (Posted Int 1)");

         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc2/VirtualInterruptDelivery",
            Value => "1");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc/UseTPRShadow",
            Value => "1");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/pin/ExternalInterruptExiting",
            Value => "1");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/exit/AckInterruptOnExit",
            Value => "0");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'process posted interrupts' is 1 for "
                  & "subject 'linux' but 'acknowledge interrupt on exit' "
                  & "is 0"),
                 Message   => "Exception mismatch (Posted Int 2)");
      end Posted_Interrupts;

      ----------------------------------------------------------------------

      procedure Preemption_Timer
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/pin/ActivateVMXTimer",
            Value => "0");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/exit/SaveVMXTimerValue",
            Value => "1");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'activate VMX-preemption timer' is 0 for "
                  & "subject 'linux' but 'save VMX-preemtion timer value' "
                  & "is 1"),
                 Message   => "Exception mismatch (VMX-preemption timer)");
      end Preemption_Timer;

      ----------------------------------------------------------------------

      procedure TPR_Shadow
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc2/VirtualInterruptDelivery",
            Value => "1");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'Use TPR Shadow' is 0 for subject "
                  & "'linux' but 'virtual-interrupt delivery' is 1"),
                 Message   => "Exception mismatch (TPR Shadow 1)");

         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc2/APICRegisterVirtualization",
            Value => "1");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'Use TPR Shadow' is 0 for subject "
                  & "'linux' but 'APIC-register virtualization' is 1"),
                 Message   => "Exception mismatch (TPR Shadow 2)");

         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc2/Virtualizex2APICMode",
            Value => "1");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'Use TPR Shadow' is 0 for subject"
                  & " 'linux' but 'Virtualize x2APIC mode' is 1"),
                 Message   => "Exception mismatch (TPR Shadow 3)");
      end TPR_Shadow;

      ----------------------------------------------------------------------

      procedure Unrestricted_Guest
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc2/UnrestrictedGuest",
            Value => "1");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc2/EnableEPT",
            Value => "0");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'unrestricted guest' is 1 for subject "
                  & "'linux' but 'Enable EPT' is 0"),
                 Message   => "Exception mismatch (Unrestricted Guest)");
      end Unrestricted_Guest;

      ----------------------------------------------------------------------

      procedure Virtual_Interrupt_Delivery
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc/UseTPRShadow",
            Value => "1");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc2/VirtualInterruptDelivery",
            Value => "1");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/pin/ExternalInterruptExiting",
            Value => "0");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'virtual-interrupt delivery' is 1 for "
                  & "subject 'linux' but 'external-interrupt exiting' is 0"),
                 Message   => "Exception mismatch (Virtual INT delivery)");
      end Virtual_Interrupt_Delivery;

      ----------------------------------------------------------------------

      procedure Virtual_NMIs
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/pin/VirtualNMIs",
            Value => "0");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc/NMIWindowExiting",
            Value => "1");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'Virtual NMIs' is 0 for subject 'linux' but"
                  & " 'NMI-window exiting' is 1"),
                 Message   => "Exception mismatch (Virtual NMIs)");
      end Virtual_NMIs;

      ----------------------------------------------------------------------

      procedure Virtualize_x2APIC_Mode
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc/UseTPRShadow",
            Value => "1");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc2/Virtualizex2APICMode",
            Value => "1");
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc2/VirtualAPICAccesses",
            Value => "1");

         VMX_Controls_Entry_Checks (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VMX control 'Virtualize x2APIC mode' is 1 for "
                  & "subject 'linux' but 'virtualize APIC accesses' is 1"),
                 Message   => "Exception mismatch (Virt x2APIC Mode)");
      end Virtualize_x2APIC_Mode;
   begin
      Positive_Test;
      IO_Bitmap_Address;
      MSR_Bitmap_Address;
      NMI_Exiting;
      Virtual_NMIs;
      TPR_Shadow;
      Virtualize_x2APIC_Mode;
      Virtual_Interrupt_Delivery;
      Posted_Interrupts;
      Unrestricted_Guest;
      Preemption_Timer;
      MSR_Storage_Address;
      Entry_To_SMM;
      Dual_Monitor_Treatment;
--  begin read only
   end Test_VMX_Controls_Entry_Checks;
--  end read only


--  begin read only
   procedure Test_VMX_Controls_Pin_Requirements (Gnattest_T : in out Test);
   procedure Test_VMX_Controls_Pin_Requirements_f8d4a7 (Gnattest_T : in out Test) renames Test_VMX_Controls_Pin_Requirements;
--  id:2.2/f8d4a7e6bdd87428/VMX_Controls_Pin_Requirements/1/0/
   procedure Test_VMX_Controls_Pin_Requirements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      VMX_Controls_Pin_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Posted-Interrupt Processing.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/pin/ProcessPostedInterrupts",
         Value => "1");

      VMX_Controls_Pin_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Pin-Based control 'Process posted interrupts' of "
               & "subject 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (Posted Int)");

      --  VMX-preemption timer.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/pin/ActivateVMXTimer",
         Value => "0");

      VMX_Controls_Pin_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Pin-Based control 'Activate VMX-preemption timer' of "
               & "subject 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (VMX Timer)");

      --  Virtual NMIs.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/pin/VirtualNMIs",
         Value => "1");

      VMX_Controls_Pin_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Pin-Based control 'Virtual NMIs' of subject 'linux' "
               & "invalid: must be 0"),
              Message   => "Exception mismatch (Virtual NMIs)");

      --  NMI exiting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/pin/NMIExiting",
         Value => "0");

      VMX_Controls_Pin_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Pin-Based control 'NMI exiting' of subject 'linux' "
               & "invalid: must be 1"),
              Message   => "Exception mismatch (NMI Exiting)");

      --  External-interrupt exiting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/pin/ExternalInterruptExiting",
         Value => "0");

      VMX_Controls_Pin_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Pin-Based control 'External-Interrupt exiting' of "
               & "subject 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (ExtInt)");
--  begin read only
   end Test_VMX_Controls_Pin_Requirements;
--  end read only


--  begin read only
   procedure Test_VMX_Controls_Proc_Requirements (Gnattest_T : in out Test);
   procedure Test_VMX_Controls_Proc_Requirements_b82eb6 (Gnattest_T : in out Test) renames Test_VMX_Controls_Proc_Requirements;
--  id:2.2/b82eb6a519d581d6/VMX_Controls_Proc_Requirements/1/0/
   procedure Test_VMX_Controls_Proc_Requirements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Secondary Proc controls.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/Activate2ndaryControls",
         Value => "0");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'Activate secondary controls' "
               & "of subject 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (2ndary Proc Ctrls)");

      --  MSR bitmaps.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/UseMSRBitmaps",
         Value => "0");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'Use MSR bitmaps' of "
               & "subject 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (MSR Bitmaps)");

      --  I/O bitmaps.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/UseIOBitmaps",
         Value => "0");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'Use I/O bitmaps' of "
               & "subject 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (I/O Bitmaps)");

      --  MOV-DR exiting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/MOVDRExiting",
         Value => "0");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'MOV-DR exiting' of "
               & "subject 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (MOV DR)");

      --  NMI-window exiting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/NMIWindowExiting",
         Value => "1");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'NMI-window exiting' of "
               & "subject 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (NMI Window)");

      --  TPR Shadow.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/UseTPRShadow",
         Value => "1");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'Use TPR shadow' of "
               & "subject 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (TPR Shadow)");

      --  CR8-store exiting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/CR8StoreExiting",
         Value => "0");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'CR8-store exiting' of "
               & "subject 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (CR8-store exiting)");

      --  CR8-load exiting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/CR8LoadExiting",
         Value => "0");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'CR8-load exiting' of "
               & "subject 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (CR8-load exiting)");

      --  CR3-load exiting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='sm']/vcpu/vmx/"
         & "controls/proc/CR3LoadExiting",
         Value => "0");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'CR3-load exiting' of "
               & "subject 'sm' invalid: must be 1"),
               Message   => "Exception mismatch (CR3-load exiting)");
      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='sm']/vcpu/vmx/"
         & "controls/proc/CR3LoadExiting",
         Value => "1");

      --  MWAIT exiting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/MWAITExiting",
         Value => "0");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'MWAIT exiting' of "
               & "subject 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (MWAIT exiting)");

      --  INVLPG exiting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/INVLPGExiting",
         Value => "0");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'INVLPG exiting' of "
               & "subject 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (INVLPG exiting)");

      --  TSC Offsetting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/UseTSCOffsetting",
         Value => "1");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'Use TSC offsetting' of "
               & "subject 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (TSC Offsetting)");

      --  Interrupt-window exiting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc/InterruptWindowExiting",
         Value => "1");

      VMX_Controls_Proc_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Processor-Based control 'Interrupt-window exiting' of "
               & "subject 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (Int Window)");
--  begin read only
   end Test_VMX_Controls_Proc_Requirements;
--  end read only


--  begin read only
   procedure Test_VMX_Controls_Proc2_Requirements (Gnattest_T : in out Test);
   procedure Test_VMX_Controls_Proc2_Requirements_11a81e (Gnattest_T : in out Test) renames Test_VMX_Controls_Proc2_Requirements;
--  id:2.2/11a81e439f6e1d42/VMX_Controls_Proc2_Requirements/1/0/
   procedure Test_VMX_Controls_Proc2_Requirements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      VMX_Controls_Proc2_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  VM Functions.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc2/EnableVMFunctions",
         Value => "1");

      VMX_Controls_Proc2_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Secondary Processor-Based control "
               & "'Enable VM functions' of subject 'linux' "
               & "invalid: must be 0"),
              Message   => "Exception mismatch (VMFUNC)");

      --  INVPCID.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc2/EnableINVPCID",
         Value => "1");

      VMX_Controls_Proc2_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Secondary Processor-Based control "
               & "'Enable INVPCID' of subject 'linux' "
               & "invalid: must be 0"),
              Message   => "Exception mismatch (INVPCID)");

      --  Virtual-interrupt delivery.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc2/VirtualInterruptDelivery",
         Value => "1");

      VMX_Controls_Proc2_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Secondary Processor-Based control "
               & "'Virtual-interrupt delivery' of subject 'linux' "
               & "invalid: must be 0"),
              Message   => "Exception mismatch (Virt Int Delivery)");

      --  APIC-register virtualization.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc2/APICRegisterVirtualization",
         Value => "1");

      VMX_Controls_Proc2_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Secondary Processor-Based control "
               & "'APIC-register virtualization' of subject 'linux' "
               & "invalid: must be 0"),
              Message   => "Exception mismatch (APIC Reg Virt)");

      --  WBINVD exiting.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc2/WBINVDExiting",
         Value => "0");

      VMX_Controls_Proc2_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Secondary Processor-Based control "
               & "'WBINVD exiting' of subject 'linux' "
               & "invalid: must be 1"),
              Message   => "Exception mismatch (WBINVD)");

      --  VPID.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc2/EnableVPID",
         Value => "1");

      VMX_Controls_Proc2_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Secondary Processor-Based control "
               & "'Enable VPID' of subject 'linux' "
               & "invalid: must be 0"),
              Message   => "Exception mismatch (VPID)");

      --  Virtualize x2APIC mode.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc2/Virtualizex2APICMode",
         Value => "1");

      VMX_Controls_Proc2_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Secondary Processor-Based control "
               & "'Virtualize x2APIC mode' of subject 'linux' "
               & "invalid: must be 0"),
              Message   => "Exception mismatch (Virt x2APIC mode)");

      --  Virtualize APIC Accesses.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/proc2/VirtualAPICAccesses",
         Value => "1");

      VMX_Controls_Proc2_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Secondary Processor-Based control "
               & "'Virtualize APIC accesses' of subject 'linux' "
               & "invalid: must be 0"),
              Message   => "Exception mismatch (Virt APIC access)");
--  begin read only
   end Test_VMX_Controls_Proc2_Requirements;
--  end read only


--  begin read only
   procedure Test_VM_Exit_Controls_Requirements (Gnattest_T : in out Test);
   procedure Test_VM_Exit_Controls_Requirements_432c58 (Gnattest_T : in out Test) renames Test_VM_Exit_Controls_Requirements;
--  id:2.2/432c58c1b884576d/VM_Exit_Controls_Requirements/1/0/
   procedure Test_VM_Exit_Controls_Requirements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      VM_Exit_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Save VMX-preemption timer.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/exit/SaveVMXTimerValue",
         Value => "1");

      VM_Exit_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Exit control 'Save VMX-preemption timer value' of "
               & "subject 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (VMX-preempt timer)");

      declare
         Node : DOM.Core.Node
           := DOM.Core.Documents.Create_Element (Doc      => Data.Doc,
                                                 Tag_Name => "msr");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => Mutools.Utils.To_Hex
              (Number => Mutools.Constants.IA32_EFER));
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "end",
            Value => Mutools.Utils.To_Hex
              (Number => Mutools.Constants.IA32_EFER));
         Muxml.Utils.Append_Child
           (Node      => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject[@name='linux']/vcpu/msrs"),
            New_Child => Node);
      end;

      --  Load IA32_EFER.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/exit/LoadIA32EFER",
         Value => "0");

      VM_Exit_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Exit control 'Load IA32_EFER' of subject"
               & " 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (Load IA32_EFER)");

      --  Save IA32_EFER.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/exit/SaveIA32EFER",
         Value => "0");

      VM_Exit_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Exit control 'Save IA32_EFER' of subject"
               & " 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (Save IA32_EFER)");

      --  Load IA32_PAT.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/exit/LoadIA32PAT",
         Value => "1");

      VM_Exit_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Exit control 'Load IA32_PAT' of subject"
               & " 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (Load IA32_PAT)");

      --  Save IA32_PAT.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/exit/SaveIA32PAT",
         Value => "1");

      VM_Exit_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Exit control 'Save IA32_PAT' of subject"
               & " 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (Save IA32_PAT)");

      --  Acknowledge interrupt on exit.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/exit/AckInterruptOnExit",
         Value => "0");

      VM_Exit_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Exit control 'Acknowledge interrupt on exit' of "
               & "subject 'linux' invalid: must be 1"),
              Message   => "Exception mismatch (Ack INT)");

      --  Load IA32_PERF_GLOBAL_CTRL.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/exit/LoadIA32PERFGLOBALCTRL",
         Value => "1");

      VM_Exit_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Exit control 'Load IA32_PERF_GLOBAL_CTRL' of subject"
               & " 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (IA32_PERF_GLOBAL_CTRL)");

      --  Host address-space size.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/exit/HostAddressspaceSize",
         Value => "0");

      VM_Exit_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Exit control 'Host address-space size' of subject "
               & "'linux' invalid: must be 1"),
              Message   => "Exception mismatch (Host Addr Space Size)");

      --  Save IA32_DEBUGCTL.

      declare
         Node : DOM.Core.Node
           := DOM.Core.Documents.Create_Element (Doc      => Data.Doc,
                                                 Tag_Name => "msr");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => Mutools.Utils.To_Hex
              (Number => Mutools.Constants.IA32_DEBUGCTL));
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "end",
            Value => Mutools.Utils.To_Hex
              (Number => Mutools.Constants.IA32_DEBUGCTL));
         Muxml.Utils.Append_Child
           (Node      => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject[@name='linux']/vcpu/msrs"),
            New_Child => Node);

         VM_Exit_Controls_Requirements (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VM-Exit control 'Save debug controls' of subject "
                  & "'linux' invalid: must be 1"),
                 Message   => "Exception mismatch (IA32_DEBUGCTL)");
      end;
--  begin read only
   end Test_VM_Exit_Controls_Requirements;
--  end read only


--  begin read only
   procedure Test_VM_Entry_Controls_Requirements (Gnattest_T : in out Test);
   procedure Test_VM_Entry_Controls_Requirements_422840 (Gnattest_T : in out Test) renames Test_VM_Entry_Controls_Requirements;
--  id:2.2/422840201cc0190f/VM_Entry_Controls_Requirements/1/0/
   procedure Test_VM_Entry_Controls_Requirements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      VM_Entry_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Load IA32_EFER.

      declare
         Node : DOM.Core.Node
           := DOM.Core.Documents.Create_Element (Doc      => Data.Doc,
                                                 Tag_Name => "msr");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => Mutools.Utils.To_Hex
              (Number => Mutools.Constants.IA32_EFER));
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "end",
            Value => Mutools.Utils.To_Hex
              (Number => Mutools.Constants.IA32_EFER));
         Muxml.Utils.Append_Child
           (Node      => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject[@name='linux']/vcpu/msrs"),
            New_Child => Node);
         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/entry/LoadIA32EFER",
            Value => "0");

         VM_Entry_Controls_Requirements (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VM-Entry control 'Load IA32_EFER' of subject"
                  & " 'linux' invalid: must be 1"),
                 Message   => "Exception mismatch (Load IA32_EFER)");
      end;

      --  Load IA32_PAT.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/entry/LoadIA32PAT",
         Value => "1");

      VM_Entry_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Entry control 'Load IA32_PAT' of "
               & "subject 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (IA32_PAT)");

      --  Load IA32_PERF_GLOBAL_CTRL.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/entry/LoadIA32PERFGLOBALCTRL",
         Value => "1");

      VM_Entry_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Entry control 'Load IA32_PERF_GLOBAL_CTRL' of "
               & "subject 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (IA32_PERF_GLOBAL_CTRL)");

      --  Dual-Monitor treatment.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/entry/DeactiveDualMonitorTreatment",
         Value => "1");

      VM_Entry_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Entry control 'Deactivate dual-monitor treatment' of"
               & " subject 'linux' invalid: must be 0"),
              Message   => "Exception mismatch (Dual-Monitor)");

      --  Entry to SMM.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "controls/entry/EntryToSMM",
         Value => "1");

      VM_Entry_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Entry control 'Entry to SMM' of subject "
               & "'linux' invalid: must be 0"),
              Message   => "Exception mismatch (Entry to SMM)");

      --  IA-32e guest mode.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']/vcpu/vmx/"
         & "controls/entry/IA32eModeGuest",
         Value => "0");

      VM_Entry_Controls_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VM-Entry control 'IA-32e mode guest' of subject "
               & "'vt' invalid: must be 1"),
              Message   => "Exception mismatch (IA-32e guest)");
      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']/vcpu/vmx/"
         & "controls/entry/IA32eModeGuest",
         Value => "1");

      --  Load IA32_DEBUGCTL.

      declare
         Node : DOM.Core.Node
           := DOM.Core.Documents.Create_Element (Doc      => Data.Doc,
                                                 Tag_Name => "msr");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => Mutools.Utils.To_Hex
              (Number => Mutools.Constants.IA32_DEBUGCTL));
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "end",
            Value => Mutools.Utils.To_Hex
              (Number => Mutools.Constants.IA32_DEBUGCTL));
         Muxml.Utils.Append_Child
           (Node      => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject[@name='linux']/vcpu/msrs"),
            New_Child => Node);

         VM_Entry_Controls_Requirements (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "VM-Entry control 'Load debug controls' of subject "
                  & "'linux' invalid: must be 1"),
                 Message   => "Exception mismatch (IA32_DEBUGCTL)");
      end;
--  begin read only
   end Test_VM_Entry_Controls_Requirements;
--  end read only


--  begin read only
   procedure Test_VMX_CR0_Mask_Requirements (Gnattest_T : in out Test);
   procedure Test_VMX_CR0_Mask_Requirements_ac4b69 (Gnattest_T : in out Test) renames Test_VMX_CR0_Mask_Requirements;
--  id:2.2/ac4b692b1a57841d/VMX_CR0_Mask_Requirements/1/0/
   procedure Test_VMX_CR0_Mask_Requirements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      VMX_CR0_Mask_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Cache Disable.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "masks/cr0/CacheDisable",
         Value => "0");

      VMX_CR0_Mask_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VMX CR0 guest/host mask control "
               & "'Cache Disable' of subject 'linux' "
               & "invalid: must be 1"),
              Message   => "Exception mismatch (CD)");

      --  Not Write-through.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
         & "masks/cr0/NotWritethrough",
         Value => "0");

      VMX_CR0_Mask_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VMX CR0 guest/host mask control "
               & "'Not Write-through' of subject 'linux' "
               & "invalid: must be 1"),
              Message   => "Exception mismatch (NW)");
--  begin read only
   end Test_VMX_CR0_Mask_Requirements;
--  end read only


--  begin read only
   procedure Test_VMX_CR4_Mask_Requirements (Gnattest_T : in out Test);
   procedure Test_VMX_CR4_Mask_Requirements_6294be (Gnattest_T : in out Test) renames Test_VMX_CR4_Mask_Requirements;
--  id:2.2/6294be7b9d1ba769/VMX_CR4_Mask_Requirements/1/0/
   procedure Test_VMX_CR4_Mask_Requirements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      VMX_CR4_Mask_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  MCE.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']/vcpu/vmx/"
         & "masks/cr4/MachineCheckEnable",
         Value => "0");

      VMX_CR4_Mask_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VMX CR4 guest/host mask control "
               & "'Machine-Check Enable' of subject 'vt' "
               & "invalid: must be 1"),
               Message   => "Exception mismatch (MCE)");

      --  PAE.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']/vcpu/vmx/"
         & "masks/cr4/PhysicalAddressExtension",
         Value => "0");

      VMX_CR4_Mask_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VMX CR4 guest/host mask control "
               & "'Physical Address Extension' of subject 'vt' "
               & "invalid: must be 1"),
              Message   => "Exception mismatch (PAE)");
--  begin read only
   end Test_VMX_CR4_Mask_Requirements;
--  end read only


--  begin read only
   procedure Test_VMX_Exception_Bitmap_Requirements (Gnattest_T : in out Test);
   procedure Test_VMX_Exception_Bitmap_Requirements_ce7455 (Gnattest_T : in out Test) renames Test_VMX_Exception_Bitmap_Requirements;
--  id:2.2/ce745507a96aaac4/VMX_Exception_Bitmap_Requirements/1/0/
   procedure Test_VMX_Exception_Bitmap_Requirements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      VMX_Exception_Bitmap_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  #MC.

      Muxml.Utils.Set_Element_Value
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']/vcpu/vmx/"
         & "masks/exception/MachineCheck",
         Value => "0");

      VMX_Exception_Bitmap_Requirements (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "VMX Exception bitmap control 'Machine Check' of subject"
               & " 'vt' invalid: must be 1"),
              Message   => "Exception mismatch (#MC)");
--  begin read only
   end Test_VMX_Exception_Bitmap_Requirements;
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
end Mucfgcheck.Subject.Test_Data.Tests;
