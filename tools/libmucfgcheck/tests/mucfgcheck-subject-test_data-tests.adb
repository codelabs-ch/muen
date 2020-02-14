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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']",
         Name  => "name",
         Value => "linux");

      begin
         Name_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subjects with global ID 1 and 4 have identical name "
                    & "'linux'",
                    Message   => "Exception mismatch");
      end;
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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']",
         Name  => "cpu",
         Value => "7");

      begin
         CPU_ID (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'cpu => 7' of 'linux' subject element not in "
                    & "valid range 0 .. 3",
                    Message   => "Exception mismatch");
      end;
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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='sm']",
         Name  => "globalId",
         Value => "0");

      begin
         Global_ID_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subjects 'tau0' and 'sm' have identical global ID 0",
                    Message   => "Exception mismatch");
      end;
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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']",
         Name  => "localId",
         Value => "0");

      begin
         Local_ID_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subjects 'sm' and 'linux' running on CPU 1 have "
                    & "identical local ID 0",
                    Message   => "Exception mismatch");
      end;
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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vt|bin']",
         Name  => "type",
         Value => "system_pt");

      begin
         Memory_Types (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical memory region 'binary' of subject 'vt' mapping "
                    & "physical region 'vt|bin' has invalid type system_pt",
                    Message   => "Exception mismatch");
      end;
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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/device"
         & "[@physical='wireless']",
         Name  => "physical",
         Value => "iommu_1");

      begin
         No_IOMMU_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "IOMMU device referenced by subject 'vt'",
                    Message   => "Exception mismatch");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/devices/device"
         & "[@physical='ethernet']",
         Name  => "physical",
         Value => "iommu_2");

      begin
         No_IOMMU_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "IOMMU device referenced by subjects 'vt', 'linux'",
                    Message   => "Exception mismatch");
      end;
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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='trap_to_sm']",
         Name  => "mode",
         Value => "ipi");

      begin
         Runnability (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'sm' is neither referenced in the scheduling"
                    & " plan nor schedulable via switch events",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Runnability;
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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']/devices/"
         & "device[@logical='vga']",
         Name  => "logical",
         Value => "wireless");

      begin
         Logical_Device_Name_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'vt' has devices with identical logical names"
                    & " 'wireless'",
                    Message   => "Exception mismatch");
      end;
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

      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/devices/device"
            & "[@physical='xhci']/irq",
            Name  => "vector",
            Value => "254");

         Logical_IRQ_MSI_Consecutiveness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "MSI IRQ 'xhci_irq1' of logical device 'xhci' of subject"
                    & " 'linux' not adjacent to other IRQs",
                    Message   => "Exception mismatch");
      end;
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

      begin

         --  Set mistmatching physical IRQ number as suffix.

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/events/source/group/"
            & "event[@logical='unmask_irq_57']/unmask_irq",
            Name  => "number",
            Value => "37");

         Logical_Unmask_Event (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (IRQ mismatch 1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical event 'unmask_irq_57' of subject 'vt' "
                    & "referencing logical IRQ wireless->irq has unmask action"
                    & " number different from physical IRQ wireless->irq: 37, "
                    & "expected 21",
                    Message   => "Exception mismatch (IRQ mismatch 1)");
      end;

      begin

         --  Set mistmatching logical IRQ number as suffix
         --  (unresolvable vector).

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/events/source/group/"
            & "event[@logical='unmask_irq_57']",
            Name  => "logical",
            Value => "unmask_irq_255");

         Logical_Unmask_Event (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (IRQ mismatch 2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical event 'unmask_irq_255' of subject 'vt' "
                    & "references invalid logical IRQ with vector 255 as "
                    & "logical name suffix",
                    Message   => "Exception mismatch (IRQ mismatch 2)");
      end;

      begin

         --  Mistmatching logical IRQ number as suffix (resolvable vector).

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/events/source/group/"
            & "event[@logical='unmask_irq_255']/unmask_irq",
            Name  => "number",
            Value => "21");

         Logical_Unmask_Event (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (IRQ mismatch 3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical event 'unmask_irq_255' of subject 'vt' "
                    & "references invalid logical IRQ with vector 255 as "
                    & "logical name suffix: expected 57",
                    Message   => "Exception mismatch (IRQ mismatch 3)");
      end;

      begin

         --  Additionally, make expected logical IRQ number non-resolvable.

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/devices/device"
            & "/irq[@logical='wlan_irq']",
            Name  => "physical",
            Value => "nonexistent");

         Logical_Unmask_Event (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (IRQ mismatch 4)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical event 'unmask_irq_255' of subject 'vt' "
                    & "references invalid logical IRQ with vector 255 as "
                    & "logical name suffix",
                    Message   => "Exception mismatch (IRQ mismatch 4)");
      end;

      begin

         --  Set non-numeric suffix.

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/events/source/group/"
            & "event[starts-with(@logical,'unmask_irq')]",
            Name  => "logical",
            Value => "unmask_irq_foobar");

         Logical_Unmask_Event (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (Non-numeric suffix)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical event 'unmask_irq_foobar' of subject 'vt' "
                    & "has invalid suffix 'foobar': must match number of "
                    & "corresponding logical IRQ vector",
                    Message   => "Exception mismatch (Non-numeric suffix)");
      end;

      begin

         --  Set non-matching prefix.

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/events/source/group/"
            & "event[starts-with(@logical,'unmask_irq')]",
            Name  => "logical",
            Value => "foobar_irq");

         Logical_Unmask_Event (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (Unexpected prefix)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical event 'foobar_irq' of subject 'vt' has "
                    & "unexpected logical name: must have the form "
                    & "'unmask_irq_$VECTORNR'",
                    Message   => "Exception mismatch (Unexpected prefix)");
      end;
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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/memory/"
         & "memory[@physical='linux|bin']",
         Name  => "virtualAddress",
         Value => "16#0000#");

      begin
         Virtual_Memory_Overlap (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of virtual memory region 'binary' and "
                    & "'zero_page' of subject 'linux'",
                    Message   => "Exception mismatch");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/memory/"
         & "memory[@physical='vt|bin']",
         Name  => "virtualAddress",
         Value => "16#000b_7000#");

      begin
         Virtual_Memory_Overlap (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of virtual memory region 'binary' and "
                    & "'vga->buffer' of subject 'vt'",
                    Message   => "Exception mismatch");
      end;
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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@type='subject_initrd']",
         Name  => "size",
         Value => "16#2000#");

      begin
         Initramfs_Consecutiveness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Initramfs region 'initramfs1' not adjacent to other "
                    & "initramfs regions",
                    Message   => "Exception mismatch");
      end;
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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/memory/memory"
         & "[@logical='crash_audit']",
         Name  => "writable",
         Value => "true");

      begin
         Crash_Audit_Write_Access (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical memory node 'crash_audit' of subject 'tau0' "
                    & "declares illegal write access to crash audit region",
                    Message   => "Exception mismatch");
      end;
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

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='vt']/devices/device/memory"
         & "[@logical='mmconf']",
         Name  => "virtualAddress",
         Value => "16#dead_beef#");

      begin
         Device_Mmconf_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "PCI mmconf region of subject 'vt' logical device "
                    & "'wireless' is 16#dead_beef# but should be "
                    & "16#f80d_0000#",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Device_Mmconf_Mappings;
--  end read only


--  begin read only
   procedure Test_Shared_Device_Same_PCI_Element (Gnattest_T : in out Test);
   procedure Test_Shared_Device_Same_PCI_Element_13370a (Gnattest_T : in out Test) renames Test_Shared_Device_Same_PCI_Element;
--  id:2.2/13370a2d725ab242/Shared_Device_Same_PCI_Element/1/0/
   procedure Test_Shared_Device_Same_PCI_Element (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Shared_Device_Same_PCI_Element (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']/devices/device"
         & "[@physical='xhci']",
         Name  => "physical",
         Value => "wireless");

      begin
         Shared_Device_Same_PCI_Element (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Shared logical devices 'wireless|xhci' specify "
                    & "different PCI elements",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Shared_Device_Same_PCI_Element;
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
         Assert (Condition => False,
                 Message   => "Exception expected (Dual-Monitor)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMX control 'deactivate dual-monitor treatment' of "
                    & "subject 'linux' is 1",
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
         Assert (Condition => False,
                 Message   => "Exception expected (Entry to SMM)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMX control 'entry to SMM' of subject 'linux' is 1",
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
         Assert (Condition => False,
                 Message   => "Exception expected (IOBM)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Address of I/O Bitmap of subject 'linux' invalid: bits "
                    & "11:0 must be zero",
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
         Assert (Condition => False,
                 Message   => "Exception expected (MSRBM)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Address of MSR Bitmap of subject 'linux' invalid: bits "
                    & "11:0 must be zero",
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
         Assert (Condition => False,
                 Message   => "Exception expected (MSR Store)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "MSR Store address of subject 'linux' invalid: bits "
                    & "3:0 must be zero",
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
         Assert (Condition => False,
                 Message   => "Exception expected (NMI Exiting)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMX control 'NMI-Exiting' is 0 for subject "
                    & "'linux' but 'Virtual NMIs' is 1",
                    Message   => "Exception mismatch (NMI Exiting)");
      end NMI_Exiting;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         --  Positive test, must not raise an exception.

         VMX_Controls_Entry_Checks (XML_Data => Data);
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
         begin
            VMX_Controls_Entry_Checks (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (Posted Int 1)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "VMX control 'process posted interrupts' is 1 for "
                       & "subject 'linux' but 'virtual-interrupt delivery' is"
                       & " 0",
                       Message   => "Exception mismatch (Posted Int 1)");
         end;

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
         begin
            VMX_Controls_Entry_Checks (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (Posted Int 2)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "VMX control 'process posted interrupts' is 1 for "
                       & "subject 'linux' but 'acknowledge interrupt on exit' "
                       & "is 0",
                       Message   => "Exception mismatch (Posted Int 2)");
         end;
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
         Assert (Condition => False,
                 Message   => "Exception expected (VMX-preemption timer)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMX control 'activate VMX-preemption timer' is 0 for "
                    & "subject 'linux' but 'save VMX-preemtion timer value' "
                    & "is 1",
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
         begin
            VMX_Controls_Entry_Checks (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (TPR Shadow 1)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "VMX control 'Use TPR Shadow' is 0 for subject "
                       & "'linux' but 'virtual-interrupt delivery' is 1",
                       Message   => "Exception mismatch (TPR Shadow 1)");
         end;

         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc2/APICRegisterVirtualization",
            Value => "1");
         begin
            VMX_Controls_Entry_Checks (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (TPR Shadow 2)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "VMX control 'Use TPR Shadow' is 0 for subject "
                       & "'linux' but 'APIC-register virtualization' is 1",
                       Message   => "Exception mismatch (TPR Shadow 2)");
         end;

         Muxml.Utils.Set_Element_Value
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='linux']/vcpu/vmx/"
            & "controls/proc2/Virtualizex2APICMode",
            Value => "1");
         begin
            VMX_Controls_Entry_Checks (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (TPR Shadow 3)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "VMX control 'Use TPR Shadow' is 0 for subject"
                       & " 'linux' but 'Virtualize x2APIC mode' is 1",
                       Message   => "Exception mismatch (TPR Shadow 3)");
         end;
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
         Assert (Condition => False,
                 Message   => "Exception expected (Unrestricted Guest)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMX control 'unrestricted guest' is 1 for subject "
                    & "'linux' but 'Enable EPT' is 0",
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
         Assert (Condition => False,
                 Message   => "Exception expected (Virtual INT delivery)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMX control 'virtual-interrupt delivery' is 1 for "
                    & "subject 'linux' but 'external-interrupt exiting' is 0",
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
         Assert (Condition => False,
                 Message   => "Exception expected (Virtual NMIs)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMX control 'Virtual NMIs' is 0 for subject 'linux' but"
                    & " 'NMI-window exiting' is 1",
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
         begin
            VMX_Controls_Entry_Checks (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (Virt x2APIC Mode)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "VMX control 'Virtualize x2APIC mode' is 1 for "
                       & "subject 'linux' but 'virtualize APIC accesses' is 1",
                       Message   => "Exception mismatch (Virt x2APIC Mode)");
         end;
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
