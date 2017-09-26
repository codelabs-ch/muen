--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Subject.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Mucfgcheck.Subject.Test_Data.Tests is


--  begin read only
   procedure Test_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Name_Uniqueness_7f1559 (Gnattest_T : in out Test) renames Test_Name_Uniqueness;
--  id:2.2/7f15594730cfb6f0/Name_Uniqueness/1/0/
   procedure Test_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-subject.ads:25:4:Name_Uniqueness
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
   --  mucfgcheck-subject.ads:28:4:CPU_ID
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
   --  mucfgcheck-subject.ads:31:4:Global_ID_Uniqueness
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
   --  mucfgcheck-subject.ads:34:4:Local_ID_Uniqueness
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
   --  mucfgcheck-subject.ads:37:4:Memory_Types
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
   --  mucfgcheck-subject.ads:40:4:No_IOMMU_Device_References
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
   --  mucfgcheck-subject.ads:45:4:Runnability
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
   procedure Test_Logical_IRQ_MSI_Consecutiveness (Gnattest_T : in out Test);
   procedure Test_Logical_IRQ_MSI_Consecutiveness_907fb8 (Gnattest_T : in out Test) renames Test_Logical_IRQ_MSI_Consecutiveness;
--  id:2.2/907fb8e6faa0778a/Logical_IRQ_MSI_Consecutiveness/1/0/
   procedure Test_Logical_IRQ_MSI_Consecutiveness (Gnattest_T : in out Test) is
   --  mucfgcheck-subject.ads:49:4:Logical_IRQ_MSI_Consecutiveness
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
   procedure Test_Virtual_Memory_Overlap (Gnattest_T : in out Test);
   procedure Test_Virtual_Memory_Overlap_7973e4 (Gnattest_T : in out Test) renames Test_Virtual_Memory_Overlap;
--  id:2.2/7973e4663e077f6d/Virtual_Memory_Overlap/1/0/
   procedure Test_Virtual_Memory_Overlap (Gnattest_T : in out Test) is
   --  mucfgcheck-subject.ads:52:4:Virtual_Memory_Overlap
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
   --  mucfgcheck-subject.ads:55:4:Initramfs_Consecutiveness
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
         when E : others =>
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
   --  mucfgcheck-subject.ads:58:4:Crash_Audit_Write_Access
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
         when E : others =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical memory node 'crash_audit' of subject 'tau0' "
                    & "declares illegal write access to crash audit region",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Crash_Audit_Write_Access;
--  end read only

end Mucfgcheck.Subject.Test_Data.Tests;
