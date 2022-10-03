--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cfgchecks.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with DOM.Core.Elements;
with Mucfgcheck.Validation_Errors;
with Expanders.Channels;
with Expanders.Subjects.Test_Data;
--  begin read only
--  end read only
package body Cfgchecks.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Tau0_Presence_In_Scheduling (Gnattest_T : in out Test);
   procedure Test_Tau0_Presence_In_Scheduling_8c7e59 (Gnattest_T : in out Test) renames Test_Tau0_Presence_In_Scheduling;
--  id:2.2/8c7e594b2b87e9fe/Tau0_Presence_In_Scheduling/1/0/
   procedure Test_Tau0_Presence_In_Scheduling (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy    : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not generate a validation error.

      Tau0_Presence_In_Scheduling (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error (1)");

      --  Remove tau0 from scheduling plan.

      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/scheduling/partitions/partition/group/"
         & "subject[@name='tau0']");

      Tau0_Presence_In_Scheduling (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg => "Tau0 subject not present but multiple major frames "
               & "specified"),
              Message   => "Exception mismatch");

      --  Remove all but one major frame.

      Mucfgcheck.Validation_Errors.Clear;
      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/scheduling/majorFrame[cpu/@id='1']");
      Tau0_Presence_In_Scheduling (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error (2)");
--  begin read only
   end Test_Tau0_Presence_In_Scheduling;
--  end read only


--  begin read only
   procedure Test_Subject_Monitor_References (Gnattest_T : in out Test);
   procedure Test_Subject_Monitor_References_3a1ff8 (Gnattest_T : in out Test) renames Test_Subject_Monitor_References;
--  id:2.2/3a1ff8d2d6e965b6/Subject_Monitor_References/1/0/
   procedure Test_Subject_Monitor_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_Monitor_References (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/monitor/interrupts"
         & "[@subject='lnx']",
         Name  => "subject",
         Value => "nonexistent");

      begin
         Subject_Monitor_References (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Mucfgcheck.Validation_Errors.Validation_Error =>
            Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                    (Msg =>"Subject 'nonexistent' referenced by subject monitor "
                     & "'subject1' does not exist"),
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Monitor_References;
--  end read only


--  begin read only
   procedure Test_Subject_Channel_References (Gnattest_T : in out Test);
   procedure Test_Subject_Channel_References_b0f446 (Gnattest_T : in out Test) renames Test_Subject_Channel_References;
--  id:2.2/b0f44645f4ddfb91/Subject_Channel_References/1/0/
   procedure Test_Subject_Channel_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/channels/reader"
         & "[@physical='data_channel']",
         Name  => "physical",
         Value => "nonexistent");

      begin
         Subject_Channel_References (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Mucfgcheck.Validation_Errors.Validation_Error =>
            Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                    (Msg =>"Channel 'nonexistent' referenced by subject 'lnx' "
                     & "does not exist"),
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Channel_References;
--  end read only


--  begin read only
   procedure Test_Subject_Component_Resource_Mappings (Gnattest_T : in out Test);
   procedure Test_Subject_Component_Resource_Mappings_8641d7 (Gnattest_T : in out Test) renames Test_Subject_Component_Resource_Mappings;
--  id:2.2/8641d70b50cb39fb/Subject_Component_Resource_Mappings/1/0/
   procedure Test_Subject_Component_Resource_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Expanders.Components.Add_Library_Resources (Data => Policy);
      Expanders.Components.Add_Channel_Arrays (Data => Policy);
      Expanders.Components.Add_Memory_Arrays (Data => Policy);

      --  Positive test, must not raise exception.

      Subject_Component_Resource_Mappings (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Add mapping of non-existent component resource.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@logical='control_data']",
         Name  => "logical",
         Value => "nonexistent");

      Subject_Component_Resource_Mappings (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'subject1' maps logical resource 'nonexistent' "
               & "which is not requested by component 'c1'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Subject_Component_Resource_Mappings;
--  end read only


--  begin read only
   procedure Test_Subject_Channel_Exports (Gnattest_T : in out Test);
   procedure Test_Subject_Channel_Exports_eaca98 (Gnattest_T : in out Test) renames Test_Subject_Channel_Exports;
--  id:2.2/eaca98df92f7e5c8/Subject_Channel_Exports/1/0/
   procedure Test_Subject_Channel_Exports (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise exception.

      Subject_Channel_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Invalid physical reference.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/component"
         & "/map[@logical='primary_data']",
         Name  => "physical",
         Value => "nonexistent");

      Subject_Channel_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Physical channel 'nonexistent' referenced by mapping of"
               & " component logical resource 'primary_data' by subject "
               & "'subject2' does not exist"),
              Message   => "Exception mismatch (1)");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/component"
         & "/map[@logical='primary_data']",
         Name  => "physical",
         Value => "data_channel3");

      --  Missing component channel mapping.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/component"
         & "/map[@logical='primary_data']",
         Name  => "logical",
         Value => "nonexistent");

      Subject_Channel_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'subject2' does not map logical channel "
               & "'primary_data' as requested by referenced component "
               & "'c2'"),
              Message   => "Exception mismatch (2)");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/component"
         & "/map[@logical='nonexistent']",
         Name  => "logical",
         Value => "primary_data");

      --  Missing component channel mapping at array level.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/component"
         & "/map[@logical='output1']",
         Name  => "logical",
         Value => "nonexistent");

      Subject_Channel_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'subject2' does not map logical channel "
               & "'output1' as requested by referenced component 'c2'"),
               Message   => "Exception mismatch (3)");
--  begin read only
   end Test_Subject_Channel_Exports;
--  end read only


--  begin read only
   procedure Test_Subject_Memory_Exports (Gnattest_T : in out Test);
   procedure Test_Subject_Memory_Exports_1a2de9 (Gnattest_T : in out Test) renames Test_Subject_Memory_Exports;
--  id:2.2/1a2de99f4bbf0a58/Subject_Memory_Exports/1/0/
   procedure Test_Subject_Memory_Exports (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise exception.

      Subject_Memory_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Invalid physical reference.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@logical='control_data']",
         Name  => "physical",
         Value => "nonexistent");

      Subject_Memory_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Physical memory region 'nonexistent' referenced by "
               & "mapping of component logical resource 'control_data' by"
               & " subject 'subject1' does not exist"),
              Message   => "Exception mismatch (1)");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@logical='control_data']",
         Name  => "physical",
         Value => "dummy_2");

      --  Missing component memory mapping.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@physical='dummy_2']",
         Name  => "logical",
         Value => "nonexistent");

      Subject_Memory_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'subject1' does not map logical memory region "
               & "'control_data' as requested by referenced component "
               & "'c1'"),
              Message   => "Exception mismatch (2)");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@physical='dummy_2']",
         Name  => "logical",
         Value => "control_data");

      --  Missing component memory mapping at array level.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/component"
         & "/map[@logical='mem1']",
         Name  => "logical",
         Value => "nonexistent");

      Subject_Memory_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'subject2' does not map logical memory region "
               & "'mem1' as requested by referenced component "
               & "'c2'"),
              Message   => "Exception mismatch (3)");
--  begin read only
   end Test_Subject_Memory_Exports;
--  end read only


--  begin read only
   procedure Test_Subject_Device_Exports (Gnattest_T : in out Test);
   procedure Test_Subject_Device_Exports_b92bb7 (Gnattest_T : in out Test) renames Test_Subject_Device_Exports;
--  id:2.2/b92bb7eb13dccc6a/Subject_Device_Exports/1/0/
   procedure Test_Subject_Device_Exports (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Remove device alias references.

      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/components/component[@name='c2']/"
         & "requires/devices");
      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/component"
         & "/map[@logical='wifi']");

      --  Positive test, must not raise exception.

      Subject_Device_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Device resource type mismatch.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@logical='storage_device']/map[@logical='mmio1']",
         Name  => "physical",
         Value => "ioport1");

      Subject_Device_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Physical device resource 'sata_controller->ioport1' and"
               & " component logical resource 'storage_device->mmio1' "
               & "mapped by subject 'subject1' have different type"),
              Message   => "Exception mismatch");

      --  Invalid physical device resource reference.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@logical='storage_device']/map[@logical='mmio1']",
         Name  => "physical",
         Value => "nonexistent");

      Subject_Device_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Physical device resource 'sata_controller->nonexistent'"
               & " referenced by mapping of component logical resource "
               & "'storage_device->mmio1' by subject 'subject1' does not "
               & "exist"),
              Message   => "Exception mismatch");

      --  Missing component device resource mapping.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@logical='storage_device']/map[@logical='mmio1']",
         Name  => "logical",
         Value => "nonexistent");

      Subject_Device_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'subject1' does not map logical device resource"
               & " 'storage_device->mmio1' as requested by referenced "
               & "component 'c1'"),
              Message   => "Exception mismatch");

      --  Invalid physical device reference.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@logical='storage_device']",
         Name  => "physical",
         Value => "nonexistent");

      Subject_Device_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Physical device 'nonexistent' referenced by mapping of"
               & " component logical resource 'storage_device' by subject"
               & " 'subject1' does not exist"),
              Message   => "Exception mismatch");

      --  Missing component device mapping.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@logical='storage_device']",
         Name  => "logical",
         Value => "nonexistent");

      Subject_Device_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'subject1' does not map logical device "
               & "'storage_device' as requested by referenced component "
               & "'c1'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Subject_Device_Exports;
--  end read only


--  begin read only
   procedure Test_Subject_Event_Exports (Gnattest_T : in out Test);
   procedure Test_Subject_Event_Exports_fdf55a (Gnattest_T : in out Test) renames Test_Subject_Event_Exports;
--  id:2.2/fdf55afe35f3d4ab/Subject_Event_Exports/1/0/
   procedure Test_Subject_Event_Exports (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise exception.

      Subject_Event_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Invalid physical reference.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@logical='handover']",
         Name  => "physical",
         Value => "nonexistent");

      Subject_Event_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Physical event 'nonexistent' referenced by "
               & "mapping of component logical resource 'handover' by"
               & " subject 'subject1' does not exist"),
              Message   => "Exception mismatch (1)");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@logical='handover']",
         Name  => "physical",
         Value => "handover_to_lnx");

      --  Missing component source event mapping.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@physical='handover_to_lnx']",
         Name  => "logical",
         Value => "nonexistent");

      Subject_Event_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'subject1' does not map logical event "
               & "'handover' as requested by referenced component 'c1'"),
              Message   => "Exception mismatch (2)");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@physical='handover_to_lnx']",
         Name  => "logical",
         Value => "handover");

      --  Missing component target event mapping.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component"
         & "/map[@physical='timer_subject1']",
         Name  => "logical",
         Value => "nonexistent");

      Subject_Event_Exports (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'subject1' does not map logical event "
               & "'trigger_timer' as requested by referenced component"
               & " 'c1'"),
              Message   => "Exception mismatch (3)");
--  begin read only
   end Test_Subject_Event_Exports;
--  end read only


--  begin read only
   procedure Test_Subject_Resource_Maps_Logical_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Subject_Resource_Maps_Logical_Uniqueness_4e4f1e (Gnattest_T : in out Test) renames Test_Subject_Resource_Maps_Logical_Uniqueness;
--  id:2.2/4e4f1e224492b324/Subject_Resource_Maps_Logical_Uniqueness/1/0/
   procedure Test_Subject_Resource_Maps_Logical_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_Resource_Maps_Logical_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/component/"
         & "map[@logical='secondary_data']",
         Name  => "logical",
         Value => "primary_data");

      Subject_Resource_Maps_Logical_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Multiple logical resource mappings with name "
               & "'primary_data' in subject 'subject2'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Subject_Resource_Maps_Logical_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Subject_IRQ_MSI_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Subject_IRQ_MSI_Name_Uniqueness_1d94bf (Gnattest_T : in out Test) renames Test_Subject_IRQ_MSI_Name_Uniqueness;
--  id:2.2/1d94bfe7db481293/Subject_IRQ_MSI_Name_Uniqueness/1/0/
   procedure Test_Subject_IRQ_MSI_Name_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Subject_IRQ_MSI_Name_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/devices/device[@logical='xhci']"
         & "/irq/msi[@logical='msi2']",
         Name  => "logical",
         Value => "msi1");

      Subject_IRQ_MSI_Name_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'lnx' device 'xhci' MSI IRQ logical 'msi1' is "
               & "not unique"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Subject_IRQ_MSI_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Subject_IRQ_MSI_References (Gnattest_T : in out Test);
   procedure Test_Subject_IRQ_MSI_References_b6f32e (Gnattest_T : in out Test) renames Test_Subject_IRQ_MSI_References;
--  id:2.2/b6f32e0bab2f3f1e/Subject_IRQ_MSI_References/1/0/
   procedure Test_Subject_IRQ_MSI_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Subject_IRQ_MSI_References (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/devices/device[@logical='xhci']"
         & "/irq/msi[@logical='msi1']",
         Name  => "physical",
         Value => "msi_nonexistent");

      Subject_IRQ_MSI_References (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Logical device 'xhci->irq1->msi1' of subject 'lnx' "
               & "references non-existent physical device MSI "
               & "'xhci->irq1->msi_nonexistent'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Subject_IRQ_MSI_References;
--  end read only


--  begin read only
   procedure Test_Subject_Monitor_Loader_Addresses (Gnattest_T : in out Test);
   procedure Test_Subject_Monitor_Loader_Addresses_053d82 (Gnattest_T : in out Test) renames Test_Subject_Monitor_Loader_Addresses;
--  id:2.2/053d82db9e9bdebc/Subject_Monitor_Loader_Addresses/1/0/
   procedure Test_Subject_Monitor_Loader_Addresses (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_Monitor_Loader_Addresses (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/monitor/loader"
         & "[@subject='lnx']",
         Name  => "virtualAddress",
         Value => "16#1000#");

      Subject_Monitor_Loader_Addresses (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Loader mapping 'monitor_loader_lnx' of subject "
               & "'subject1' not in valid range 16#0001_0000_0000# "
               & ".. 16#6fff_ffff_ffff#"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Subject_Monitor_Loader_Addresses;
--  end read only


--  begin read only
   procedure Test_Channel_Reader_Writer (Gnattest_T : in out Test);
   procedure Test_Channel_Reader_Writer_918b3c (Gnattest_T : in out Test) renames Test_Channel_Reader_Writer;
--  id:2.2/918b3c5761bd21a7/Channel_Reader_Writer/1/0/
   procedure Test_Channel_Reader_Writer (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Multiple_Readers
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/channels/channel[@name='data_channel2']",
            Name  => "hasEvent",
            Value => "");
         declare
            Node   : DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='subject1']/channels");
            Reader : DOM.Core.Node := DOM.Core.Documents.Create_Element
              (Doc      => Policy.Doc,
               Tag_Name => "reader");
         begin
            DOM.Core.Elements.Set_Attribute
              (Elem  => Reader,
               Name  => "physical",
               Value => "data_channel2");
            DOM.Core.Elements.Set_Attribute
              (Elem  => Reader,
               Name  => "virtualAddress",
               Value => "16#6000#");
            Muxml.Utils.Append_Child (Node      => Node,
                                      New_Child => Reader);
         end;

         --  Remove 'unexpanded' channels first.

         declare
            use Ada.Strings.Unbounded;

            To_Remove : constant array (1 .. 7) of Unbounded_String
              := (To_Unbounded_String ("data_channel3"),
                  To_Unbounded_String ("data_channel4"),
                  To_Unbounded_String ("data_channel5"),
                  To_Unbounded_String ("chan_array1"),
                  To_Unbounded_String ("chan_array2"),
                  To_Unbounded_String ("chan_array3"),
                  To_Unbounded_String ("chan_array4"));
            Node     : DOM.Core.Node;
            Channels : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Policy.Doc,
                 XPath => "/system/channels");
         begin
            for C of To_Remove loop
               Node := DOM.Core.Nodes.Remove_Child
                 (N         => Channels,
                  Old_Child => Muxml.Utils.Get_Element
                    (Doc   => Policy.Doc,
                     XPath => "/system/channels/channel[@name='"
                     & To_String (C) & "']"));
            end loop;
         end;

         Mucfgcheck.Validation_Errors.Clear;
         Channel_Reader_Writer (XML_Data => Policy);

         --  Must not raise an exception.

         Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
                 Message   => "Unexpected error in positive test");

      end Multiple_Readers;

      ----------------------------------------------------------------------

      procedure No_Reader
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject/channels/reader"
            & "[@physical='data_channel']",
            Name  => "physical",
            Value => "nonexistent");

         Channel_Reader_Writer (XML_Data => Policy);
         Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                 (Msg =>"Invalid number of readers for channel 'data_channel':"
                  & " 0"),
                 Message   => "Exception mismatch (reader)");
      end No_Reader;

      ----------------------------------------------------------------------

      procedure No_Writer
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject/channels/writer"
            & "[@physical='data_channel']",
            Name  => "physical",
            Value => "nonexistent");

         Channel_Reader_Writer (XML_Data => Policy);
         Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                 (Msg =>"Invalid number of writers for channel 'data_channel':"
                  & " 0"),
                 Message   => "Exception mismatch (writer)");
      end No_Writer;

      Policy : Muxml.XML_Data_Type;
   begin
      No_Writer;
      No_Reader;
      Multiple_Readers;
--  begin read only
   end Test_Channel_Reader_Writer;
--  end read only


--  begin read only
   procedure Test_Channel_Writer_Has_Event_ID (Gnattest_T : in out Test);
   procedure Test_Channel_Writer_Has_Event_ID_ae7fe0 (Gnattest_T : in out Test) renames Test_Channel_Writer_Has_Event_ID;
--  id:2.2/ae7fe00845fcf90b/Channel_Writer_Has_Event_ID/1/0/
   procedure Test_Channel_Writer_Has_Event_ID (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Expanders.Subjects.Test_Data.Prepare_Channel_Events (Data => Policy);
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/channels/writer"
         & "[@physical='data_channel']",
         Name  => "event",
         Value => "");

      Channel_Writer_Has_Event_ID (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg => "Missing 'event' attribute for writer of channel "
               & "'data_channel'"),
              Message   => "Exception mismatch");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/channels/writer"
         & "[@physical='data_channel']",
         Name  => "event",
         Value => "1");
      DOM.Core.Elements.Remove_Attribute
        (Elem => Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/channels/channel[@name='data_channel']"),
         Name => "hasEvent");
      Channel_Writer_Has_Event_ID (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg => "Logical channel writer 'primary_data' specifies event "
               & "but referenced channel 'data_channel' is missing hasEvent "
               & "attribute"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Channel_Writer_Has_Event_ID;
--  end read only


--  begin read only
   procedure Test_Channel_Reader_Has_Event_Vector (Gnattest_T : in out Test);
   procedure Test_Channel_Reader_Has_Event_Vector_dc496b (Gnattest_T : in out Test) renames Test_Channel_Reader_Has_Event_Vector;
--  id:2.2/dc496b276bd400a4/Channel_Reader_Has_Event_Vector/1/0/
   procedure Test_Channel_Reader_Has_Event_Vector (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Expanders.Subjects.Test_Data.Prepare_Channel_Events (Data => Policy);
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/channels/reader"
         & "[@physical='data_channel']",
         Name  => "vector",
         Value => "");

      Channel_Reader_Has_Event_Vector
        (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Missing 'vector' attribute for reader of channel "
               & "'data_channel'"),
              Message   => "Exception mismatch");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/channels/reader"
         & "[@physical='data_channel']",
         Name  => "vector",
         Value => "12");

      DOM.Core.Elements.Remove_Attribute
        (Elem => Muxml.Utils.Get_Element 
          (Doc   => Policy.Doc,
           XPath => "/system/channels/channel[@name='data_channel']"),
         Name => "hasEvent");
      Channel_Reader_Has_Event_Vector
        (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg => "Logical channel reader 'channel_1' specifies event but "
               & "referenced channel 'data_channel' is missing hasEvent "
               & "attribute"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Channel_Reader_Has_Event_Vector;
--  end read only


--  begin read only
   procedure Test_Hardware_CPU_Count_Presence (Gnattest_T : in out Test);
   procedure Test_Hardware_CPU_Count_Presence_be1502 (Gnattest_T : in out Test) renames Test_Hardware_CPU_Count_Presence;
--  id:2.2/be15023995701e7a/Hardware_CPU_Count_Presence/1/0/
   procedure Test_Hardware_CPU_Count_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Remove_Child
        (Node       => DOM.Core.Documents.Get_Element (Doc => Policy.Doc),
         Child_Name => "hardware");

      Hardware_CPU_Count_Presence (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Required '/system/hardware/processor/@cpuCores' "
               & "attribute not found, add it or use mucfgmerge tool"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Hardware_CPU_Count_Presence;
--  end read only


--  begin read only
   procedure Test_Hardware_Reserved_Memory_Region_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Hardware_Reserved_Memory_Region_Name_Uniqueness_6768d8 (Gnattest_T : in out Test) renames Test_Hardware_Reserved_Memory_Region_Name_Uniqueness;
--  id:2.2/6768d81ba492d717/Hardware_Reserved_Memory_Region_Name_Uniqueness/1/0/
   procedure Test_Hardware_Reserved_Memory_Region_Name_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Hardware_Reserved_Memory_Region_Name_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/hardware/memory/reservedMemory[@name='rmrr2']",
         Name  => "name",
         Value => "rmrr1");

      Hardware_Reserved_Memory_Region_Name_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Reserved memory region name 'rmrr1' is not unique"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Hardware_Reserved_Memory_Region_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Hardware_Reserved_Memory_Region_References (Gnattest_T : in out Test);
   procedure Test_Hardware_Reserved_Memory_Region_References_87fa67 (Gnattest_T : in out Test) renames Test_Hardware_Reserved_Memory_Region_References;
--  id:2.2/87fa6719d6d5223a/Hardware_Reserved_Memory_Region_References/1/0/
   procedure Test_Hardware_Reserved_Memory_Region_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Hardware_Reserved_Memory_Region_References (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/hardware/devices/device[@name='nic1']/"
         & "reservedMemory[@ref='rmrr1']",
         Name  => "ref",
         Value => "nonexistent");

      begin
         Hardware_Reserved_Memory_Region_References (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
            when Mucfgcheck.Validation_Errors.Validation_Error =>
            Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                    (Msg =>"Reserved region 'nonexistent' referenced by device "
                     & "'nic1' does not exist"),
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Hardware_Reserved_Memory_Region_References;
--  end read only


--  begin read only
   procedure Test_Hardware_IRQ_MSI_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Hardware_IRQ_MSI_Name_Uniqueness_6ac6e0 (Gnattest_T : in out Test) renames Test_Hardware_IRQ_MSI_Name_Uniqueness;
--  id:2.2/6ac6e0afa5a1ba01/Hardware_IRQ_MSI_Name_Uniqueness/1/0/
   procedure Test_Hardware_IRQ_MSI_Name_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Hardware_IRQ_MSI_Name_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/hardware/devices/device[@name='xhci']"
         & "/irq/msi[@name='msi2']",
         Name  => "name",
         Value => "msi1");

      Hardware_IRQ_MSI_Name_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Device 'xhci' MSI IRQ name 'msi1' is not unique"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Hardware_IRQ_MSI_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Hardware_IRQ_Type_Consistency (Gnattest_T : in out Test);
   procedure Test_Hardware_IRQ_Type_Consistency_9cb7c8 (Gnattest_T : in out Test) renames Test_Hardware_IRQ_Type_Consistency;
--  id:2.2/9cb7c8ec1be925e6/Hardware_IRQ_Type_Consistency/1/0/
   procedure Test_Hardware_IRQ_Type_Consistency (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Expanders.Platform.Add_Subject_Device_Resources (Data => Policy);
      Expanders.Platform.Resolve_Device_Aliases (Data => Policy);
      Expanders.Components.Add_Devices (Data => Policy);

      --  Positive tests, must not raise an exception.

      Hardware_IRQ_Type_Consistency (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      declare
         IRQ_Node  : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/subjects/subject[@name='subject2']/devices/"
              & "device[@logical='wifi']/irq");
         MSI_Node  : constant DOM.Core.Node
           := DOM.Core.Documents.Create_Element
             (Doc      => Policy.Doc,
              Tag_Name => "msi");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => MSI_Node,
            Name  => "physical",
            Value => "msi1");
         DOM.Core.Elements.Set_Attribute
           (Elem  => MSI_Node,
            Name  => "logical",
            Value => "msi1");
         Muxml.Utils.Append_Child (Node      => IRQ_Node,
                                   New_Child => MSI_Node);

         Hardware_IRQ_Type_Consistency (XML_Data => Policy);
         Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                 (Msg =>"Physical device 'wlan3' has both legacy and MSI IRQ "
                  & "references"),
                 Message   => "Exception mismatch (1)");
      end;

      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/devices/"
         & "device[@logical='wifi']/irq/msi");

      declare
         IRQ_Node  : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/subjects/subject[@name='lnx']/devices/"
              & "device[@logical='wifi']/irq");
         MSI_Node  : constant DOM.Core.Node
           := DOM.Core.Documents.Create_Element
             (Doc      => Policy.Doc,
              Tag_Name => "msi");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => MSI_Node,
            Name  => "physical",
            Value => "msi1");
         DOM.Core.Elements.Set_Attribute
           (Elem  => MSI_Node,
            Name  => "logical",
            Value => "msi1");
         Muxml.Utils.Append_Child (Node      => IRQ_Node,
                                   New_Child => MSI_Node);

         Hardware_IRQ_Type_Consistency (XML_Data => Policy);
         Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                 (Msg =>"Physical device 'wlan3' has both legacy and MSI IRQ "
                  & "references"),
                 Message   => "Exception mismatch (2)");
      end;

      declare
         Dev_Node  : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/subjects/subject[@name='lnx']/devices/"
              & "device[@logical='wifi']");
         IRQ_Node  : constant DOM.Core.Node
           := DOM.Core.Documents.Create_Element
             (Doc      => Policy.Doc,
              Tag_Name => "irq");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => IRQ_Node,
            Name  => "physical",
            Value => "irq1");
         DOM.Core.Elements.Set_Attribute
           (Elem  => IRQ_Node,
            Name  => "logical",
            Value => "interrupt");
         Muxml.Utils.Append_Child (Node      => Dev_Node,
                                   New_Child => IRQ_Node);

         Hardware_IRQ_Type_Consistency (XML_Data => Policy);
         Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                 (Msg =>"Logical device 'wifi' of subject 'lnx' declares both "
                  & "legacy and MSI IRQ resources"),
                 Message   => "Exception mismatch (3)");
      end;
--  begin read only
   end Test_Hardware_IRQ_Type_Consistency;
--  end read only


--  begin read only
   procedure Test_Device_RMRR_Domain_Assignment (Gnattest_T : in out Test);
   procedure Test_Device_RMRR_Domain_Assignment_fa2422 (Gnattest_T : in out Test) renames Test_Device_RMRR_Domain_Assignment;
--  id:2.2/fa242238d9ae76ba/Device_RMRR_Domain_Assignment/1/0/
   procedure Test_Device_RMRR_Domain_Assignment (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Device_RMRR_Domain_Assignment (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      declare
         Node : DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/devices/device[@name='xhci']");
         RMRR_Ref : DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "reservedMemory");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => RMRR_Ref,
            Name  => "ref",
            Value => "rmrr1");

         Muxml.Utils.Append_Child (Node      => Node,
                                   New_Child => RMRR_Ref);
      end;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/deviceDomains/domain/devices/device"
            & "[@logical='xhci']"),
         Name  => "mapReservedMemory",
         Value => "true");

      Device_RMRR_Domain_Assignment (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Device 'nic1' referencing reserved memory region "
               & "'rmrr1' assigned to different device domain than other "
               & "device(s) referencing the same region: 'nic1_domain' vs"
               & " 'xhci_domain'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Device_RMRR_Domain_Assignment;
--  end read only


--  begin read only
   procedure Test_Subject_Component_References (Gnattest_T : in out Test);
   procedure Test_Subject_Component_References_0ac6d5 (Gnattest_T : in out Test) renames Test_Subject_Component_References;
--  id:2.2/0ac6d5c2c7416f1f/Subject_Component_References/1/0/
   procedure Test_Subject_Component_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='lnx']/component",
         Name  => "ref",
         Value => "nonexistent");

      begin
         Subject_Component_References (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Mucfgcheck.Validation_Errors.Validation_Error =>
            Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                    (Msg =>"Component 'nonexistent' referenced by subject 'lnx' "
                     & "does not exist"),
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Component_References;
--  end read only


--  begin read only
   procedure Test_Subject_Sibling_References (Gnattest_T : in out Test);
   procedure Test_Subject_Sibling_References_aa80a0 (Gnattest_T : in out Test) renames Test_Subject_Sibling_References;
--  id:2.2/aa80a0a33de1c6d6/Subject_Sibling_References/1/0/
   procedure Test_Subject_Sibling_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Subject_Sibling_References (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='lnx_core_1']/sibling",
         Name  => "ref",
         Value => "lxn");

      begin
         Subject_Sibling_References (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Mucfgcheck.Validation_Errors.Validation_Error =>
            Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                    (Msg =>"Sibling 'lxn' referenced by subject 'lnx_core_1' does "
                     & "not exist"),
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Sibling_References;
--  end read only


--  begin read only
   procedure Test_Subject_Sibling_Device_BDFs (Gnattest_T : in out Test);
   procedure Test_Subject_Sibling_Device_BDFs_135d2e (Gnattest_T : in out Test) renames Test_Subject_Sibling_Device_BDFs;
--  id:2.2/135d2e7fb31099e0/Subject_Sibling_Device_BDFs/1/0/
   procedure Test_Subject_Sibling_Device_BDFs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy   : Muxml.XML_Data_Type;
      Dev, PCI : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Subject_Sibling_Device_BDFs (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='lnx']",
         Name  => "profile",
         Value => "linux");

      --  Same physical device, unequal BDFs.

      Dev := Expanders.XML_Utils.Create_Logical_Device_Node
        (Policy        => Policy,
         Logical_Name  => "wlan",
         Physical_Name => "wifi");
      PCI := Mutools.PCI.Create_PCI_Node
        (Policy => Policy,
         Bus    => 4,
         Device => 2,
         Func   => 2);
      Muxml.Utils.Append_Child
        (Node      => Dev,
         New_Child => PCI);

      Expanders.Subjects.Add_Missing_Elements (Data => Policy);
      Muxml.Utils.Append_Child
        (Node      => Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject[@name='lnx_core_1']/devices"),
         New_Child => Dev);

      Subject_Sibling_Device_BDFs (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Linux sibling 'lnx' logical device 'wifi' PCI BDF not "
               & "equal to logical device 'wlan' of sibling 'lnx_core_1' "
               & "referencing same physdev"),
              Message   => "Exception mismatch (1)");

      --  Different physical device, same BDFs.

      DOM.Core.Elements.Set_Attribute
        (Elem  => Dev,
         Name  => "physical",
         Value => "dev1");
      DOM.Core.Elements.Set_Attribute
        (Elem  => PCI,
         Name  => "bus",
         Value => "16#01#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => PCI,
         Name  => "device",
         Value => "16#05#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => PCI,
         Name  => "function",
         Value => "2");

      Subject_Sibling_Device_BDFs (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Logical device 'wlan2' of Linux sibling 'lnx' has equal"
               & " PCI BDF with logical device 'wlan' of sibling "
               & "'lnx_core_1'"),
              Message   => "Exception mismatch (2)");
--  begin read only
   end Test_Subject_Sibling_Device_BDFs;
--  end read only


--  begin read only
   procedure Test_Subject_Sibling_Bootparams (Gnattest_T : in out Test);
   procedure Test_Subject_Sibling_Bootparams_445400 (Gnattest_T : in out Test) renames Test_Subject_Sibling_Bootparams;
--  id:2.2/4454003aa47331e8/Subject_Sibling_Bootparams/1/0/
   procedure Test_Subject_Sibling_Bootparams (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy     : Muxml.XML_Data_Type;
      Bootparams : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Subject_Sibling_Bootparams (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject/sibling[@ref='lnx']/..");
      begin
         Bootparams := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "bootparams");
         Muxml.Utils.Insert_Before (Parent    => Node,
                                    New_Child => Bootparams,
                                    Ref_Child => "memory");

         --  Empty bootparams should not raise an exception.

         Subject_Sibling_Bootparams (XML_Data => Policy);
         Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
                 Message   => "Unexpected error with empty bootparams");
      end;

      Muxml.Utils.Append_Child
        (Node      => Bootparams,
         New_Child => DOM.Core.Documents.Create_Text_Node
           (Doc  => Policy.Doc,
            Data => "console=ttyS3"));

      Subject_Sibling_Bootparams (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'lnx_core_1' which is a sibling of 'lnx' "
               & "specifies boot parameters"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Subject_Sibling_Bootparams;
--  end read only


--  begin read only
   procedure Test_Subject_Sibling_Memory (Gnattest_T : in out Test);
   procedure Test_Subject_Sibling_Memory_e48e0a (Gnattest_T : in out Test) renames Test_Subject_Sibling_Memory;
--  id:2.2/e48e0a90af3ef8b3/Subject_Sibling_Memory/1/0/
   procedure Test_Subject_Sibling_Memory (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Memory : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Subject_Sibling_Memory (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject/sibling[@ref='lnx']/..");
      begin
         Memory := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "memory");
         Muxml.Utils.Insert_Before (Parent    => Node,
                                    New_Child => Memory,
                                    Ref_Child => "events");

         --  Empty memory section should not raise an exception.

         Subject_Sibling_Memory (XML_Data => Policy);
         Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
                 Message   => "Unexpected error with empty memory section");
      end;

      Muxml.Utils.Append_Child
        (Node      => Memory,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "memory"));

      Subject_Sibling_Memory (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Subject 'lnx_core_1' which is a sibling of 'lnx' "
               & "specifies additional memory"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Subject_Sibling_Memory;
--  end read only


--  begin read only
   procedure Test_Library_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Library_Name_Uniqueness_93d539 (Gnattest_T : in out Test) renames Test_Library_Name_Uniqueness;
--  id:2.2/93d539eb567fce5a/Library_Name_Uniqueness/1/0/
   procedure Test_Library_Name_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Library_Name_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/components/library[@name='l2']",
         Name  => "name",
         Value => "l1");

      Library_Name_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Library name 'l1' is not unique"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Library_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Component_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Component_Name_Uniqueness_081515 (Gnattest_T : in out Test) renames Test_Component_Name_Uniqueness;
--  id:2.2/0815153248ced8a1/Component_Name_Uniqueness/1/0/
   procedure Test_Component_Name_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Component_Name_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/components/component[@name='c2']",
         Name  => "name",
         Value => "c1");

      Component_Name_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Component name 'c1' is not unique"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Component_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Component_Channel_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Component_Channel_Name_Uniqueness_00e23b (Gnattest_T : in out Test) renames Test_Component_Channel_Name_Uniqueness;
--  id:2.2/00e23bc975658da7/Component_Channel_Name_Uniqueness/1/0/
   procedure Test_Component_Channel_Name_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/components/component[@name='c2']/requires/channels/"
         & "reader[@logical='secondary_data']",
         Name  => "logical",
         Value => "primary_data");

      Component_Channel_Name_Uniqueness (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Multiple channels with name 'primary_data' in component"
               & " 'c2'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Component_Channel_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Component_Channel_Event (Gnattest_T : in out Test);
   procedure Test_Component_Channel_Event_41519c (Gnattest_T : in out Test) renames Test_Component_Channel_Event;
--  id:2.2/41519c5a874c2bbf/Component_Channel_Event/1/0/
   procedure Test_Component_Channel_Event (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Expanders.Components.Add_Channel_Arrays (Data => Policy);

      --  Positive tests, must not raise an exception.

      Component_Channel_Event (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/channels/channel[@name='chan_array1']",
         Name  => "hasEvent",
         Value => "");

      Component_Channel_Event (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg => "Component 'c2' referenced by subject 'subject2' "
               & "requests logical channel 'output1' with event but "
               & "mapped physical channel 'chan_array1' has no event"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Component_Channel_Event;
--  end read only


--  begin read only
   procedure Test_Component_Channel_Size (Gnattest_T : in out Test);
   procedure Test_Component_Channel_Size_0e858d (Gnattest_T : in out Test) renames Test_Component_Channel_Size;
--  id:2.2/0e858d3a74aed20c/Component_Channel_Size/1/0/
   procedure Test_Component_Channel_Size (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/channels/channel[@name='data_channel3']",
         Name  => "size",
         Value => "16#4000#");

      Component_Channel_Size (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Component 'c2' referenced by subject 'subject2' "
               & "requests size 16#1000# for logical channel "
               & "'primary_data' but linked physical channel "
               & "'data_channel3' has size 16#4000#"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Component_Channel_Size;
--  end read only


--  begin read only
   procedure Test_Component_Memory_Size (Gnattest_T : in out Test);
   procedure Test_Component_Memory_Size_089b62 (Gnattest_T : in out Test) renames Test_Component_Memory_Size;
--  id:2.2/089b62d9130a6f0d/Component_Memory_Size/1/0/
   procedure Test_Component_Memory_Size (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Component_Memory_Size (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Memory size mismatch.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/memory/memory[@name='dummy_2']",
         Name  => "size",
         Value => "16#f000#");

      Component_Memory_Size (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Component 'c1' referenced by subject 'subject1' "
               & "requests size 16#2000# for logical memory "
               & "'control_data' but linked physical memory region "
               & "'dummy_2' has size 16#f000#"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Component_Memory_Size;
--  end read only


--  begin read only
   procedure Test_Component_Device_Memory_Size (Gnattest_T : in out Test);
   procedure Test_Component_Device_Memory_Size_0031d9 (Gnattest_T : in out Test) renames Test_Component_Device_Memory_Size;
--  id:2.2/0031d9ab666c16ac/Component_Device_Memory_Size/1/0/
   procedure Test_Component_Device_Memory_Size (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Remove device alias references.

      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/components/component[@name='c2']/requires/devices");
      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/component"
         & "/map[@logical='wifi']");

      --  Positive test, must not raise an exception.

      Component_Device_Memory_Size (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Device memory size mismatch.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/hardware/devices/device[@name='sata_controller']/"
         & "memory[@name='mem1']",
         Name  => "size",
         Value => "16#beef#");

      Component_Device_Memory_Size (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Component 'c1' referenced by subject 'subject1' "
               & "requests size 16#4000# for logical device memory "
               & "'storage_device->mmio1' but linked physical device "
               & "memory 'sata_controller->mem1' has size 16#beef#"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Component_Device_Memory_Size;
--  end read only


--  begin read only
   procedure Test_Component_Device_IO_Port_Range (Gnattest_T : in out Test);
   procedure Test_Component_Device_IO_Port_Range_866f3a (Gnattest_T : in out Test) renames Test_Component_Device_IO_Port_Range;
--  id:2.2/866f3a92e56cdceb/Component_Device_IO_Port_Range/1/0/
   procedure Test_Component_Device_IO_Port_Range (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Remove device alias references.

      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/components/component[@name='c2']/requires/devices");
      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/component"
         & "/map[@logical='wifi']");

      --  Positive test, must not raise an exception.

      Component_Device_IO_Port_Range (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Device I/O port start mismatch.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/hardware/devices/device[@name='sata_controller']/"
         & "ioPort[@name='ioport3']",
         Name  => "start",
         Value => "16#03f8#");

      Component_Device_IO_Port_Range (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Component 'c1' referenced by subject 'subject1' "
               & "requests I/O range 16#50a0#..16#50a7# for "
               & "'storage_device->port_3' but physical device "
               & "'sata_controller->ioport3' has 16#03f8#..16#50a7#"),
              Message   => "Exception mismatch (1)");

      --  Device I/O port end mismatch.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/hardware/devices/device[@name='sata_controller']/"
         & "ioPort[@name='ioport1']",
         Name  => "end",
         Value => "16#beef#");

      Component_Device_IO_Port_Range (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Contains
              (Msg =>"Component 'c1' referenced by subject 'subject1' "
               & "requests I/O range 16#50a8#..16#50af# for "
               & "'storage_device->port_1' but physical device " &
                 "'sata_controller->ioport1' has 16#50a8#..16#beef#"),
              Message   => "Exception mismatch (2)");
--  begin read only
   end Test_Component_Device_IO_Port_Range;
--  end read only


--  begin read only
   procedure Test_Component_Library_References (Gnattest_T : in out Test);
   procedure Test_Component_Library_References_d2285b (Gnattest_T : in out Test) renames Test_Component_Library_References;
--  id:2.2/d2285b248b088593/Component_Library_References/1/0/
   procedure Test_Component_Library_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Component_Library_References (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/components/component[@name='c1']/depends/library",
         Name  => "ref",
         Value => "nonexistent");

      begin
         Component_Library_References (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Mucfgcheck.Validation_Errors.Validation_Error =>
            Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                    (Msg =>"Library 'nonexistent' referenced by component 'c1' "
                     & "does not exist"),
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Component_Library_References;
--  end read only


--  begin read only
   procedure Test_Component_Library_Cyclic_References (Gnattest_T : in out Test);
   procedure Test_Component_Library_Cyclic_References_5c0f40 (Gnattest_T : in out Test) renames Test_Component_Library_Cyclic_References;
--  id:2.2/5c0f40b345ab7567/Component_Library_Cyclic_References/1/0/
   procedure Test_Component_Library_Cyclic_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Component_Library_Cyclic_References (XML_Data => Policy);
      Assert (Condition => Mucfgcheck.Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      declare
         Lib_Node  : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/components/library[@name='l0']");
         Deps_Node : constant DOM.Core.Node
           := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "depends");
         Dep_Node  : constant DOM.Core.Node
           := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "library");
      begin
         DOM.Core.Elements.Set_Attribute (Elem  => Dep_Node,
                                          Name  => "ref",
                                          Value => "l1");
         Muxml.Utils.Append_Child (Node      => Deps_Node,
                                   New_Child => Dep_Node);
         Muxml.Utils.Insert_Before (Parent    => Lib_Node,
                                    New_Child => Deps_Node,
                                    Ref_Child => "devices");

         Component_Library_Cyclic_References (XML_Data => Policy);
         Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                 (Msg =>"Cyclic component dependency detected: l0->l1->l0"),
                 Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Component_Library_Cyclic_References;
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
end Cfgchecks.Test_Data.Tests;
