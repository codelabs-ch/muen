--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expand.Pre_Checks.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expand.Pre_Checks.Test_Data.Tests is


--  begin read only
   procedure Test_Register_All (Gnattest_T : in out Test);
   procedure Test_Register_All_3f90ea (Gnattest_T : in out Test) renames Test_Register_All;
--  id:2.2/3f90ea30314141bf/Register_All/1/0/
   procedure Test_Register_All (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:26:4:Register_All
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Register_All;
      Assert (Condition => Check_Procs.Get_Count = 23,
              Message   => "Count mismatch:" & Get_Count'Img);
--  begin read only
   end Test_Register_All;
--  end read only


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_9b6b0d (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/9b6b0dee792a1a08/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:29:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Check_Procs.Register (Process => Inc_Counter'Access);
      Run (Data => Policy);
      Assert (Condition => Test_Counter = 1,
              Message   => "Counter not 1:" & Test_Counter'Img);
--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Get_Count (Gnattest_T : in out Test);
   procedure Test_Get_Count_1fbd7c (Gnattest_T : in out Test) renames Test_Get_Count;
--  id:2.2/1fbd7c784b3d55c2/Get_Count/1/0/
   procedure Test_Get_Count (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:32:4:Get_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Get_Count = 0,
              Message   => "Count not zero");

      Check_Procs.Register (Process => Inc_Counter'Access);
      Assert (Condition => Check_Procs.Get_Count = 1,
              Message   => "Count not 1");
--  begin read only
   end Test_Get_Count;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_4b4f85 (Gnattest_T : in out Test) renames Test_Clear;
--  id:2.2/4b4f85da05a9b689/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:35:4:Clear
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Check_Procs.Get_Count = 0,
              Message   => "Procs not empty");

      Check_Procs.Register (Process => Inc_Counter'Access);
      Assert (Condition => Check_Procs.Get_Count /= 0,
              Message   => "Procs count still zero");

      Clear;
      Assert (Condition => Check_Procs.Get_Count = 0,
              Message   => "Procs not cleared");
--  begin read only
   end Test_Clear;
--  end read only


--  begin read only
   procedure Test_Tau0_Presence_In_Scheduling (Gnattest_T : in out Test);
   procedure Test_Tau0_Presence_In_Scheduling_8c7e59 (Gnattest_T : in out Test) renames Test_Tau0_Presence_In_Scheduling;
--  id:2.2/8c7e594b2b87e9fe/Tau0_Presence_In_Scheduling/1/0/
   procedure Test_Tau0_Presence_In_Scheduling (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:40:4:Tau0_Presence_In_Scheduling
--  end read only

      pragma Unreferenced (Gnattest_T);

      Tau0_Node : DOM.Core.Node;
      Policy    : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Tau0_Node := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/scheduling/majorFrame/cpu/"
         & "minorFrame[@subject='tau0']");
      Tau0_Node := DOM.Core.Nodes.Remove_Child
        (N         => DOM.Core.Nodes.Parent_Node (N => Tau0_Node),
         Old_Child => Tau0_Node);
      pragma Unreferenced (Tau0_Node);

      begin
         Tau0_Presence_In_Scheduling (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject tau0 not present in scheduling plan",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Tau0_Presence_In_Scheduling;
--  end read only


--  begin read only
   procedure Test_Subject_Monitor_References (Gnattest_T : in out Test);
   procedure Test_Subject_Monitor_References_3a1ff8 (Gnattest_T : in out Test) renames Test_Subject_Monitor_References;
--  id:2.2/3a1ff8d2d6e965b6/Subject_Monitor_References/1/0/
   procedure Test_Subject_Monitor_References (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:43:4:Subject_Monitor_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/monitor/state[@subject='lnx']",
         Name  => "subject",
         Value => "nonexistent");

      begin
         Subject_Monitor_References (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'nonexistent' referenced by subject monitor "
                    & "'subject1' does not exist",
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
   --  expand-pre_checks.ads:46:4:Subject_Channel_References
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
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Channel 'nonexistent' referenced by subject 'lnx' does"
                    & " not exist",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Channel_References;
--  end read only


--  begin read only
   procedure Test_Subject_Channel_Exports (Gnattest_T : in out Test);
   procedure Test_Subject_Channel_Exports_eaca98 (Gnattest_T : in out Test) renames Test_Subject_Channel_Exports;
--  id:2.2/eaca98df92f7e5c8/Subject_Channel_Exports/1/0/
   procedure Test_Subject_Channel_Exports (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:50:4:Subject_Channel_Exports
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
         Name  => "logical",
         Value => "nonexistent");

      begin
         Subject_Channel_Exports (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'lnx' does not export logical reader channel "
                    & "'primary_data' as requested by referenced component "
                    & "'linux'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Channel_Exports;
--  end read only


--  begin read only
   procedure Test_Channel_Reader_Writer (Gnattest_T : in out Test);
   procedure Test_Channel_Reader_Writer_918b3c (Gnattest_T : in out Test) renames Test_Channel_Reader_Writer;
--  id:2.2/918b3c5761bd21a7/Channel_Reader_Writer/1/0/
   procedure Test_Channel_Reader_Writer (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:53:4:Channel_Reader_Writer
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

         Channel_Reader_Writer (XML_Data => Policy);

         --  Must not raise an exception.

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
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid number of readers for channel 'data_channel':"
                    & " 0",
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
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid number of writers for channel 'data_channel':"
                    & " 0",
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
   --  expand-pre_checks.ads:56:4:Channel_Writer_Has_Event_ID
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/channels/writer"
         & "[@physical='data_channel']",
         Name  => "event",
         Value => "");

      begin
         Channel_Writer_Has_Event_ID (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Missing 'event' attribute for writer of channel "
                    & "'data_channel'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Channel_Writer_Has_Event_ID;
--  end read only


--  begin read only
   procedure Test_Channel_Reader_Has_Event_Vector (Gnattest_T : in out Test);
   procedure Test_Channel_Reader_Has_Event_Vector_dc496b (Gnattest_T : in out Test) renames Test_Channel_Reader_Has_Event_Vector;
--  id:2.2/dc496b276bd400a4/Channel_Reader_Has_Event_Vector/1/0/
   procedure Test_Channel_Reader_Has_Event_Vector (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:60:4:Channel_Reader_Has_Event_Vector
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
         Name  => "vector",
         Value => "");

      begin
         Channel_Reader_Has_Event_Vector
           (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Missing 'vector' attribute for reader of channel "
                    & "'data_channel'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Channel_Reader_Has_Event_Vector;
--  end read only


--  begin read only
   procedure Test_Platform_CPU_Count_Presence (Gnattest_T : in out Test);
   procedure Test_Platform_CPU_Count_Presence_a69356 (Gnattest_T : in out Test) renames Test_Platform_CPU_Count_Presence;
--  id:2.2/a6935685458554d5/Platform_CPU_Count_Presence/1/0/
   procedure Test_Platform_CPU_Count_Presence (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:64:4:Platform_CPU_Count_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Remove_Child
        (Node       => DOM.Core.Documents.Get_Element (Doc => Policy.Doc),
         Child_Name => "platform");

      begin
         Platform_CPU_Count_Presence (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Required '/system/platform/processor/@logicalCpus' "
                    & "attribute not found, add it or use mucfgmerge tool",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Platform_CPU_Count_Presence;
--  end read only


--  begin read only
   procedure Test_Platform_IOAPIC_Presence (Gnattest_T : in out Test);
   procedure Test_Platform_IOAPIC_Presence_c56d2d (Gnattest_T : in out Test) renames Test_Platform_IOAPIC_Presence;
--  id:2.2/c56d2d279580918e/Platform_IOAPIC_Presence/1/0/
   procedure Test_Platform_IOAPIC_Presence (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:67:4:Platform_IOAPIC_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      declare
         Devs : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/platform/devices");
         Node : DOM.Core.Node;
      begin
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "device");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "name",
            Value => "ioapic");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "shared",
            Value => "false");

         Muxml.Utils.Append_Child
           (Node      => Devs,
            New_Child => Node);

         Node := DOM.Core.Nodes.Append_Child
           (N         => Node,
            New_Child => DOM.Core.Documents.Create_Element
              (Doc      => Policy.Doc,
               Tag_Name => "memory"));
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "name",
            Value => "mmio");

         Platform_IOAPIC_Presence (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple I/O APIC devices or I/O APIC device with "
                    & "multiple memory regions present",
                    Message   => "Exception mismatch");
      end;

      Muxml.Utils.Remove_Child
        (Node       => DOM.Core.Documents.Get_Element (Doc => Policy.Doc),
         Child_Name => "platform");

      begin
         Platform_IOAPIC_Presence (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Required I/O APIC device with memory region "
                    & "missing",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Platform_IOAPIC_Presence;
--  end read only


--  begin read only
   procedure Test_Platform_IOMMU_Memory (Gnattest_T : in out Test);
   procedure Test_Platform_IOMMU_Memory_4183d0 (Gnattest_T : in out Test) renames Test_Platform_IOMMU_Memory;
--  id:2.2/4183d0fc1a20cebd/Platform_IOMMU_Memory/1/0/
   procedure Test_Platform_IOMMU_Memory (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:70:4:Platform_IOMMU_Memory
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy  : Muxml.XML_Data_Type;
      IOMMU_1 : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      IOMMU_1 := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/platform/devices/device[@name='iommu_1']");

      declare
         Node : DOM.Core.Node;
      begin
         Node := DOM.Core.Nodes.Append_Child
           (N         => IOMMU_1,
            New_Child => DOM.Core.Documents.Create_Element
              (Doc      => Policy.Doc,
               Tag_Name => "memory"));
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "name",
            Value => "mmio_2");

         Platform_IOMMU_Memory (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "IOMMU device 'iommu_1' has multiple memory regions",
                    Message   => "Exception mismatch");
      end;

      Muxml.Utils.Remove_Child
        (Node       => IOMMU_1,
         Child_Name => "memory");
      Muxml.Utils.Remove_Child
        (Node       => IOMMU_1,
         Child_Name => "memory");

      begin
         Platform_IOMMU_Memory (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "IOMMU device 'iommu_1' has no memory region",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Platform_IOMMU_Memory;
--  end read only


--  begin read only
   procedure Test_Subject_Component_References (Gnattest_T : in out Test);
   procedure Test_Subject_Component_References_0ac6d5 (Gnattest_T : in out Test) renames Test_Subject_Component_References;
--  id:2.2/0ac6d5c2c7416f1f/Subject_Component_References/1/0/
   procedure Test_Subject_Component_References (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:73:4:Subject_Component_References
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
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Component 'nonexistent' referenced by subject 'lnx' "
                    & "does not exist",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Component_References;
--  end read only


--  begin read only
   procedure Test_Component_Channel_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Component_Channel_Name_Uniqueness_00e23b (Gnattest_T : in out Test) renames Test_Component_Channel_Name_Uniqueness;
--  id:2.2/00e23bc975658da7/Component_Channel_Name_Uniqueness/1/0/
   procedure Test_Component_Channel_Name_Uniqueness (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:76:4:Component_Channel_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/components/component[@name='linux']/channels/"
         & "reader[@logical='secondary_data']",
         Name  => "logical",
         Value => "primary_data");

      begin
         Component_Channel_Name_Uniqueness (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple channels with name 'primary_data' in component"
                    & " 'linux'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Component_Channel_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Component_Channel_Size (Gnattest_T : in out Test);
   procedure Test_Component_Channel_Size_0e858d (Gnattest_T : in out Test) renames Test_Component_Channel_Size;
--  id:2.2/0e858d3a74aed20c/Component_Channel_Size/1/0/
   procedure Test_Component_Channel_Size (Gnattest_T : in out Test) is
   --  expand-pre_checks.ads:81:4:Component_Channel_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/channels/channel[@name='data_channel']",
         Name  => "size",
         Value => "16#4000#");

      begin
         Component_Channel_Size (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Component 'linux' referenced by subject 'lnx' requests "
                    & "size 16#1000# for logical channel 'primary_data' but "
                    & "linked physical channel 'data_channel' has size "
                    & "16#4000#",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Component_Channel_Size;
--  end read only

end Expand.Pre_Checks.Test_Data.Tests;
