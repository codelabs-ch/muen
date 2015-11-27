--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cfgchecks.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Cfgchecks.Test_Data.Tests is


--  begin read only
   procedure Test_Tau0_Presence_In_Scheduling (Gnattest_T : in out Test);
   procedure Test_Tau0_Presence_In_Scheduling_8c7e59 (Gnattest_T : in out Test) renames Test_Tau0_Presence_In_Scheduling;
--  id:2.2/8c7e594b2b87e9fe/Tau0_Presence_In_Scheduling/1/0/
   procedure Test_Tau0_Presence_In_Scheduling (Gnattest_T : in out Test) is
   --  cfgchecks.ads:25:4:Tau0_Presence_In_Scheduling
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy    : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Remove all tau0 minor frames from scheduling plan.

      declare
         Tau0_Nodes : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "/system/scheduling/majorFrame/cpu/"
              & "minorFrame[@subject='tau0']");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Tau0_Nodes) - 1 loop
            declare
               Node : DOM.Core.Node := DOM.Core.Nodes.Item
                 (List  => Tau0_Nodes,
                  Index => I);
            begin
               Node := DOM.Core.Nodes.Remove_Child
                 (N         => DOM.Core.Nodes.Parent_Node (N => Node),
                  Old_Child => Node);
            end;
         end loop;
      end;

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
   --  cfgchecks.ads:28:4:Subject_Monitor_References
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
   --  cfgchecks.ads:31:4:Subject_Channel_References
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
   --  cfgchecks.ads:35:4:Subject_Channel_Exports
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject2']/component"
         & "/map[@logical='primary_data']",
         Name  => "logical",
         Value => "nonexistent");

      begin
         Subject_Channel_Exports (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'subject2' does not export logical channel "
                    & "'primary_data' as requested by referenced component "
                    & "'c2'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Channel_Exports;
--  end read only


--  begin read only
   procedure Test_Subject_Resource_Maps_Logical_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Subject_Resource_Maps_Logical_Uniqueness_4e4f1e (Gnattest_T : in out Test) renames Test_Subject_Resource_Maps_Logical_Uniqueness;
--  id:2.2/4e4f1e224492b324/Subject_Resource_Maps_Logical_Uniqueness/1/0/
   procedure Test_Subject_Resource_Maps_Logical_Uniqueness (Gnattest_T : in out Test) is
   --  cfgchecks.ads:38:4:Subject_Resource_Maps_Logical_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component/"
         & "map[@logical='secondary_data']",
         Name  => "logical",
         Value => "primary_data");

      begin
         Subject_Resource_Maps_Logical_Uniqueness (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple logical resource mappings with name "
                    & "'primary_data' in subject 'subject1'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Resource_Maps_Logical_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Subject_Resource_Maps_Physical_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Subject_Resource_Maps_Physical_Uniqueness_205f09 (Gnattest_T : in out Test) renames Test_Subject_Resource_Maps_Physical_Uniqueness;
--  id:2.2/205f09d9298f08c0/Subject_Resource_Maps_Physical_Uniqueness/1/0/
   procedure Test_Subject_Resource_Maps_Physical_Uniqueness (Gnattest_T : in out Test) is
   --  cfgchecks.ads:42:4:Subject_Resource_Maps_Physical_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component/"
         & "map[@physical='data_channel2']",
         Name  => "physical",
         Value => "data_channel");

      begin
         Subject_Resource_Maps_Physical_Uniqueness (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple physical resource mappings with name "
                    & "'data_channel' in subject 'subject1'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Resource_Maps_Physical_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Subject_Resource_Maps_Physical_References (Gnattest_T : in out Test);
   procedure Test_Subject_Resource_Maps_Physical_References_9ccc85 (Gnattest_T : in out Test) renames Test_Subject_Resource_Maps_Physical_References;
--  id:2.2/9ccc851dbe0378ad/Subject_Resource_Maps_Physical_References/1/0/
   procedure Test_Subject_Resource_Maps_Physical_References (Gnattest_T : in out Test) is
   --  cfgchecks.ads:46:4:Subject_Resource_Maps_Physical_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/component/"
         & "map[@physical='data_channel2']",
         Name  => "physical",
         Value => "nonexistent");

      begin
         Subject_Resource_Maps_Physical_References (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical channel 'nonexistent' referenced by subject "
                    & "'subject1' component resource mapping does not exist",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Resource_Maps_Physical_References;
--  end read only


--  begin read only
   procedure Test_Channel_Reader_Writer (Gnattest_T : in out Test);
   procedure Test_Channel_Reader_Writer_918b3c (Gnattest_T : in out Test) renames Test_Channel_Reader_Writer;
--  id:2.2/918b3c5761bd21a7/Channel_Reader_Writer/1/0/
   procedure Test_Channel_Reader_Writer (Gnattest_T : in out Test) is
   --  cfgchecks.ads:50:4:Channel_Reader_Writer
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

         --  Remove 'unexpanded' channels of subject2 first.

         declare
            Node : DOM.Core.Node;
         begin
            Node := DOM.Core.Nodes.Remove_Child
              (N         => Muxml.Utils.Get_Element
                 (Doc   => Policy.Doc,
                  XPath => "/system/channels"),
               Old_Child => Muxml.Utils.Get_Element
                 (Doc   => Policy.Doc,
                  XPath => "/system/channels/channel[@name='data_channel3']"));
            Node := DOM.Core.Nodes.Remove_Child
              (N         => Muxml.Utils.Get_Element
                 (Doc   => Policy.Doc,
                  XPath => "/system/channels"),
               Old_Child => Muxml.Utils.Get_Element
                 (Doc   => Policy.Doc,
                  XPath => "/system/channels/channel[@name='data_channel4']"));
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
   --  cfgchecks.ads:53:4:Channel_Writer_Has_Event_ID
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
   --  cfgchecks.ads:57:4:Channel_Reader_Has_Event_Vector
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
   --  cfgchecks.ads:61:4:Platform_CPU_Count_Presence
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
   --  cfgchecks.ads:64:4:Platform_IOAPIC_Presence
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
   --  cfgchecks.ads:67:4:Platform_IOMMU_Memory
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
   procedure Test_Platform_Reserved_Memory_Region_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Platform_Reserved_Memory_Region_Name_Uniqueness_07e05b (Gnattest_T : in out Test) renames Test_Platform_Reserved_Memory_Region_Name_Uniqueness;
--  id:2.2/07e05b6e486b70f7/Platform_Reserved_Memory_Region_Name_Uniqueness/1/0/
   procedure Test_Platform_Reserved_Memory_Region_Name_Uniqueness (Gnattest_T : in out Test) is
   --  cfgchecks.ads:70:4:Platform_Reserved_Memory_Region_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Platform_Reserved_Memory_Region_Name_Uniqueness (XML_Data => Policy);

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/platform/memory/reservedMemory[@name='rmrr2']",
         Name  => "name",
         Value => "rmrr1");

      begin
         Platform_Reserved_Memory_Region_Name_Uniqueness (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple reserved memory regions with name 'rmrr1'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Platform_Reserved_Memory_Region_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Platform_Reserved_Memory_Region_References (Gnattest_T : in out Test);
   procedure Test_Platform_Reserved_Memory_Region_References_84b88a (Gnattest_T : in out Test) renames Test_Platform_Reserved_Memory_Region_References;
--  id:2.2/84b88aa03fd757ab/Platform_Reserved_Memory_Region_References/1/0/
   procedure Test_Platform_Reserved_Memory_Region_References (Gnattest_T : in out Test) is
   --  cfgchecks.ads:74:4:Platform_Reserved_Memory_Region_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must no raise an exception.

      Platform_Reserved_Memory_Region_References (XML_Data => Policy);

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/platform/devices/device[@name='nic1']/"
         & "reservedMemory[@ref='rmrr1']",
         Name  => "ref",
         Value => "nonexistent");

      begin
         Platform_Reserved_Memory_Region_References (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Reserved region 'nonexistent' referenced by device "
                    & "'nic1' does not exist",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Platform_Reserved_Memory_Region_References;
--  end read only


--  begin read only
   procedure Test_Device_RMRR_Domain_Assignment (Gnattest_T : in out Test);
   procedure Test_Device_RMRR_Domain_Assignment_fa2422 (Gnattest_T : in out Test) renames Test_Device_RMRR_Domain_Assignment;
--  id:2.2/fa242238d9ae76ba/Device_RMRR_Domain_Assignment/1/0/
   procedure Test_Device_RMRR_Domain_Assignment (Gnattest_T : in out Test) is
   --  cfgchecks.ads:79:4:Device_RMRR_Domain_Assignment
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive tests, must not raise an exception.

      Device_RMRR_Domain_Assignment (XML_Data => Policy);

      declare
         Node   : DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/platform/devices/device[@name='xhci']");
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

      begin
         Device_RMRR_Domain_Assignment (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device 'nic1' referencing reserved memory region "
                    & "'rmrr1' assigned to different device domain than other "
                    & "device(s) referencing the same region: 'nic1_domain' vs"
                    & " 'xhci_domain'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Device_RMRR_Domain_Assignment;
--  end read only


--  begin read only
   procedure Test_Subject_Component_References (Gnattest_T : in out Test);
   procedure Test_Subject_Component_References_0ac6d5 (Gnattest_T : in out Test) renames Test_Subject_Component_References;
--  id:2.2/0ac6d5c2c7416f1f/Subject_Component_References/1/0/
   procedure Test_Subject_Component_References (Gnattest_T : in out Test) is
   --  cfgchecks.ads:82:4:Subject_Component_References
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
   --  cfgchecks.ads:85:4:Component_Channel_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/components/component[@name='c2']/channels/"
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
                    & " 'c2'",
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
   --  cfgchecks.ads:90:4:Component_Channel_Size
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

      begin
         Component_Channel_Size (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Component 'c2' referenced by subject 'subject2' "
                    & "requests size 16#1000# for logical channel "
                    & "'primary_data' but linked physical channel "
                    & "'data_channel3' has size 16#4000#",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Component_Channel_Size;
--  end read only


--  begin read only
   procedure Test_Kernel_Diagnostics_Dev_Reference (Gnattest_T : in out Test);
   procedure Test_Kernel_Diagnostics_Dev_Reference_a807d7 (Gnattest_T : in out Test) renames Test_Kernel_Diagnostics_Dev_Reference;
--  id:2.2/a807d763b4f8343b/Kernel_Diagnostics_Dev_Reference/1/0/
   procedure Test_Kernel_Diagnostics_Dev_Reference (Gnattest_T : in out Test) is
   --  cfgchecks.ads:93:4:Kernel_Diagnostics_Dev_Reference
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Kernel_Diagnostics_Dev_Reference (XML_Data => Policy);

      --  Set invalid kernel diagnostics device reference.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/kernelDiagnosticsDevice",
         Name  => "physical",
         Value => "nonexistent");

      begin
         Kernel_Diagnostics_Dev_Reference (XML_Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel diagnostics device 'nonexistent' with I/O port "
                    & "resource 'serial' does not reference a physical I/O "
                    & "device",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Kernel_Diagnostics_Dev_Reference;
--  end read only

end Cfgchecks.Test_Data.Tests;
