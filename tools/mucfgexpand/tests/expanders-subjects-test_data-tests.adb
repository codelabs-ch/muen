--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Subjects.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Expanders.Scheduling;
with Mucfgcheck.Validation_Errors;
--  begin read only
--  end read only
package body Expanders.Subjects.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Handle_Profile (Gnattest_T : in out Test);
   procedure Test_Handle_Profile_63fad5 (Gnattest_T : in out Test) renames Test_Handle_Profile;
--  id:2.2/63fad54f185e99dc/Handle_Profile/1/0/
   procedure Test_Handle_Profile (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_profiles.xml",
         Ref_Diff => "data/subjects_profiles.xml.diff",
         Pre      => Prepare_Profile'Access,
         Expander => Handle_Profile'Access);
--  begin read only
   end Test_Handle_Profile;
--  end read only


--  begin read only
   procedure Test_Add_Tau0 (Gnattest_T : in out Test);
   procedure Test_Add_Tau0_485f2f (Gnattest_T : in out Test) renames Test_Add_Tau0;
--  id:2.2/485f2f136ee7d8d2/Add_Tau0/1/0/
   procedure Test_Add_Tau0 (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Without_Tau0
      is
         use type DOM.Core.Node;

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");

         --  Remove tau0 from scheduling plan.

         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/scheduling/partitions/partition/group/"
            & "subject[@name='tau0']");

         Add_Tau0 (Data => Policy);
         Assert (Condition => Muxml.Utils.Get_Element
                 (Doc   => Policy.Doc,
                  XPath => "/system/subjects/subject[@name='tau0']") = null,
                 Message   => "Expanded Tau0 present");
      end Without_Tau0;
   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_tau0.xml",
         Ref_Diff => "data/subjects_tau0.xml.diff",
         Pre      => Scheduling.Add_Partition_CPU_IDs'Access,
         Expander => Add_Tau0'Access);
      Without_Tau0;
--  begin read only
   end Test_Add_Tau0;
--  end read only


--  begin read only
   procedure Test_Handle_Monitors (Gnattest_T : in out Test);
   procedure Test_Handle_Monitors_f27dfd (Gnattest_T : in out Test) renames Test_Handle_Monitors;
--  id:2.2/f27dfdc9e8bf4f96/Handle_Monitors/1/0/
   procedure Test_Handle_Monitors (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_monitors.xml",
         Ref_Diff => "data/subjects_monitors.xml.diff",
         Expander => Handle_Monitors'Access);
--  begin read only
   end Test_Handle_Monitors;
--  end read only


--  begin read only
   procedure Test_Handle_Loaders (Gnattest_T : in out Test);
   procedure Test_Handle_Loaders_a0eaf7 (Gnattest_T : in out Test) renames Test_Handle_Loaders;
--  id:2.2/a0eaf73fb3b2fe22/Handle_Loaders/1/0/
   procedure Test_Handle_Loaders (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_monitor_loaders.xml",
         Ref_Diff => "data/subjects_monitor_loaders.xml.diff",
         Pre      => Prepare_Loader_Expansion'Access,
         Expander => Handle_Loaders'Access);
--  begin read only
   end Test_Handle_Loaders;
--  end read only


--  begin read only
   procedure Test_Add_Global_IDs (Gnattest_T : in out Test);
   procedure Test_Add_Global_IDs_30829a (Gnattest_T : in out Test) renames Test_Add_Global_IDs;
--  id:2.2/30829aa5b1d20017/Add_Global_IDs/1/0/
   procedure Test_Add_Global_IDs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure No_Tau0
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");

         --  Remove Tau0 from scheduling plan.

         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/scheduling/partitions/partition/group/"
            & "subject[@name='tau0']");
         Add_Global_IDs (Data => Policy);
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Policy.Doc,
                  XPath => "/system/subjects/subject[@globalId='0']",
                  Name  => "name") = "lnx",
                 Message   => "Subject with global ID 0 mismatch");
      end No_Tau0;
   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_global_ids.xml",
         Ref_Diff => "data/subjects_global_ids.xml.diff",
         Expander => Add_Global_IDs'Access);
      No_Tau0;
--  begin read only
   end Test_Add_Global_IDs;
--  end read only


--  begin read only
   procedure Test_Add_Local_IDs (Gnattest_T : in out Test);
   procedure Test_Add_Local_IDs_389b2d (Gnattest_T : in out Test) renames Test_Add_Local_IDs;
--  id:2.2/389b2d3bc382c47c/Add_Local_IDs/1/0/
   procedure Test_Add_Local_IDs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_local_ids.xml",
         Ref_Diff => "data/subjects_local_ids.xml.diff",
         Pre      => Prepare_Local_IDs'Access,
         Expander => Add_Local_IDs'Access);
--  begin read only
   end Test_Add_Local_IDs;
--  end read only


--  begin read only
   procedure Test_Add_Missing_Elements (Gnattest_T : in out Test);
   procedure Test_Add_Missing_Elements_154aec (Gnattest_T : in out Test) renames Test_Add_Missing_Elements;
--  id:2.2/154aec16850b8f76/Add_Missing_Elements/1/0/
   procedure Test_Add_Missing_Elements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_missing_elements.xml",
         Ref_Diff => "data/subjects_missing_elements.xml.diff",
         Expander => Add_Missing_Elements'Access);
--  begin read only
   end Test_Add_Missing_Elements;
--  end read only


--  begin read only
   procedure Test_Add_Channel_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Channel_Mappings_4547d6 (Gnattest_T : in out Test) renames Test_Add_Channel_Mappings;
--  id:2.2/4547d6b6e6f42707/Add_Channel_Mappings/1/0/
   procedure Test_Add_Channel_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_channels.xml",
         Ref_Diff => "data/subjects_channels.xml.diff",
         Expander => Add_Channel_Mappings'Access);
--  begin read only
   end Test_Add_Channel_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Channel_Events (Gnattest_T : in out Test);
   procedure Test_Add_Channel_Events_e021f9 (Gnattest_T : in out Test) renames Test_Add_Channel_Events;
--  id:2.2/e021f991d3ef8e8e/Add_Channel_Events/1/0/
   procedure Test_Add_Channel_Events (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Missing_Endpoint
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");

         --  Remove reader of channel with event.

         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject/channels/"
            & "reader[@physical='data_channel']");

         begin
            Add_Channel_Events (Data => Policy);
            Assert (Condition => False,
                    Message   => "Exception expected (1)");

         exception
            when Mucfgcheck.Validation_Errors.Validation_Error =>
               Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                       (Msg =>"No reader for channel 'data_channel'"),
                       Message   => "Exception message mismatch (reader)");
         end;

         --  Remove writer of channel with event.

         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject/channels/"
            & "writer[@physical='data_channel']");

         begin
            Add_Channel_Events (Data => Policy);
            Assert (Condition => False,
                    Message   => "Exception expected (2)");

         exception
            when Mucfgcheck.Validation_Errors.Validation_Error =>
               Assert (Condition => Mucfgcheck.Validation_Errors.Contains
                       (Msg =>"No writer for channel 'data_channel'"),
                       Message   => "Exception message mismatch (writer)");
         end;
      end Missing_Endpoint;
   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_channel_events.xml",
         Ref_Diff => "data/subjects_channel_events.xml.diff",
         Pre      => Prepare_Channel_Events'Access,
         Expander => Add_Channel_Events'Access);
      Missing_Endpoint;
--  begin read only
   end Test_Add_Channel_Events;
--  end read only


--  begin read only
   procedure Test_Remove_Channel_Elements (Gnattest_T : in out Test);
   procedure Test_Remove_Channel_Elements_d2d8a9 (Gnattest_T : in out Test) renames Test_Remove_Channel_Elements;
--  id:2.2/d2d8a9d28497a995/Remove_Channel_Elements/1/0/
   procedure Test_Remove_Channel_Elements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_remove_channels.xml",
         Ref_Diff => "data/subjects_remove_channels.xml.diff",
         Expander => Remove_Channel_Elements'Access);
--  begin read only
   end Test_Remove_Channel_Elements;
--  end read only


--  begin read only
   procedure Test_Add_Default_Events (Gnattest_T : in out Test);
   procedure Test_Add_Default_Events_b015b8 (Gnattest_T : in out Test) renames Test_Add_Default_Events;
--  id:2.2/b015b89079ac2230/Add_Default_Events/1/0/
   procedure Test_Add_Default_Events (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_default_events.xml",
         Ref_Diff => "data/subjects_default_events.xml.diff",
         Expander => Add_Default_Events'Access);
--  begin read only
   end Test_Add_Default_Events;
--  end read only


--  begin read only
   procedure Test_Add_CPU_IDs (Gnattest_T : in out Test);
   procedure Test_Add_CPU_IDs_cf5c89 (Gnattest_T : in out Test) renames Test_Add_CPU_IDs;
--  id:2.2/cf5c8998ce7da859/Add_CPU_IDs/1/0/
   procedure Test_Add_CPU_IDs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_cpu_ids.xml",
         Ref_Diff => "data/subjects_cpu_ids.xml.diff",
         Pre      => Scheduling.Add_Partition_CPU_IDs'Access,
         Expander => Add_CPU_IDs'Access);
--  begin read only
   end Test_Add_CPU_IDs;
--  end read only


--  begin read only
   procedure Test_Add_Device_Memory_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Device_Memory_Mappings_c30caf (Gnattest_T : in out Test) renames Test_Add_Device_Memory_Mappings;
--  id:2.2/c30cafc35150bb96/Add_Device_Memory_Mappings/1/0/
   procedure Test_Add_Device_Memory_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      declare
         Path : constant String := "/system/subjects/subject[@name='lnx']/"
           & "devices/device/memory[@logical='mmio']";
         Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element (Doc   => Policy.Doc,
                                       XPath => Path);
      begin
         DOM.Core.Elements.Remove_Attribute (Elem => Node,
                                             Name => "virtualAddress");

         Add_Device_Memory_Mappings (Data => Policy);

         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Policy.Doc,
                  XPath => Path,
                  Name  => "virtualAddress") = "16#d252_0000#",
                 Message   => "Address mismatch (1)");
      end;

      --  Mmconf regions handling

      declare
         Path : constant String := "/system/subjects/subject"
           & "[@name='subject1']/devices/device/memory[@logical='mmconf']";
         Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element (Doc   => Policy.Doc,
                                       XPath => Path);
      begin
         DOM.Core.Elements.Remove_Attribute (Elem => Node,
                                             Name => "virtualAddress");

         Add_Device_Memory_Mappings (Data => Policy);

         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Policy.Doc,
                  XPath => Path,
                  Name  => "virtualAddress") = "16#f80c_8000#",
                 Message   => "Address mismatch (2)");
      end;
--  begin read only
   end Test_Add_Device_Memory_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Device_BDFs (Gnattest_T : in out Test);
   procedure Test_Add_Device_BDFs_e4e082 (Gnattest_T : in out Test) renames Test_Add_Device_BDFs;
--  id:2.2/e4e082898d30fd9b/Add_Device_BDFs/1/0/
   procedure Test_Add_Device_BDFs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy    : Muxml.XML_Data_Type;
      XHCI_Path : constant String := "/system/subjects/subject/devices/"
        & "device[@physical='xhci']/pci";
      NIC_Path  : constant String := "/system/subjects/subject/devices/"
        & "device[@physical='nic1']/pci";
      Wlan_Path : constant String := "/system/subjects/subject/devices/"
        & "device[@physical='wlan1']/pci";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      --  Resolve 'wireless' device alias so there are multiple subject devices
      --  without PCI BDF assigned.

      Platform.Resolve_Device_Aliases (Data => Policy);

      Add_Device_BDFs (Data => Policy);

      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => XHCI_Path,
               Name  => "bus") = "16#00#",
              Message   => "Bus mismatch (XHCI)");
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => XHCI_Path,
               Name  => "device") = "16#01#",
              Message   => "Device mismatch (XHCI)");
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => XHCI_Path,
               Name  => "function") = "0",
              Message   => "Function mismatch (XHCI)");

      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => NIC_Path,
               Name  => "bus") = "16#00#",
              Message   => "Bus mismatch (NIC)");
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => NIC_Path,
               Name  => "device") = "16#19#",
              Message   => "Device mismatch (NIC)");
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => NIC_Path,
               Name  => "function") = "0",
              Message   => "Function mismatch (NIC)");

      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => Wlan_Path,
               Name  => "bus") = "16#00#",
              Message   => "Bus mismatch (Wlan)");
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => Wlan_Path,
               Name  => "device") = "16#02#",
              Message   => "Device mismatch (Wlan)");
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => Wlan_Path,
               Name  => "function") = "0",
              Message   => "Function mismatch (Wlan)");
--  begin read only
   end Test_Add_Device_BDFs;
--  end read only


--  begin read only
   procedure Test_Add_Device_Resources (Gnattest_T : in out Test);
   procedure Test_Add_Device_Resources_3701c7 (Gnattest_T : in out Test) renames Test_Add_Device_Resources;
--  id:2.2/3701c737ebf21eab/Add_Device_Resources/1/0/
   procedure Test_Add_Device_Resources (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_add_device_resources.xml",
         Ref_Diff => "data/subjects_add_device_resources.xml.diff",
         Pre      => Remove_Subj_Device_Resources'Access,
         Expander => Add_Device_Resources'Access);
--  begin read only
   end Test_Add_Device_Resources;
--  end read only


--  begin read only
   procedure Test_Add_Device_MSIs (Gnattest_T : in out Test);
   procedure Test_Add_Device_MSIs_072d7d (Gnattest_T : in out Test) renames Test_Add_Device_MSIs;
--  id:2.2/072d7d2944bcab27/Add_Device_MSIs/1/0/
   procedure Test_Add_Device_MSIs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_add_device_msis.xml",
         Ref_Diff => "data/subjects_add_device_msis.xml.diff",
         Pre      => Components.Add_Devices'Access,
         Expander => Add_Device_MSIs'Access);
--  begin read only
   end Test_Add_Device_MSIs;
--  end read only


--  begin read only
   procedure Test_Add_Device_Vectors (Gnattest_T : in out Test);
   procedure Test_Add_Device_Vectors_f2568e (Gnattest_T : in out Test) renames Test_Add_Device_Vectors;
--  id:2.2/f2568e5087acb4c9/Add_Device_Vectors/1/0/
   procedure Test_Add_Device_Vectors (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_add_device_vectors.xml",
         Ref_Diff => "data/subjects_add_device_vectors.xml.diff",
         Pre      => Prepare_Profile'Access,
         Expander => Add_Device_Vectors'Access);
--  begin read only
   end Test_Add_Device_Vectors;
--  end read only


--  begin read only
   procedure Test_Add_Unmask_IRQ_Events (Gnattest_T : in out Test);
   procedure Test_Add_Unmask_IRQ_Events_e6628d (Gnattest_T : in out Test) renames Test_Add_Unmask_IRQ_Events;
--  id:2.2/e6628de36901c24b/Add_Unmask_IRQ_Events/1/0/
   procedure Test_Add_Unmask_IRQ_Events (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_add_unmask_irq_events.xml",
         Ref_Diff => "data/subjects_add_unmask_irq_events.xml.diff",
         Pre      => Prepare_Unmask_Events'Access,
         Expander => Add_Unmask_IRQ_Events'Access);
--  begin read only
   end Test_Add_Unmask_IRQ_Events;
--  end read only


--  begin read only
   procedure Test_Add_Sinfo_Regions (Gnattest_T : in out Test);
   procedure Test_Add_Sinfo_Regions_f78150 (Gnattest_T : in out Test) renames Test_Add_Sinfo_Regions;
--  id:2.2/f78150be0443b081/Add_Sinfo_Regions/1/0/
   procedure Test_Add_Sinfo_Regions (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_sinfo_regions.xml",
         Ref_Diff => "data/subjects_sinfo_regions.xml.diff",
         Pre      => Add_Missing_Elements'Access,
         Expander => Add_Sinfo_Regions'Access);
--  begin read only
   end Test_Add_Sinfo_Regions;
--  end read only


--  begin read only
   procedure Test_Add_Scheduling_Info_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Scheduling_Info_Mappings_a32472 (Gnattest_T : in out Test) renames Test_Add_Scheduling_Info_Mappings;
--  id:2.2/a32472f62674d43f/Add_Scheduling_Info_Mappings/1/0/
   procedure Test_Add_Scheduling_Info_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_scheduling_info_mapping.xml",
         Ref_Diff => "data/subjects_scheduling_info_mapping.xml.diff",
         Pre      => Prepare_Sched_Info_Mappings'Access,
         Expander => Add_Scheduling_Info_Mappings'Access);
--  begin read only
   end Test_Add_Scheduling_Info_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Timed_Event_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Timed_Event_Mappings_fece2f (Gnattest_T : in out Test) renames Test_Add_Timed_Event_Mappings;
--  id:2.2/fece2f23e853f3ea/Add_Timed_Event_Mappings/1/0/
   procedure Test_Add_Timed_Event_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
       Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_timed_event.xml",
         Ref_Diff => "data/subjects_timed_event.xml.diff",
         Pre      => Add_Missing_Elements'Access,
         Expander => Add_Timed_Event_Mappings'Access);
--  begin read only
   end Test_Add_Timed_Event_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Target_Event_IDs (Gnattest_T : in out Test);
   procedure Test_Add_Target_Event_IDs_f6cb39 (Gnattest_T : in out Test) renames Test_Add_Target_Event_IDs;
--  id:2.2/f6cb39672ad32558/Add_Target_Event_IDs/1/0/
   procedure Test_Add_Target_Event_IDs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
       Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_target_event_ids.xml",
         Ref_Diff => "data/subjects_target_event_ids.xml.diff",
         Expander => Add_Target_Event_IDs'Access);
--  begin read only
   end Test_Add_Target_Event_IDs;
--  end read only


--  begin read only
   procedure Test_Add_Mugensched_Idle_Subjects (Gnattest_T : in out Test);
   procedure Test_Add_Mugensched_Idle_Subjects_ccd057 (Gnattest_T : in out Test) renames Test_Add_Mugensched_Idle_Subjects;
--  id:2.2/ccd0570517a12c7d/Add_Mugensched_Idle_Subjects/1/0/
   procedure Test_Add_Mugensched_Idle_Subjects (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_mugenschedcfg_idle.xml",
         Ref_Diff => "data/subjects_mugenschedcfg_idle.xml.diff",
         Pre      => Inject_Idle_Subject'Access,
         Expander => Add_Mugensched_Idle_Subjects'Access);
--  begin read only
   end Test_Add_Mugensched_Idle_Subjects;
--  end read only


--  begin read only
   procedure Test_Add_Sibling_Memory (Gnattest_T : in out Test);
   procedure Test_Add_Sibling_Memory_321f1f (Gnattest_T : in out Test) renames Test_Add_Sibling_Memory;
--  id:2.2/321f1f12580bd613/Add_Sibling_Memory/1/0/
   procedure Test_Add_Sibling_Memory (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_sibling_memory.xml",
         Ref_Diff => "data/subjects_sibling_memory.xml.diff",
         Pre      => Prepare_Sibling_Mappings'Access,
         Expander => Add_Sibling_Memory'Access);
--  begin read only
   end Test_Add_Sibling_Memory;
--  end read only


--  begin read only
   procedure Test_Merge_Bootparams (Gnattest_T : in out Test);
   procedure Test_Merge_Bootparams_8099d9 (Gnattest_T : in out Test) renames Test_Merge_Bootparams;
--  id:2.2/8099d96b018a8e2b/Merge_Bootparams/1/0/
   procedure Test_Merge_Bootparams (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_bootparams.xml",
         Ref_Diff => "data/subjects_bootparams.xml.diff",
         Expander => Merge_Bootparams'Access);
--  begin read only
   end Test_Merge_Bootparams;
--  end read only


--  begin read only
   procedure Test_Remove_Monitors (Gnattest_T : in out Test);
   procedure Test_Remove_Monitors_1be168 (Gnattest_T : in out Test) renames Test_Remove_Monitors;
--  id:2.2/1be168f6b3ffa304/Remove_Monitors/1/0/
   procedure Test_Remove_Monitors (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_monitors_remove.xml",
         Ref_Diff => "data/subjects_monitors_remove.xml.diff",
         Expander => Remove_Monitors'Access);
--  begin read only
   end Test_Remove_Monitors;
--  end read only


--  begin read only
   procedure Test_Remove_Device_MSIs (Gnattest_T : in out Test);
   procedure Test_Remove_Device_MSIs_0be2c1 (Gnattest_T : in out Test) renames Test_Remove_Device_MSIs;
--  id:2.2/0be2c11b0685099d/Remove_Device_MSIs/1/0/
   procedure Test_Remove_Device_MSIs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/subjects_device_msi_remove.xml",
         Ref_Diff => "data/subjects_device_msi_remove.xml.diff",
         Pre      => Components.Add_Devices'Access,
         Expander => Remove_Device_MSIs'Access);
--  begin read only
   end Test_Remove_Device_MSIs;
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
end Expanders.Subjects.Test_Data.Tests;
