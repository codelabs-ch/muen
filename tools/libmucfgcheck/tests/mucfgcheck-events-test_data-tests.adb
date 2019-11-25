--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Events.Test_Data.

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
package body Mucfgcheck.Events.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Physical_Event_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Physical_Event_Name_Uniqueness_5e2eca (Gnattest_T : in out Test) renames Test_Physical_Event_Name_Uniqueness;
--  id:2.2/5e2eca7d6fc14927/Physical_Event_Name_Uniqueness/1/0/
   procedure Test_Physical_Event_Name_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Physical_Event_Name_Uniqueness (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='trap_to_sm']",
         Name  => "name",
         Value => "resume_linux");

      begin
         Physical_Event_Name_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple physical events with name 'resume_linux'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Physical_Event_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Source_Targets (Gnattest_T : in out Test);
   procedure Test_Source_Targets_dd485f (Gnattest_T : in out Test) renames Test_Source_Targets;
--  id:2.2/dd485fd3b78efbdb/Source_Targets/1/0/
   procedure Test_Source_Targets (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Source_Targets (XML_Data => Data);

      declare
         Node : DOM.Core.Node
           := DOM.Core.Documents.Create_Element
             (Doc      => Data.Doc,
              Tag_Name => "event");
         Target_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              XPath => "/system/subjects/subject[@name='linux']"
              & "/events/target");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "id",
            Value => "22");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "logical",
            Value => "system_reboot");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "physical",
            Value => "system_reboot");
         Node := DOM.Core.Nodes.Append_Child
           (N         => Target_Node,
            New_Child => Node);

         Source_Targets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid number of targets for kernel-mode event "
                    & "'system_reboot': 1 (no target allowed)",
                    Message   => "Exception mismatch (target)");
            Node := DOM.Core.Nodes.Remove_Child
              (N         => Target_Node,
               Old_Child => Node);
      end;

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/events/target/"
            & "event[@physical='trap_to_sm']/..");
      begin
         Muxml.Utils.Remove_Child
           (Node       => Node,
            Child_Name => "event");

         Source_Targets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid number of targets for event 'trap_to_sm': 0",
                    Message   => "Exception mismatch (target)");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='trap_to_sm']",
         Name  => "name",
         Value => "new_event");

      begin
         Source_Targets (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid number of sources for event 'new_event': 0",
                    Message   => "Exception mismatch (source)");
      end;
--  begin read only
   end Test_Source_Targets;
--  end read only


--  begin read only
   procedure Test_Subject_Event_References (Gnattest_T : in out Test);
   procedure Test_Subject_Event_References_0768ea (Gnattest_T : in out Test) renames Test_Subject_Event_References;
--  id:2.2/0768eab62525b03d/Subject_Event_References/1/0/
   procedure Test_Subject_Event_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_Event_References (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/target/"
         & "event[@physical='trap_to_sm']",
         Name  => "physical",
         Value => "nonexistent_dst");

      begin
         Subject_Event_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Event 'nonexistent_dst' referenced by subject 'sm' does"
                    & " not exist",
                    Message   => "Exception mismatch (target)");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/"
         & "event[@physical='resume_linux']",
         Name  => "physical",
         Value => "nonexistent_src");

      begin
         Subject_Event_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Event 'nonexistent_src' referenced by subject 'sm' does"
                    & " not exist",
                    Message   => "Exception mismatch (source)");
      end;
--  begin read only
   end Test_Subject_Event_References;
--  end read only


--  begin read only
   procedure Test_Self_References (Gnattest_T : in out Test);
   procedure Test_Self_References_af5859 (Gnattest_T : in out Test) renames Test_Self_References;
--  id:2.2/af5859813505ea74/Self_References/1/0/
   procedure Test_Self_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Self_References (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/target/event"
         & "[@physical='linux_console']",
         Name  => "physical",
         Value => "linux_keyboard");

      begin
         Self_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Reference to self in event 'linux_keyboard' of subject "
                    & "'vt'",
                    Message   => "Exception mismatch");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/target/event"
         & "[@physical='linux_keyboard']",
         Name  => "physical",
         Value => "linux_console");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='linux_console']",
         Name  => "mode",
         Value => "self");

      begin
         Self_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Reference to other subject in self-event "
                    & "'linux_console' of subject 'linux'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Self_References;
--  end read only


--  begin read only
   procedure Test_Switch_Same_Core (Gnattest_T : in out Test);
   procedure Test_Switch_Same_Core_9bc636 (Gnattest_T : in out Test) renames Test_Switch_Same_Core;
--  id:2.2/9bc636b0bd4cd54e/Switch_Same_Core/1/0/
   procedure Test_Switch_Same_Core (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/"
         & "event[@physical='linux_keyboard']",
         Name  => "physical",
         Value => "resume_linux");

      begin
         Switch_Same_Core (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Destination subject 'linux' (CPU 1) in subject's 'vt' "
                    & "(CPU 0) switch notification 'resume_linux' invalid - "
                    & "must run on the same CPU",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Switch_Same_Core;
--  end read only


--  begin read only
   procedure Test_IPI_Different_Core (Gnattest_T : in out Test);
   procedure Test_IPI_Different_Core_c8a75b (Gnattest_T : in out Test) renames Test_IPI_Different_Core;
--  id:2.2/c8a75bb306ee763d/IPI_Different_Core/1/0/
   procedure Test_IPI_Different_Core (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      IPI_Different_Core (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='trap_to_sm']",
         Name  => "mode",
         Value => "ipi");

      begin
         IPI_Different_Core (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Destination subject 'sm' (CPU 1) in subject's 'linux' "
                    & "(CPU 1) ipi notification 'trap_to_sm' invalid - must run"
                    & " on different CPU",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_IPI_Different_Core;
--  end read only


--  begin read only
   procedure Test_Source_Group_Event_ID_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Source_Group_Event_ID_Uniqueness_0d6e56 (Gnattest_T : in out Test) renames Test_Source_Group_Event_ID_Uniqueness;
--  id:2.2/0d6e56c19519f6f3/Source_Group_Event_ID_Uniqueness/1/0/
   procedure Test_Source_Group_Event_ID_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/event"
         & "[@logical='resume_linux']",
         Name  => "id",
         Value => "1");

      begin
         Source_Group_Event_ID_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'sm' source events 'resume_linux' and "
                    & "'channel_event_sm_console' share ID 1",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Source_Group_Event_ID_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Source_Group_Event_ID_Validity (Gnattest_T : in out Test);
   procedure Test_Source_Group_Event_ID_Validity_ed9d9b (Gnattest_T : in out Test) renames Test_Source_Group_Event_ID_Validity;
--  id:2.2/ed9d9bbe36269c5a/Source_Group_Event_ID_Validity/1/0/
   procedure Test_Source_Group_Event_ID_Validity (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/event"
         & "[@logical='resume_linux']",
         Name  => "id",
         Value => "256");

      begin
         Source_Group_Event_ID_Validity (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'sm': ID 256 of event 'resume_linux' invalid "
                    & "for group VMCALL",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Source_Group_Event_ID_Validity;
--  end read only


--  begin read only
   procedure Test_Self_Event_Action (Gnattest_T : in out Test);
   procedure Test_Self_Event_Action_e649a6 (Gnattest_T : in out Test) renames Test_Self_Event_Action;
--  id:2.2/e649a6f8cf4efeb5/Self_Event_Action/1/0/
   procedure Test_Self_Event_Action (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Self_Event_Action (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/target/event"
         & "[@physical='linux_console']",
         Name  => "physical",
         Value => "linux_keyboard");
      Muxml.Utils.Remove_Child
        (Node       => Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/events/target/event"
            & "[@physical='linux_keyboard']"),
         Child_Name => "inject_interrupt");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='linux_keyboard']",
         Name  => "mode",
         Value => "self");

      begin
         Self_Event_Action (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Self-event 'channel_event_linux_console' of subject "
                    & "'vt' does not specify an action",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Self_Event_Action;
--  end read only


--  begin read only
   procedure Test_Kernel_Mode_Event_Actions (Gnattest_T : in out Test);
   procedure Test_Kernel_Mode_Event_Actions_f55e89 (Gnattest_T : in out Test) renames Test_Kernel_Mode_Event_Actions;
--  id:2.2/f55e893967a529e0/Kernel_Mode_Event_Actions/1/0/
   procedure Test_Kernel_Mode_Event_Actions (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;

      ----------------------------------------------------------------------

      procedure Missing_System_Poweroff
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Remove_Child
           (Node       => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject/events/source/group/"
               & "event[system_poweroff]"),
            Child_Name => "system_poweroff");

         begin
            Kernel_Mode_Event_Actions (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (Poweroff)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Kernel-mode source event 'system_poweroff' of subject"
                       & " 'vt' does not specify mandatory event action",
                       Message   => "Exception mismatch (Poweroff)");
         end;
      end Missing_System_Poweroff;

      ----------------------------------------------------------------------

      procedure Missing_System_Reboot
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Remove_Child
           (Node       => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject/events/source/group/"
               & "event[system_reboot]"),
            Child_Name => "system_reboot");

         begin
            Kernel_Mode_Event_Actions (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (Reboot)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Kernel-mode source event 'system_reboot' of subject "
                       & "'vt' does not specify mandatory event action",
                       Message   => "Exception mismatch (Reboot)");
         end;
      end Missing_System_Reboot;

      ----------------------------------------------------------------------

      procedure Missing_Unmask_Irq
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Remove_Child
           (Node       => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject/events/source/group/"
               & "event[unmask_irq]"),
            Child_Name => "unmask_irq");

         begin
            Kernel_Mode_Event_Actions (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (Unmask IRQ)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Kernel-mode source event 'unmask_wlan_irq' of subject"
                         & " 'vt' does not specify mandatory event action",
                       Message   => "Exception mismatch (Unmask IRQ)");
         end;
      end Missing_Unmask_Irq;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         --  Positive test, must not raise an exception.

         Kernel_Mode_Event_Actions (XML_Data => Data);
      end Positive_Test;
   begin
      Positive_Test;
      Missing_System_Poweroff;
      Missing_System_Reboot;
      Missing_Unmask_Irq;
--  begin read only
   end Test_Kernel_Mode_Event_Actions;
--  end read only


--  begin read only
   procedure Test_Kernel_Mode_System_Actions (Gnattest_T : in out Test);
   procedure Test_Kernel_Mode_System_Actions_150fed (Gnattest_T : in out Test) renames Test_Kernel_Mode_System_Actions;
--  id:2.2/150fed25899be466/Kernel_Mode_System_Actions/1/0/
   procedure Test_Kernel_Mode_System_Actions (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Kernel_Mode_System_Actions (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='system_reboot']",
         Name  => "mode",
         Value => "ipi");

      begin
         Kernel_Mode_System_Actions (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "System action for event 'system_reboot' of subject 'vt'"
                    & " does not reference physical kernel-mode event "
                    & "'system_reboot'",
                    Message   => "Exception mismatch (1)");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='system_reboot']",
         Name  => "mode",
         Value => "kernel");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='system_poweroff']",
         Name  => "mode",
         Value => "ipi");

      begin
         Kernel_Mode_System_Actions (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "System action for event 'system_poweroff' of subject "
                    & "'vt' does not reference physical kernel-mode event "
                    & "'system_poweroff'",
                    Message   => "Exception mismatch (2)");
      end;
--  begin read only
   end Test_Kernel_Mode_System_Actions;
--  end read only


--  begin read only
   procedure Test_Level_Triggered_Unmask_IRQ_Action (Gnattest_T : in out Test);
   procedure Test_Level_Triggered_Unmask_IRQ_Action_03dce8 (Gnattest_T : in out Test) renames Test_Level_Triggered_Unmask_IRQ_Action;
--  id:2.2/03dce8c1382eca13/Level_Triggered_Unmask_IRQ_Action/1/0/
   procedure Test_Level_Triggered_Unmask_IRQ_Action (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);


      ----------------------------------------------------------------------

      procedure Missing_Event
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Remove_Child
           (Node       => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject/events/source/group/"
               & "event[unmask_irq]"),
            Child_Name => "unmask_irq");

         begin
            Level_Triggered_Unmask_IRQ_Action (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (Missing event)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "No event with unmask_irq action and matching number "
                       & "21 for IRQ 'wireless->irq'",
                       Message   => "Exception mismatch (Missing event)");
         end;
      end Missing_Event;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         --  Positive test, must not raise an exception.

         Level_Triggered_Unmask_IRQ_Action (XML_Data => Data);
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Unassigned_IRQ_Event
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Remove_Elements
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/devices/"
            & "device[@physical='wireless']/irq");

         begin
            Level_Triggered_Unmask_IRQ_Action (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (Unassigned IRQ)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Event unmask_wlan_irq of subject 'vt' has unmask_irq "
                       & "action for unassigned IRQ 'wireless->irq'",
                       Message   => "Exception mismatch (Unassigned IRQ)");
         end;
      end Unassigned_IRQ_Event;

      ----------------------------------------------------------------------

      procedure Unmask_Nr_Mismatch
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/events/source/group/"
            & "event[@logical='unmask_wlan_irq']/unmask_irq",
            Name  => "number",
            Value => "0");

         begin
            Level_Triggered_Unmask_IRQ_Action (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (Number mismatch)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "No event with unmask_irq action and matching number "
                       & "21 for IRQ 'wireless->irq'",
                       Message   => "Exception mismatch (Number mismatch)");
         end;
      end Unmask_Nr_Mismatch;
   begin
      Positive_Test;
      Missing_Event;
      Unmask_Nr_Mismatch;
      Unassigned_IRQ_Event;
--  begin read only
   end Test_Level_Triggered_Unmask_IRQ_Action;
--  end read only


--  begin read only
   procedure Test_Get_Max_ID (Gnattest_T : in out Test);
   procedure Test_Get_Max_ID_a65afa (Gnattest_T : in out Test) renames Test_Get_Max_ID;
--  id:2.2/a65afae2a79d6438/Get_Max_ID/1/0/
   procedure Test_Get_Max_ID (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Get_Max_ID (Group => Mutools.Types.Vmx_Exit) = 59,
              Message   => "Invalid VMX exit max ID");
      Assert (Condition => Get_Max_ID (Group => Mutools.Types.Vmcall) = 63,
              Message   => "Invalid Vmcall max ID");
--  begin read only
   end Test_Get_Max_ID;
--  end read only


--  begin read only
   procedure Test_Is_Valid_Event_ID (Gnattest_T : in out Test);
   procedure Test_Is_Valid_Event_ID_2d339d (Gnattest_T : in out Test) renames Test_Is_Valid_Event_ID;
--  id:2.2/2d339dda9942d861/Is_Valid_Event_ID/1/0/
   procedure Test_Is_Valid_Event_ID (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Mutools.Types;

      type ID_Test_Info is record
         Group : Event_Group_Type;
         ID    : Natural;
         Valid : Boolean;
      end record;

      Test_Data : constant array (Natural range <>) of ID_Test_Info
        := ((Group => Vmx_Exit,
             ID    => 0,
             Valid => True),
            (Group => Vmx_Exit,
             ID    => 34,
             Valid => True),
            (Group => Vmx_Exit,
             ID    => 35,
             Valid => False),
            (Group => Vmx_Exit,
             ID    => 38,
             Valid => False),
            (Group => Vmx_Exit,
             ID    => 42,
             Valid => False),
            (Group => Vmx_Exit,
             ID    => 59,
             Valid => True),
            (Group => Vmx_Exit,
             ID    => 60,
             Valid => False),
            (Group => Vmcall,
             ID    => 0,
             Valid => True),
            (Group => Vmcall,
             ID    => 31,
             Valid => True),
            (Group => Vmcall,
             ID    => 63,
             Valid => True),
            (Group => Vmcall,
             ID    => 64,
             Valid => False));
   begin
      for Data of Test_Data loop
         Assert (Condition => Mucfgcheck.Events.Is_Valid_Event_ID
                 (Group => Data.Group,
                  ID    => Data.ID) = Data.Valid,
                 Message   => "Unexpected result for ID" & Data.ID'Img
                 & " of group " & Data.Group'Img);
      end loop;
--  begin read only
   end Test_Is_Valid_Event_ID;
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
end Mucfgcheck.Events.Test_Data.Tests;
