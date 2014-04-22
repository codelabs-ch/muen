--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Exceptions;

with DOM.Core.Elements;

with Muxml.Utils;
with Mutools.Types;
with Mucfgcheck.Events;

package body Event_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Get_Max_ID
   is
   begin
      Assert (Condition => Mucfgcheck.Events.Get_Max_ID
              (Group => Mutools.Types.Vmx_Exit) = 59,
              Message   => "Invalid VMX exit max ID");
      Assert (Condition => Mucfgcheck.Events.Get_Max_ID
              (Group => Mutools.Types.Vmcall) = 31,
              Message   => "Invalid Vmcall max ID");
   end Get_Max_ID;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Event validator tests");
      T.Add_Test_Routine
        (Routine => Validate_Source_Target'Access,
         Name    => "Validate event source/target connections");
      T.Add_Test_Routine
        (Routine => Validate_Subject_Event_References'Access,
         Name    => "Validate subject event references");
      T.Add_Test_Routine
        (Routine => Validate_Self_References'Access,
         Name    => "Validate event table self-references");
      T.Add_Test_Routine
        (Routine => Validate_Switch_Destination'Access,
         Name    => "Validate event switch destination");
      T.Add_Test_Routine
        (Routine => Validate_IPI_Destination'Access,
         Name    => "Validate event IPI destination");
      T.Add_Test_Routine
        (Routine => Validate_Source_Group_IDs_Uniqueness'Access,
         Name    => "Validate uniqueness of source event IDs");
      T.Add_Test_Routine
        (Routine => Validate_Source_Group_IDs'Access,
         Name    => "Validate source event IDs");
      T.Add_Test_Routine
        (Routine => Get_Max_ID'Access,
         Name    => "Check per-group max event ID");
      T.Add_Test_Routine
        (Routine => Is_Valid_Event_ID'Access,
         Name    => "Check per-group event ID validity");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Is_Valid_Event_ID
   is
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
             ID    => 32,
             Valid => False));
   begin
      for Data of Test_Data loop
         Assert (Condition => Mucfgcheck.Events.Is_Valid_Event_ID
                 (Group => Data.Group,
                  ID    => Data.ID) = Data.Valid,
                 Message   => "Unexpected result for ID" & Data.ID'Img
                 & " of group " & Data.Group'Img);
      end loop;
   end Is_Valid_Event_ID;

   -------------------------------------------------------------------------

   procedure Validate_IPI_Destination
   is
      Data       : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/events/event[@name='trap_to_sm']");
      begin
         DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                          Name  => "mode",
                                          Value => "ipi");

         Mucfgcheck.Events.IPI_Different_Core (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Destination subject 'sm' (CPU 1) in subject's 'linux' "
                    & "(CPU 1) ipi notification 'trap_to_sm' invalid - no IPI"
                    & " allowed",
                    Message   => "Exception mismatch");
      end;
   end Validate_IPI_Destination;

   -------------------------------------------------------------------------

   procedure Validate_Self_References
   is
      Data       : Muxml.XML_Data_Type;
      Event_Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      Event_Node := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/target/event"
         & "[@physical='nonexistent']");
      DOM.Core.Elements.Set_Attribute (Elem  => Event_Node,
                                       Name  => "physical",
                                       Value => "linux_kbd");

      begin
         Mucfgcheck.Events.Self_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Reference to self in event 'linux_kbd' of subject "
                    & "'linux'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Self_References;

   -------------------------------------------------------------------------

   procedure Validate_Source_Group_IDs
   is
      Data       : Muxml.XML_Data_Type;
      Event_Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");
      Event_Node := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/event"
         & "[@logical='invalid_subject']");
      DOM.Core.Elements.Set_Attribute (Elem  => Event_Node,
                                       Name  => "id",
                                       Value => "256");

      begin
         Mucfgcheck.Events.Source_Group_Event_ID_Validity (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'linux': ID 256 of event 'invalid_subject' "
                    & "invalid for group VMCALL",
                    Message   => "Exception mismatch");
      end;
   end Validate_Source_Group_IDs;

   -------------------------------------------------------------------------

   procedure Validate_Source_Group_IDs_Uniqueness
   is
      Data       : Muxml.XML_Data_Type;
      Event_Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");
      Event_Node := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/source/group/event"
         & "[@logical='invalid_subject']");
      DOM.Core.Elements.Set_Attribute (Elem  => Event_Node,
                                       Name  => "id",
                                       Value => "1");

      begin
         Mucfgcheck.Events.Source_Group_Event_ID_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'linux' source events 'forward_keyboard' and "
                    & "'invalid_subject' share ID 1",
                    Message   => "Exception mismatch");
      end;
   end Validate_Source_Group_IDs_Uniqueness;

   -------------------------------------------------------------------------

   procedure Validate_Source_Target
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Events.Source_Targets (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid number of targets for event 'linux_kbd': 0",
                    Message   => "Exception mismatch (target)");
      end;

      declare
         Event_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/events/event[@name='linux_kbd']");
      begin
         DOM.Core.Elements.Set_Attribute (Elem  => Event_Node,
                                          Name  => "name",
                                          Value => "new_event");

         Mucfgcheck.Events.Source_Targets (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid number of sources for event 'new_event': 0",
                    Message   => "Exception mismatch (source)");
      end;
   end Validate_Source_Target;

   -------------------------------------------------------------------------

   procedure Validate_Subject_Event_References
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Events.Subject_Event_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Event 'test' referenced by subject 'linux' does not "
                    & "exist",
                    Message   => "Exception mismatch (source)");
      end;

      declare
         Event_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/events/source/group/"
            & "event/notify[@physical='test']/..");
      begin
         Muxml.Utils.Remove_Child (Node       => Event_Node,
                                   Child_Name => "notify");
         Mucfgcheck.Events.Subject_Event_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Event 'nonexistent' referenced by subject 'linux' does "
                    & "not exist",
                    Message   => "Exception mismatch (target)");
      end;
   end Validate_Subject_Event_References;

   -------------------------------------------------------------------------

   procedure Validate_Switch_Destination
   is
      Data       : Muxml.XML_Data_Type;
      Event_Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");
      Event_Node := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/events/target/event"
         & "[@physical='nonexistent']");
      DOM.Core.Elements.Set_Attribute (Elem  => Event_Node,
                                       Name  => "physical",
                                       Value => "switch_to_linux");

      begin
         Mucfgcheck.Events.Switch_Same_Core (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Destination subject 'linux' (CPU 0) in subject's "
                    & "'subject1' (CPU 1) switch notification 'switch_to_"
                    & "linux' invalid - must run on the same CPU",
                    Message   => "Exception mismatch");
      end;
   end Validate_Switch_Destination;

end Event_Tests;
