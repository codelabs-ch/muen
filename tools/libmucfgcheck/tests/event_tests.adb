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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Muxml;
with Mucfgcheck.Events;

package body Event_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Event validator tests");
      T.Add_Test_Routine
        (Routine => Validate_Source_Target'Access,
         Name    => "Validate event source/target connections");
      T.Add_Test_Routine
        (Routine => Validate_Subject_References'Access,
         Name    => "Validate event table subject references");
      T.Add_Test_Routine
        (Routine => Validate_Self_References'Access,
         Name    => "Validate event table self-references");
      T.Add_Test_Routine
        (Routine => Validate_Switch_Destination'Access,
         Name    => "Validate event switch destination");
      T.Add_Test_Routine
        (Routine => Validate_IPI_Destination'Access,
         Name    => "Validate event IPI destination");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Validate_IPI_Destination
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Events.IPI_Different_Core (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Destination subject 'linux' (CPU 0) in subject's "
                    & "'linux' (CPU 0) ipi notification 'forward_keyboard' "
                    & "invalid - no IPI allowed",
                    Message   => "Exception mismatch");
      end;
   end Validate_IPI_Destination;

   -------------------------------------------------------------------------

   procedure Validate_Self_References
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Events.Self_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Reference to self in event table entry "
                    & "'forward_keyboard' of subject 'linux'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Self_References;

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
         Event_Node : constant DOM.Core.Node
           := DOM.Core.Nodes.Item
             (List  => McKae.XML.XPath.XIA.XPath_Query
                (N     => Data.Doc,
                 XPath => "/system/events/event[@name='linux_kbd']"),
              Index => 0);
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

   procedure Validate_Subject_References
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Events.Subject_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Reference to unknown subject 'nonexistent' in event "
                    & "'invalid_subject'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Subject_References;

   -------------------------------------------------------------------------

   procedure Validate_Switch_Destination
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Events.Switch_Same_Core (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Destination subject 'linux' (CPU 0) in subject's "
                    & "'subject1' (CPU 1) switch notification 'linux_switch' "
                    & "invalid - must run on the same CPU",
                    Message   => "Exception mismatch");
      end;
   end Validate_Switch_Destination;

end Event_Tests;
