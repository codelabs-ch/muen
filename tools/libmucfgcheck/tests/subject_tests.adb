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

with Mucfgcheck.Subject;

package body Subject_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Subject validator tests");
      T.Add_Test_Routine
        (Routine => Validate_CPU_IDs'Access,
         Name    => "Validate CPU IDs");
      T.Add_Test_Routine
        (Routine => Validate_Event_Subject_References'Access,
         Name    => "Validate event table subject references");
      T.Add_Test_Routine
        (Routine => Validate_Event_Self_References'Access,
         Name    => "Validate event table self-references");
      T.Add_Test_Routine
        (Routine => Validate_Event_Switch_Destination'Access,
         Name    => "Validate event switch destination");
      T.Add_Test_Routine
        (Routine => Validate_Event_IPI_Destination'Access,
         Name    => "Validate event IPI destination");
      T.Add_Test_Routine
        (Routine => Validate_Name_Uniqueness'Access,
         Name    => "Validate name uniqueness");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Validate_CPU_IDs
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");
      declare
         Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Data.Doc,
               XPath => "/system/subjects/subject"),
            Index => 0);
      begin

         --  Set invalid CPU ID.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "cpu",
            Value => "7");

         Mucfgcheck.Subject.CPU_ID (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'cpu => 7' of 'linux' subject element not in "
                    & "valid range 0 .. 0",
                    Message   => "Exception mismatch");
      end;
   end Validate_CPU_IDs;

   -------------------------------------------------------------------------

   procedure Validate_Event_IPI_Destination
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Subject.Event_IPI_Different_Core (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Destination subject 'linux' (CPU 0) in subject's "
                    & "'linux' (CPU 0) ipi notification 'forward_keyboard' "
                    & "invalid - no IPI allowed",
                    Message   => "Exception mismatch");
      end;
   end Validate_Event_IPI_Destination;

   -------------------------------------------------------------------------

   procedure Validate_Event_Self_References
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Subject.Event_Self_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Reference to self in event table entry "
                    & "'forward_keyboard' of subject 'linux'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Event_Self_References;

   -------------------------------------------------------------------------

   procedure Validate_Event_Subject_References
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Subject.Event_Subject_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Reference to unknown subject 'nonexistent' in event "
                    & "'invalid_subject'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Event_Subject_References;

   -------------------------------------------------------------------------

   procedure Validate_Event_Switch_Destination
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Subject.Event_Switch_Same_Core (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Destination subject 'linux' (CPU 0) in subject's "
                    & "'subject1' (CPU 1) switch notification 'linux_switch' "
                    & "invalid - must run on the same CPU",
                    Message   => "Exception mismatch");
      end;
   end Validate_Event_Switch_Destination;

   -------------------------------------------------------------------------

   procedure Validate_Name_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");
      declare
         Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Data.Doc,
               XPath => "/system/subjects/subject[@name='subject1']"),
            Index => 0);
      begin

         --  Set duplicate subject name.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "name",
            Value => "linux");

         Mucfgcheck.Subject.Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subjects with id 0 and 1 have identical name 'linux'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Name_Uniqueness;

end Subject_Tests;
