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
with DOM.Core.Documents;

with Muxml.Utils;
with Mucfgcheck;

with Expand.Pre_Checks;

package body Pre_Check_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Channel_Reader_Has_Event_Vector
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/channels/reader"
         & "[@ref='data_channel']",
         Name  => "vector",
         Value => "");

      begin
         Expand.Pre_Checks.Channel_Reader_Has_Event_Vector
           (XML_Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Missing 'vector' attribute for reader of channel "
                    & "'data_channel'",
                    Message   => "Exception mismatch");
      end;
   end Channel_Reader_Has_Event_Vector;

   -------------------------------------------------------------------------

   procedure Channel_Reader_Writer
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/channels/writer"
         & "[@ref='data_channel']",
         Name  => "ref",
         Value => "nonexistent");

      begin
         Expand.Pre_Checks.Channel_Reader_Writer (XML_Data => Policy);
         Fail (Message => "Exception expected (writer)");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid number of writers for channel 'data_channel':"
                    & " 0",
                    Message   => "Exception mismatch (writer)");
      end;

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/channels/reader"
         & "[@ref='data_channel']",
         Name  => "ref",
         Value => "nonexistent");

      begin
         Expand.Pre_Checks.Channel_Reader_Writer (XML_Data => Policy);
         Fail (Message => "Exception expected (reader)");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid number of readers for channel 'data_channel':"
                    & " 0",
                    Message   => "Exception mismatch (reader)");
      end;
   end Channel_Reader_Writer;

   -------------------------------------------------------------------------

   procedure Channel_Writer_Has_Event_ID
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/channels/writer"
         & "[@ref='data_channel']",
         Name  => "event",
         Value => "");

      begin
         Expand.Pre_Checks.Channel_Writer_Has_Event_ID (XML_Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Missing 'event' attribute for writer of channel "
                    & "'data_channel'",
                    Message   => "Exception mismatch");
      end;
   end Channel_Writer_Has_Event_ID;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Pre-check tests");
      T.Add_Test_Routine
        (Routine => Tau0_Presence_In_Scheduling'Access,
         Name    => "Presence of tau0 in scheduling plan");
      T.Add_Test_Routine
        (Routine => Subject_Monitor_References'Access,
         Name    => "Subject monitor references");
      T.Add_Test_Routine
        (Routine => Subject_Channel_References'Access,
         Name    => "Subject channel references");
      T.Add_Test_Routine
        (Routine => Channel_Reader_Writer'Access,
         Name    => "Channel reader/writer counts");
      T.Add_Test_Routine
        (Routine => Channel_Writer_Has_Event_ID'Access,
         Name    => "Channel writers event IDs");
      T.Add_Test_Routine
        (Routine => Channel_Reader_Has_Event_Vector'Access,
         Name    => "Channel readers vector numbers");
      T.Add_Test_Routine
        (Routine => Platform_CPU_Count_Presence'Access,
         Name    => "Logical CPU count attribute");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Platform_CPU_Count_Presence
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Remove_Child
        (Node       => DOM.Core.Documents.Get_Element (Doc => Policy.Doc),
         Child_Name => "platform");

      begin
         Expand.Pre_Checks.Platform_CPU_Count_Presence (XML_Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Required '/system/platform/processor/@logicalCpus' "
                    & "attribute not found, add it or use mucfgmerge tool",
                    Message   => "Exception mismatch");
      end;
   end Platform_CPU_Count_Presence;

   -------------------------------------------------------------------------

   procedure Subject_Channel_References
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/channels/reader"
         & "[@ref='data_channel']",
         Name  => "ref",
         Value => "nonexistent");

      begin
         Expand.Pre_Checks.Subject_Channel_References (XML_Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Channel 'nonexistent' referenced by subject 'lnx' does"
                    & " not exist",
                    Message   => "Exception mismatch");
      end;
   end Subject_Channel_References;

   -------------------------------------------------------------------------

   procedure Subject_Monitor_References
   is
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
         Expand.Pre_Checks.Subject_Monitor_References (XML_Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'nonexistent' referenced by subject monitor "
                    & "'subject1' does not exist",
                    Message   => "Exception mismatch");
      end;
   end Subject_Monitor_References;

   -------------------------------------------------------------------------

   procedure Tau0_Presence_In_Scheduling
   is
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
         Expand.Pre_Checks.Tau0_Presence_In_Scheduling (XML_Data => Policy);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject tau0 not present in scheduling plan",
                    Message   => "Exception mismatch");
      end;
   end Tau0_Presence_In_Scheduling;

end Pre_Check_Tests;
