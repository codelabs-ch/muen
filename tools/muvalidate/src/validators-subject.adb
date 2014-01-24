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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;

package body Validators.Subject
is

   use McKae.XML.XPath.XIA;

   -------------------------------------------------------------------------

   procedure CPU_ID (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count : constant Positive
        := Positive'Value
          (Muxml.Utils.Get_Attribute
               (Doc   => XML_Data.Doc,
                XPath => "/system/platform/processor",
                Name  => "logicalCpus"));
      Last_Id   : constant Natural := CPU_Count - 1;
      Nodes     : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "//subjects/subject");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "subject",
                       Attr      => "cpu",
                       Name_Attr => "name",
                       Test      => Less_Or_Equal'Access,
                       Right     => Interfaces.Unsigned_64 (Last_Id),
                       Error_Msg => "not in valid range 0 .." & Last_Id'Img);
   end CPU_ID;

   -------------------------------------------------------------------------

   procedure Event_Self_References (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "//subjects/subject");
   begin
      Mulog.Log (Msg => "Checking self-references in" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " subject event table(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Entries   : constant DOM.Core.Node_List
              := XPath_Query (N     => Subj_Node,
                              XPath => "events/source//notify[@subject='"
                              & Subj_Name & "']");
         begin
            if DOM.Core.Nodes.Length (List => Entries) /= 0 then
               declare
                  Event_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Entries,
                       Index => 0);
                  Event_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => DOM.Core.Nodes.Parent_Node (N => Event_Node),
                       Name => "logical");
               begin
                  raise Validation_Error with "Reference to self in event "
                    & "table entry '" & Event_Name & "' of subject '"
                    & Subj_Name & "'";
               end;
            end if;
         end;
      end loop;
   end Event_Self_References;

   -------------------------------------------------------------------------

   procedure Event_Subject_References (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "//notify");
   begin
      Mulog.Log (Msg => "Checking subject references in"
                 & DOM.Core.Nodes.Length (List => Nodes)'Img
                 & " event notification(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Notify_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Ref    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Notify_Node,
                 Name => "subject");
            Subjects    : constant DOM.Core.Node_List
              := XPath_Query (N     => XML_Data.Doc,
                              XPath => "//subjects/subject[@name='"
                              & Subj_Ref & "']");
         begin
            if DOM.Core.Nodes.Length (List => Subjects) = 0 then
               declare
                  Event_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => DOM.Core.Nodes.Parent_Node (N => Notify_Node),
                       Name => "logical");
               begin
                  raise Validation_Error with "Reference to unknown subject '"
                    & Subj_Ref & "' in event '" & Event_Name & "'";
               end;
            end if;
         end;
      end loop;
   end Event_Subject_References;

   -------------------------------------------------------------------------

   procedure Event_Switch_Same_Core (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "//subjects/subject");
   begin
      Mulog.Log (Msg => "Checking switch destinations in"
                 & DOM.Core.Nodes.Length (List => Nodes)'Img & " subject event"
                 & " table(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Subj_CPU  : constant Natural
              := Natural'Value (DOM.Core.Elements.Get_Attribute
                                (Elem => Subj_Node,
                                 Name => "cpu"));
            Switches  : constant DOM.Core.Node_List
              := XPath_Query
                (N     => Subj_Node,
                 XPath => "events/source//notify[@mode='switch']");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Switches) - 1 loop
               declare
                  Event_Node    : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Switches,
                       Index => 0);
                  Event_Name    : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => DOM.Core.Nodes.Parent_Node (N => Event_Node),
                       Name => "logical");
                  Ref_Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Event_Node,
                       Name => "subject");
                  Ref_Subj_CPU : constant Natural
                    := Natural'Value
                      (DOM.Core.Nodes.Node_Value
                           (DOM.Core.Nodes.Item
                                (List  => XPath_Query
                                     (N     => XML_Data.Doc,
                                      XPath => "//subjects/subject[@name='"
                                      & Ref_Subj_Name & "']/@cpu"),
                                 Index => 0)));
               begin
                  if Ref_Subj_CPU /= Subj_CPU then
                     raise Validation_Error with "Destination subject '"
                       & Ref_Subj_Name & "' in subject's '" & Subj_Name
                       & "' switch notification '" & Event_Name & "' runs on "
                       & "different CPU" & Ref_Subj_CPU'Img & " (instead of"
                       & Subj_CPU'Img & ")";
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Event_Switch_Same_Core;

end Validators.Subject;
