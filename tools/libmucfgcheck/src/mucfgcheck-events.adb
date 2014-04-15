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

package body Mucfgcheck.Events
is

   use McKae.XML.XPath.XIA;

   --  Check event notification destinations of subjects with given
   --  notification mode.
   procedure Check_Event_Destination
     (XML_Data  : Muxml.XML_Data_Type;
      Mode      : String;
      Test      : Test_Function;
      Error_Msg : String);

   -------------------------------------------------------------------------

   procedure Check_Event_Destination
     (XML_Data  : Muxml.XML_Data_Type;
      Mode      : String;
      Test      : Test_Function;
      Error_Msg : String)
   is
      Nodes : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/subjects/subject");
   begin
      Mulog.Log (Msg => "Checking " & Mode & " destinations in"
                 & DOM.Core.Nodes.Length (List => Nodes)'Img
                 & " subject event table(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Subj_CPU  : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                     (Elem => Subj_Node,
                      Name => "cpu"));
            Events    : constant DOM.Core.Node_List
              := XPath_Query
                (N     => Subj_Node,
                 XPath => "events/source/group/event/notify[@mode='"
                 & Mode & "']");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Events) - 1 loop
               declare
                  Event_Node    : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Events,
                       Index => J);
                  Event_Name    : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => DOM.Core.Nodes.Parent_Node (N => Event_Node),
                       Name => "logical");
                  Ref_Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Event_Node,
                       Name => "subject");
                  Ref_Subj_CPU  : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Nodes.Node_Value
                           (DOM.Core.Nodes.Item
                                (List  => XPath_Query
                                     (N     => XML_Data.Doc,
                                      XPath => "/system/subjects/subject["
                                      & "@name='" & Ref_Subj_Name & "']/@cpu"),
                                 Index => 0)));
               begin
                  if not Test (Ref_Subj_CPU, Subj_CPU) then
                     raise Validation_Error with "Destination subject '"
                       & Ref_Subj_Name & "' (CPU" & Ref_Subj_CPU'Img & ") in "
                       & "subject's '" & Subj_Name & "' (CPU" & Subj_CPU'Img
                       & ") " & Mode & " notification '" & Event_Name & "' "
                       & "invalid - " & Error_Msg;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Check_Event_Destination;

   -------------------------------------------------------------------------

   procedure IPI_Different_Core (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Event_Destination (XML_Data  => XML_Data,
                               Mode      => "ipi",
                               Test      => Not_Equals'Access,
                               Error_Msg => "no IPI allowed");
   end IPI_Different_Core;

   -------------------------------------------------------------------------

   procedure Self_References (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/subjects/subject");
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
                              XPath => "events/source/group/event/notify["
                              & "@subject='" & Subj_Name & "']");
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
   end Self_References;

   -------------------------------------------------------------------------

   procedure Subject_References (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/subjects/subject/events/source/group"
                        & "/event/notify");
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
                              XPath => "/system/subjects/subject[@name='"
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
   end Subject_References;

   -------------------------------------------------------------------------

   procedure Switch_Same_Core (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Event_Destination (XML_Data  => XML_Data,
                               Mode      => "switch",
                               Test      => Equals'Access,
                               Error_Msg => "must run on the same CPU");
   end Switch_Same_Core;

end Mucfgcheck.Events;
