--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;
with Mulog;

with Muxml.Utils;

package body Expanders.Events
is

   -------------------------------------------------------------------------

   procedure Handle_Asap_Events (Data : in out Muxml.XML_Data_Type)
   is
      Event_Nodes   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/events/event[@mode='asap']");
      Source_Events : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/events/source/group/event");
      Target_Events : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/events/target/event");
   begin
      Mulog.Log (Msg => "Setting mode for" & DOM.Core.Nodes.Length
                 (List => Event_Nodes)'Img & " 'asap' event(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Event_Nodes) - 1 loop
         declare
            Event : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Event_Nodes,
                 Index => I);
            Ev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Event,
                 Name => "name");
            Source : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Source_Events,
                 Ref_Attr  => "physical",
                 Ref_Value => Ev_Name);
            Source_Subject : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Source,
                 Level => 4);
            Source_CPU : constant Natural
              := Natural'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Source_Subject,
                    Name => "cpu"));
            Target : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Target_Events,
                 Ref_Attr  => "physical",
                 Ref_Value => Ev_Name);
            Target_Subject : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Target,
                 Level => 3);
            Target_CPU : constant Natural
              := Natural'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Target_Subject,
                    Name => "cpu"));
         begin
            if Source_CPU = Target_CPU then
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Event,
                  Name  => "mode",
                  Value => "async");
            else
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Event,
                  Name  => "mode",
                  Value => "ipi");
            end if;
         end;
      end loop;
   end Handle_Asap_Events;

end Expanders.Events;
