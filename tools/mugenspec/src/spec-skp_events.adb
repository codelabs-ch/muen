--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Templates;

with String_Templates;

package body Spec.Skp_Events
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;

      Subjects      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Subj_Count    : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);
      Phys_Events   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/events/event");
      Target_Events : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject/events/target/event");

      Buffer : Unbounded_String;
      Tmpl   : Mutools.Templates.Template_Type;

      --  Add event action entry to template buffer.
      procedure Add_Event_Action_Entry (Event : DOM.Core.Node);

      --  Add source event entry to template buffer.
      procedure Add_Event_Entry (Event : DOM.Core.Node);

      --  Append SPARK events spec of given subject to template buffer.
      procedure Write_Subject_Event_Spec (Subject : DOM.Core.Node);

      -------------------------------------------------------------------

      procedure Add_Event_Action_Entry (Event : DOM.Core.Node)
      is
         use type DOM.Core.Node;

         Event_ID : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Event,
              Name => "id");
         Action : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Event,
              XPath => "*");
         Action_Kind : constant String
           := (if Action /= null then DOM.Core.Nodes.Node_Name (N => Action)
               else "No_Action");
      begin
         Buffer := Buffer & Indent (N => 3)  & " "
           & Event_ID & " => Event_Action_Type'("
           & ASCII.LF
           & Indent (N => 4) & "Kind   => "
           & Mutools.Utils.To_Ada_Identifier (Str => Action_Kind) & ",";

         Buffer := Buffer & ASCII.LF & Indent (N => 4) & "Vector => ";
         if Action_Kind = "inject_interrupt" then
            Buffer := Buffer & DOM.Core.Elements.Get_Attribute
              (Elem => Action,
               Name => "vector");
         else
            Buffer := Buffer & "Invalid_Vector";
         end if;

         Buffer := Buffer & ")";
      end Add_Event_Action_Entry;

      -------------------------------------------------------------------

      procedure Add_Event_Entry (Event : DOM.Core.Node)
      is
         use type DOM.Core.Node;

         Event_ID : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Event,
              Name => "id");
         Phys_Event_Ref : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Event,
              Name => "physical");
         Notify_Mode : constant String
           := Muxml.Utils.Get_Attribute
             (Nodes     => Phys_Events,
              Ref_Attr  => "name",
              Ref_Value => Phys_Event_Ref,
              Attr_Name => "mode");
         Src_Action : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Event,
              XPath => "*");
         Src_Action_Kind : constant String
           := (if Src_Action /= null then Mutools.Utils.To_Ada_Identifier
               (Str => DOM.Core.Nodes.Node_Name (N => Src_Action))
               else "No_Action");
         Event_Target : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes     => Target_Events,
              Ref_Attr  => "physical",
              Ref_Value => Phys_Event_Ref);
         Target_Subj_ID : constant String
           := (if Event_Target = null then "Skp.Invalid_Subject"
               else DOM.Core.Elements.Get_Attribute
                 (Elem => Muxml.Utils.Ancestor_Node
                      (Node  => Event_Target,
                       Level => 3),
                  Name => "globalId"));
         Target_Event_ID : constant String
           := (if Event_Target = null then "Invalid_Target_Event"
               else DOM.Core.Elements.Get_Attribute
                 (Elem => Event_Target,
                  Name => "id"));
      begin
         Buffer := Buffer & Indent (N => 3)  & " "
           & Event_ID & " => Event_Entry_Type'("
           & ASCII.LF
           & Indent (N => 4) & "Source_Action  => " & Src_Action_Kind & ","
           & ASCII.LF
           & Indent (N => 4) & "Target_Subject => " & Target_Subj_ID & ","
           & ASCII.LF
           & Indent (N => 4) & "Target_Event   => " & Target_Event_ID & ",";

         Buffer := Buffer & ASCII.LF & Indent (N => 4) & "Handover       => ";
         if Notify_Mode = "switch" then
            Buffer := Buffer & "True,";
         else
            Buffer := Buffer & "False,";
         end if;

         Buffer := Buffer & ASCII.LF & Indent (N => 4) & "Send_IPI       => ";
         if Notify_Mode = "ipi" then
            Buffer := Buffer & "True)";
         else
            Buffer := Buffer & "False)";
         end if;
      end Add_Event_Entry;

      ----------------------------------------------------------------------

      procedure Write_Subject_Event_Spec (Subject : DOM.Core.Node)
      is
         use type DOM.Core.Node;

         Subj_ID : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Subject,
            Name => "globalId");
         Traps : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "events/source/group[@name='vmx_exit']/*");
         Trap_Count : constant Natural := DOM.Core.Nodes.Length
           (List => Traps);
         Src_Events : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "events/source/group[@name='vmcall']/*");
         Target_Events : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "events/target/event");
         Src_Ev_Count : constant Natural := DOM.Core.Nodes.Length
           (List => Src_Events);
         Target_Ev_Count : constant Natural := DOM.Core.Nodes.Length
           (List => Target_Events);
      begin
         Buffer := Buffer & Indent (N => 2) & Subj_ID
           & " => Subject_Events_Type'(" & ASCII.LF
           & Indent & "    Source_Traps  => ";

         if Trap_Count = 0 then
            Buffer := Buffer & "Null_Trap_Table,";
         else
            Buffer := Buffer & "Trap_Table_Type'(" & ASCII.LF;
            for I in 0 .. Trap_Count - 1 loop
               Add_Event_Entry (Event => DOM.Core.Nodes.Item
                                (List  => Traps,
                                 Index => I));
               if I < Trap_Count - 1 then
                  Buffer := Buffer & "," & ASCII.LF;
               end if;
            end loop;

            Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
              & " others => Null_Event),";
         end if;

         Buffer := Buffer & ASCII.LF
           & Indent & "    Source_Events => ";

         if Src_Ev_Count = 0 then
            Buffer := Buffer & "Null_Event_Table,";
         else
            Buffer := Buffer & "Event_Table_Type'(" & ASCII.LF;
            for I in 0 .. Src_Ev_Count - 1 loop
               Add_Event_Entry (Event => DOM.Core.Nodes.Item
                                (List  => Src_Events,
                                 Index => I));
               if I < Src_Ev_Count - 1 then
                  Buffer := Buffer & "," & ASCII.LF;
               end if;
            end loop;

            if Src_Ev_Count /= 32 then
               Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
                 & " others => Null_Event";
            end if;
            Buffer := Buffer & "),";
         end if;

         Buffer := Buffer & ASCII.LF
           & Indent & "    Target_Events => ";

         if Target_Ev_Count = 0 then
            Buffer := Buffer & "Null_Event_Action_Table)";
         else
            Buffer := Buffer & "Event_Action_Table_Type'(" & ASCII.LF;
            for I in 0 .. Target_Ev_Count - 1 loop
               Add_Event_Action_Entry (Event => DOM.Core.Nodes.Item
                                       (List  => Target_Events,
                                        Index => I));
               if I < Target_Ev_Count - 1 then
                  Buffer := Buffer & "," & ASCII.LF;
               end if;
            end loop;

            if Target_Ev_Count /= 32 then
               Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
                 & " others => Null_Event_Action";
            end if;
            Buffer := Buffer & "))";
         end if;
      end Write_Subject_Event_Spec;
   begin
      Mulog.Log (Msg => "Writing event spec to '"
                 & Output_Dir & "/skp-events.adb'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_events_ads);
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-events.ads");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_events_adb);

      for I in 0 .. Subj_Count - 1 loop
         Write_Subject_Event_Spec
           (Subject => DOM.Core.Nodes.Item
              (List  => Subjects,
               Index => I));
         if I < Subj_Count - 1 then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
      end loop;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__events__",
         Content  => To_String (Buffer));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-events.adb");
   end Write;

end Spec.Skp_Events;
