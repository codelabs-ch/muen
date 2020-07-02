--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Match;
with Mutools.Types;

package body Mucfgcheck.Events
is

   use McKae.XML.XPath.XIA;

   --  Check event destinations of subjects with given notification mode.
   procedure Check_Event_Destination
     (XML_Data  : Muxml.XML_Data_Type;
      Mode      : String;
      Test      : Test_Function_2;
      Error_Msg : String);

   -------------------------------------------------------------------------

   procedure Check_Event_Destination
     (XML_Data  : Muxml.XML_Data_Type;
      Mode      : String;
      Test      : Test_Function_2;
      Error_Msg : String)
   is
      Nodes : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/events/event[@mode='" & Mode & "']");
      Sources : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/source/"
           & "group/*[self::event or self::default]");
      Targets : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/target/event");
   begin
      Mulog.Log (Msg => "Checking " & Mode & " destinations in"
                 & DOM.Core.Nodes.Length (List => Nodes)'Img & " event(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Event_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Event_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Event_Node,
                 Name => "name");
            Src_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Sources,
                 Ref_Attr  => "physical",
                 Ref_Value => Event_Name);
            Src_Subj : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node (Node  => Src_Node,
                                            Level => 4);
            Src_Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Src_Subj,
                 Name => "name");
            Src_Subj_CPU : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Src_Subj,
                    Name => "cpu"));
            Dst_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Targets,
                 Ref_Attr  => "physical",
                 Ref_Value => Event_Name);
            Dst_Subj : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node (Node  => Dst_Node,
                                            Level => 3);
            Dst_Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dst_Subj,
                 Name => "name");
            Dst_Subj_CPU : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Dst_Subj,
                    Name => "cpu"));
         begin
            if not Test (Src_Subj_CPU, Dst_Subj_CPU) then
               raise Validation_Error with "Destination subject '"
                 & Dst_Subj_Name & "' (CPU" & Dst_Subj_CPU'Img & ") in "
                 & "subject's '" & Src_Subj_Name & "' (CPU" & Src_Subj_CPU'Img
                 & ") " & Mode & " notification '" & Event_Name & "' "
                 & "invalid - " & Error_Msg;
            end if;
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
                               Error_Msg => "must run on different CPU");
   end IPI_Different_Core;

   -------------------------------------------------------------------------

   procedure Kernel_Mode_Event_Actions (XML_Data : Muxml.XML_Data_Type)
   is
      Events : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/events/event[@mode='kernel']");
      Src_Events_No_Action : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/source/group/"
           & "event[count(*) = 0]");
      Count : constant Natural
        := DOM.Core.Nodes.Length (List => Events);
   begin
      if Count = 0 then
         return;
      end if;

      Mulog.Log (Msg => "Checking" & Count'Img
                 & " kernel-mode event action(s)");

      for I in 0 .. Count - 1 loop
         declare
            use type DOM.Core.Node;

            Ev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Events,
                 Index => I);
            Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Ev,
                 Name => "name");
            Source_Without_Action : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Src_Events_No_Action,
                 Ref_Attr  => "physical",
                 Ref_Value => Name);
         begin
            if Source_Without_Action /= null then
               declare
                  Ev_Logical : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Source_Without_Action,
                       Name => "logical");
                  Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Muxml.Utils.Ancestor_Node
                         (Node  => Source_Without_Action,
                          Level => 4),
                       Name => "name");
               begin
                  raise Validation_Error with "Kernel-mode source event '"
                    & Ev_Logical & "' of subject '" & Subj_Name & "' does not"
                    & " specify mandatory event action";
               end;
            end if;
         end;
      end loop;
   end Kernel_Mode_Event_Actions;

   -------------------------------------------------------------------------

   procedure Kernel_Mode_System_Actions (XML_Data : Muxml.XML_Data_Type)
   is
      Actions : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/source/group/"
           & "event[system_reboot|system_panic|system_poweroff|unmask_irq]");
      Pairs : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (Left_Nodes  => Actions,
           Right_Nodes => McKae.XML.XPath.XIA.XPath_Query
             (N     => XML_Data.Doc,
              XPath => "/system/events/event[@mode!='kernel']"),
           Match       => Mutools.Match.Is_Valid_Reference'Access);
   begin
      Mulog.Log (Msg => "Checking physical event reference of"
                 & DOM.Core.Nodes.Length (List => Actions)'Img
                 & " system action(s)");

      if DOM.Core.Nodes.Length (List => Pairs.Left) > 0 then
         declare
            Ev_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Pairs.Left,
                 Index => 0);
            Ev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Ev_Node,
                 Name => "logical");
            Phys_Ev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Item
                   (List  => Pairs.Right,
                    Index => 0),
                 Name => "name");
            Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Ev_Node,
                 Level => 4);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
         begin
            raise Validation_Error with "System action for event '"
              & Ev_Name & "' of subject '" & Subj_Name & "' does not reference"
              & " physical kernel-mode event '" & Phys_Ev_Name & "'";
         end;
      end if;
   end Kernel_Mode_System_Actions;

   -------------------------------------------------------------------------

   procedure Level_Triggered_Unmask_IRQ_Action (XML_Data : Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Phys_Devs : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device"
           & "[pci/@msi='false' and irq]");
      Log_Devs : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/devices/device");
      Unmask_Events : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/source"
           & "/group/*/unmask_irq");
      Dev_Count : constant Natural := DOM.Core.Nodes.Length
        (List => Phys_Devs);

      --  Returns True if the given physical IRQ of the device with specified
      --  name is assigned to a subject.
      function IRQ_Is_Assigned
        (Phys_IRQ      : DOM.Core.Node;
         Phys_Dev_Name : String)
         return Boolean;

      --  Check presence/absence of corresponding unmask event for given
      --  physical IRQ depending on assignment of the given IRQ.
      procedure Check_IRQ_Unmask_Event
        (Phys_IRQ : DOM.Core.Node;
         Dev_Name : String;
         Assigned : Boolean);

      ----------------------------------------------------------------------

      procedure Check_IRQ_Unmask_Event
        (Phys_IRQ : DOM.Core.Node;
         Dev_Name : String;
         Assigned : Boolean)
      is
         IRQ_Nr : constant String
           := DOM.Core.Elements.Get_Attribute (Elem => Phys_IRQ,
                                               Name => "number");
         IRQ_Name : constant String
           := DOM.Core.Elements.Get_Attribute (Elem => Phys_IRQ,
                                               Name => "name");
         Unmask_Ev : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes     => Unmask_Events,
              Ref_Attr  => "number",
              Ref_Value => IRQ_Nr);
      begin
         if Assigned and then Unmask_Ev = null then
            raise Validation_Error with "No event with unmask_irq action "
              & "and matching number " & IRQ_Nr & " for IRQ '" & Dev_Name
              & "->" & IRQ_Name & "'";
         end if;

         if not Assigned and then Unmask_Ev /= null then
            declare
               Ev_Name : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => DOM.Core.Nodes.Parent_Node (N => Unmask_Ev),
                  Name => "logical");
               Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Muxml.Utils.Ancestor_Node (Node  => Unmask_Ev,
                                                     Level => 5),
                  Name => "name");
            begin
               raise Validation_Error with "Event " & Ev_Name & " of subject '"
                 & Subj_Name & "' has unmask_irq action "
                 & "for unassigned IRQ '" & Dev_Name & "->" & IRQ_Name & "'";
            end;
         end if;
      end Check_IRQ_Unmask_Event;

      ----------------------------------------------------------------------

      function IRQ_Is_Assigned
        (Phys_IRQ      : DOM.Core.Node;
         Phys_Dev_Name : String)
         return Boolean
      is
         Phys_IRQ_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Phys_IRQ,
            Name => "name");
         Log_Dev : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes     => Log_Devs,
              Ref_Attr  => "physical",
              Ref_Value => Phys_Dev_Name);
         Log_IRQ : constant DOM.Core.Node
           := (if Log_Dev = null then null
               else Muxml.Utils.Get_Element
                 (Doc   => Log_Dev,
                  XPath => "irq[@physical='" & Phys_IRQ_Name & "']"));
      begin
         return Log_IRQ /= null;
      end IRQ_Is_Assigned;
   begin
      Mulog.Log (Msg => "Checking" & Dev_Count'Img & " physical devices for "
                 & "presence of corresponding IRQ unmask event(s)");

      for I in 0 .. Dev_Count - 1 loop
         declare
            Phys_Dev : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Phys_Devs,
               Index => I);
            Phys_Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Phys_Dev,
                                                  Name => "name");
            Phys_IRQs : constant DOM.Core.Node_List
              := XPath_Query
                (N     => Phys_Dev,
                 XPath => "irq");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Phys_IRQs) - 1
            loop
               declare
                  Phys_IRQ : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Phys_IRQs,
                                            Index => J);
               begin
                  Check_IRQ_Unmask_Event
                    (Phys_IRQ => Phys_IRQ,
                     Dev_Name => Phys_Dev_Name,
                     Assigned => IRQ_Is_Assigned
                       (Phys_IRQ      => Phys_IRQ,
                        Phys_Dev_Name => Phys_Dev_Name));
               end;
            end loop;
         end;
      end loop;
   end Level_Triggered_Unmask_IRQ_Action;

   -------------------------------------------------------------------------

   procedure Physical_Event_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Events : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/events/event");

      --  Check inequality of event names.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "name");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         if Left_Name = Right_Name then
            raise Validation_Error with "Multiple physical events with name '"
              & Left_Name & "'";
         end if;
      end Check_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Events)'Img & " event name(s)");

      Compare_All (Nodes      => Events,
                   Comparator => Check_Inequality'Access);
   end Physical_Event_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Self_Event_Action (XML_Data : Muxml.XML_Data_Type)
   is
      Events  : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/events/event[@mode='self']");
      Targets : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/target/event");
   begin
      Mulog.Log (Msg => "Checking self-event target actions of"
                 & DOM.Core.Nodes.Length (List => Events)'Img & " event(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Events) - 1 loop
         declare
            use type DOM.Core.Node;

            Ev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Item
                   (List  => Events,
                    Index => I),
                 Name => "name");
            Target_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Targets,
                 Ref_Attr  => "physical",
                 Ref_Value => Ev_Name);
            Target_Logical : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Target_Node,
                 Name => "logical");
            Target_Action  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Target_Node,
                 XPath => "*");
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Muxml.Utils.Ancestor_Node
                   (Node  => Target_Node,
                    Level => 3),
                 Name => "name");
         begin
            if Target_Action = null then
               raise Validation_Error with "Self-event '" & Target_Logical
                 & "' of subject '" & Subj_Name & "' does not specify an "
                 & "action";
            end if;
         end;
      end loop;
   end Self_Event_Action;

   -------------------------------------------------------------------------

   procedure Self_References (XML_Data : Muxml.XML_Data_Type)
   is
      Events  : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/events/event");
      Sources : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/source/"
           & "group/*[self::event or self::default]");
      Targets : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/target/event");
   begin
      Mulog.Log (Msg => "Checking self-references in" & DOM.Core.Nodes.Length
                 (List => Sources)'Img & " subject event(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Sources) - 1 loop
         declare
            use type DOM.Core.Node;

            Src_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Sources,
                                      Index => I);
            Src_Subj : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node (Node  => Src_Node,
                                            Level => 4);
            Src_Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Src_Subj,
                 Name => "name");
            Dst_Event_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Src_Node,
                 Name => "physical");
            Dst_Event_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element (Nodes     => Targets,
                                          Ref_Attr  => "physical",
                                          Ref_Value => Dst_Event_Name);
            Dst_Subj : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node (Node  => Dst_Event_Node,
                                            Level => 3);
            Ev_Mode : constant String
              := Muxml.Utils.Get_Attribute
                (Nodes     => Events,
                 Ref_Attr  => "name",
                 Ref_Value => Dst_Event_Name,
                 Attr_Name => "mode");
         begin
            if Ev_Mode = "self" then
               if Dst_Subj /= Src_Subj then
                  raise Validation_Error with "Reference to other subject in "
                    & "self-event '" & Dst_Event_Name & "' of subject '"
                    & Src_Subj_Name & "'";
               end if;
            else
               if Dst_Subj = Src_Subj then
                  raise Validation_Error with "Reference to self in event '"
                    & Dst_Event_Name & "' of subject '" & Src_Subj_Name & "'";
               end if;
            end if;
         end;
      end loop;
   end Self_References;

   -------------------------------------------------------------------------

   procedure Source_Group_Event_ID_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;

      Subj_Name : Unbounded_String;

      --  Check inequality of event ID.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_ID : constant Natural
           := Natural'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Left,
                 Name => "id"));
         Left_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Left,
              Name => "logical");
         Right_ID : constant Natural
           := Natural'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Right,
                 Name => "id"));
         Right_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Right,
              Name => "logical");
      begin
         if Left_ID = Right_ID then
            raise Validation_Error with "Subject '" & To_String (Subj_Name)
              & "' source events '" & Left_Name & "' and '" & Right_Name
              & "' share ID" & Left_ID'Img;
         end if;
      end Check_Inequality;

      Subject_Event_Groups : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/source/group");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subject_Event_Groups) - 1
      loop
         declare
            Ev_Grp_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subject_Event_Groups,
                 Index => I);
            Ev_Group_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Ev_Grp_Node,
                 Name => "name");
            Events : constant DOM.Core.Node_List
              := XPath_Query
                 (N     => Ev_Grp_Node,
                  XPath => "event");
         begin
            if DOM.Core.Nodes.Length (List => Events) > 1 then
               Subj_Name := To_Unbounded_String
                 (DOM.Core.Elements.Get_Attribute
                    (Elem => Muxml.Utils.Ancestor_Node
                         (Node  => Ev_Grp_Node,
                          Level => 3),
                     Name => "name"));

               Mulog.Log (Msg => "Checking uniqueness of"
                          & DOM.Core.Nodes.Length (List => Events)'Img
                          & " " & Ev_Group_Name & " source event ID(s) for "
                          & "subject '" & To_String (Subj_Name) & "'");
               Compare_All (Nodes      => Events,
                            Comparator => Check_Inequality'Access);
            end if;
         end;
      end loop;
   end Source_Group_Event_ID_Uniqueness;

   -------------------------------------------------------------------------

   procedure Source_Group_Event_ID_Validity (XML_Data : Muxml.XML_Data_Type)
   is
      Events : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/source/group/event");
   begin
      Mulog.Log (Msg => "Checking validity of" & DOM.Core.Nodes.Length
                 (List => Events)'Img & " source event ID(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Events) - 1 loop
         declare
            Event_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Events,
                 Index => I);
            Event_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Event_Node,
                 Name => "logical");
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Muxml.Utils.Ancestor_Node
                   (Node  => Event_Node,
                    Level => 4),
                 Name => "name");
            Event_ID : constant Natural
              := Natural'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Event_Node,
                    Name => "id"));
            Event_Group : constant Mutools.Types.Event_Group_Type
              := Mutools.Types.Event_Group_Type'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => DOM.Core.Nodes.Parent_Node (N => Event_Node),
                    Name => "name"));
         begin
            if not Mutools.Types.Is_Valid_Event_ID
              (Group => Event_Group,
               ID    => Event_ID)
            then
               raise Validation_Error with "Subject '" & Subj_Name & "': ID"
                 & Event_ID'Img & " of event '" & Event_Name & "' invalid for "
                 & "group " & Event_Group'Img;
            end if;
         end;
      end loop;
   end Source_Group_Event_ID_Validity;

   -------------------------------------------------------------------------

   procedure Source_Targets (XML_Data : Muxml.XML_Data_Type)
   is
      Events : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/events/event");
      Sources : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/source/group/"
           & "*[self::event or self::default]");
      Targets : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/target/event");
   begin
      Mulog.Log (Msg => "Checking" & DOM.Core.Nodes.Length
                 (List => Events)'Img & " event source/target connection(s)");
      for I in 0 .. DOM.Core.Nodes.Length (List => Events) - 1 loop
         declare
            Event : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Events,
                 Index => I);
            Event_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Event,
                 Name => "name");
            Kernel_Mode : constant Boolean
              := DOM.Core.Elements.Get_Attribute
                (Elem => Event,
                 Name => "mode") = "kernel";
            Source_Count : constant Natural
              := DOM.Core.Nodes.Length
                (List => Muxml.Utils.Get_Elements
                   (Nodes     => Sources,
                    Ref_Attr  => "physical",
                    Ref_Value => Event_Name));
            Target_Count : constant Natural
              := DOM.Core.Nodes.Length
                (List => Muxml.Utils.Get_Elements
                   (Nodes     => Targets,
                    Ref_Attr  => "physical",
                    Ref_Value => Event_Name));
         begin
            if Source_Count = 0 then
               raise Mucfgcheck.Validation_Error with "Invalid number of "
                 & "sources for event '" & Event_Name & "':"
                 & Source_Count'Img;
            end if;

            if Kernel_Mode then
               if Target_Count > 0 then
                  raise Mucfgcheck.Validation_Error with "Invalid number of "
                    & "targets for kernel-mode event '" & Event_Name & "':"
                    & Target_Count'Img & " (no target allowed)";
               end if;
            else
               if Target_Count /= 1 then
                  raise Mucfgcheck.Validation_Error with "Invalid number of "
                    & "targets for event '" & Event_Name & "':"
                    & Target_Count'Img;
               end if;
            end if;

         end;
      end loop;
   end Source_Targets;

   -------------------------------------------------------------------------

   procedure Source_VMX_Exit_Event_Completeness
     (XML_Data : Muxml.XML_Data_Type)
   is

      --  Returns True if the given node list contains a <default> element.
      function Contains_Default_Event
        (List : DOM.Core.Node_List)
         return Boolean;

      ----------------------------------------------------------------

      function Contains_Default_Event
        (List : DOM.Core.Node_List)
         return Boolean
      is
         Cur_Node : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => List) - 1 loop
            Cur_Node := DOM.Core.Nodes.Item (List  => List,
                                             Index => I);
            if DOM.Core.Nodes.Node_Name (N => Cur_Node) = "default" then
               return True;
            end if;
         end loop;

         return False;
      end Contains_Default_Event;

      ----------------------------------------------------------------------

      Max_VMX_Exit_ID : constant Natural
        := Mutools.Types.Get_Max_ID (Group => Mutools.Types.Vmx_Exit);
      Subjects : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            use type DOM.Core.Node;

            Subject : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Subjects,
               Index => I);
            Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Subject,
               Name => "name");
            Vmx_Exit_Grp : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Subject,
               XPath => "events/source/group[@name='vmx_exit']");
            Src_Events : DOM.Core.Node_List;
         begin
            Mulog.Log (Msg => "Checking 'vmx_exit' group source event "
                       & "completeness for subject '" & Subj_Name & "'");
            if Vmx_Exit_Grp = null then
               raise Validation_Error with "Subject '" & Subj_Name
                 & "' does not specify any source event in 'vmx_exit' group";
            end if;

            Src_Events := XPath_Query
              (N     => Vmx_Exit_Grp,
               XPath => "*");
            if not Contains_Default_Event (List => Src_Events) then
               for Ev_ID in 0 .. Max_VMX_Exit_ID loop
                  if Mutools.Types.Is_Valid_Event_ID
                    (Group => Mutools.Types.Vmx_Exit,
                     ID    => Ev_ID)
                    and then Muxml.Utils.Get_Element
                      (Nodes     => Src_Events,
                       Ref_Attr  => "id",
                       Ref_Value => Ada.Strings.Fixed.Trim
                         (Source => Ev_ID'Img,
                          Side   => Ada.Strings.Left)) = null
                  then
                     raise Validation_Error with "Subject '" & Subj_Name
                       & "' does not specify 'vmx_exit' group source event "
                       & "with ID" & Ev_ID'Img;
                  end if;
               end loop;
            end if;
         end;
      end loop;
   end Source_VMX_Exit_Event_Completeness;

   -------------------------------------------------------------------------

   procedure Subject_Event_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Match name of reference and event.
      function Match_Event_Name (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Ref_Event_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
         Parent_Tag : constant String := DOM.Core.Nodes.Node_Name
           (N => DOM.Core.Nodes.Parent_Node (N => Node));
         Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Muxml.Utils.Ancestor_Node
              (Node  => Node,
               Level => (if Parent_Tag = "group" then 4 else 3)),
            Name => "name");
      begin
         return "Event '" & Ref_Event_Name & "' referenced by subject '"
           & Subj_Name & "' does not exist";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_Event_Name (Left, Right : DOM.Core.Node) return Boolean
      is
         Ref_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "physical");
         Event_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         return Ref_Name = Event_Name;
      end Match_Event_Name;
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject/events//*[@physical]",
         Ref_XPath    => "/system/events/event",
         Log_Message  => "subject event reference(s)",
         Error        => Error_Msg'Access,
         Match        => Match_Event_Name'Access);
   end Subject_Event_References;

   -------------------------------------------------------------------------

   procedure Switch_Same_Core (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Event_Destination (XML_Data  => XML_Data,
                               Mode      => "switch",
                               Test      => Equals'Access,
                               Error_Msg => "must run on the same CPU");
   end Switch_Same_Core;

   -------------------------------------------------------------------------

   procedure Target_Event_ID_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;

      Subj_Name : Unbounded_String;

      --  Check inequality of event ID.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_ID : constant Natural
           := Natural'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Left,
                 Name => "id"));
         Left_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Left,
              Name => "logical");
         Right_ID : constant Natural
           := Natural'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Right,
                 Name => "id"));
         Right_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Right,
              Name => "logical");
      begin
         if Left_ID = Right_ID then
            raise Validation_Error with "Subject '" & To_String (Subj_Name)
              & "' target events '" & Left_Name & "' and '" & Right_Name
              & "' share ID" & Left_ID'Img;
         end if;
      end Check_Inequality;

      Subject_Target_Events : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/events/target");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subject_Target_Events) - 1
      loop
         declare
            Event_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subject_Target_Events,
                 Index => I);
            Events : constant DOM.Core.Node_List
              := XPath_Query
                 (N     => Event_Node,
                  XPath => "event");
         begin
            if DOM.Core.Nodes.Length (List => Events) > 1 then
               Subj_Name := To_Unbounded_String
                 (DOM.Core.Elements.Get_Attribute
                    (Elem => Muxml.Utils.Ancestor_Node
                         (Node  => Event_Node,
                          Level => 2),
                     Name => "name"));

               Mulog.Log (Msg => "Checking uniqueness of"
                          & DOM.Core.Nodes.Length (List => Events)'Img
                          & " target event ID(s) for subject '"
                          & To_String (Subj_Name) & "'");
               Compare_All (Nodes      => Events,
                            Comparator => Check_Inequality'Access);
            end if;
         end;
      end loop;
   end Target_Event_ID_Uniqueness;

end Mucfgcheck.Events;
