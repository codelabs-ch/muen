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

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Constants;
with Mutools.Templates;
with Mutools.Types;

with String_Templates;

package body Spec.Skp_Events
is

   use type Ada.Containers.Hash_Type;

   Invalid_Target_Subject : constant Natural := Natural'Last;
   Invalid_Target_Event   : constant Natural := Natural'Last;

   type Source_Event_Type is record
      Source_Action  : Mutools.Types.Event_Action_Kind;
      Target_Subject : Natural;
      Target_Event   : Natural;
      Handover       : Boolean;
      Send_IPI       : Boolean;
      IRQ_Number     : Natural;
   end record;

   Invalid_Event : constant Source_Event_Type :=
     (Source_Action  => Mutools.Types.System_Panic,
      Target_Subject => Invalid_Target_Subject,
      Target_Event   => Invalid_Target_Event,
      Handover       => False,
      Send_IPI       => False,
      IRQ_Number     => 0);

   --  Returns the hash for a given source event. It is a trivial
   --  implementation and only used for calculating key hashes for hashed maps,
   --  which are not required to be (cryptographically) strong.
   function Hash (Src_Ev : Source_Event_Type) return Ada.Containers.Hash_Type
   is (Mutools.Types.Source_Event_Action_Kind'Pos (Src_Ev.Source_Action)
        + Ada.Containers.Hash_Type'Mod (Src_Ev.Target_Subject)
        + Ada.Containers.Hash_Type'Mod (Src_Ev.Target_Event)
        + (if Src_Ev.Handover then 1 else 0)
        + (if Src_Ev.Send_IPI then 1 else 0)
        + Ada.Containers.Hash_Type (Src_Ev.IRQ_Number));

   package SON is new Ada.Containers.Ordered_Sets
     (Element_Type => Natural);

   use type SON.Set;

   --  Container to map source events to the event IDs that reference a given
   --  event.
   package SEM is new Ada.Containers.Hashed_Maps
     (Key_Type        => Source_Event_Type,
      Element_Type    => SON.Set,
      Hash            => Hash,
      Equivalent_Keys => "=");

   --  Convert given source event to string representation and add it to the
   --  specified buffer.
   procedure Add_Source_Event
     (Event  :        Source_Event_Type;
      Buffer : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Return source event for given event node.
   function To_Source_Event
     (Event           : DOM.Core.Node;
      Physical_Events : DOM.Core.Node_List;
      Target_Events   : DOM.Core.Node_List)
      return Source_Event_Type;

   --  Return map of source events for given events node list.
   function To_Source_Event_Map
     (Events          : DOM.Core.Node_List;
      Event_Group     : Mutools.Types.Event_Group_Type;
      Physical_Events : DOM.Core.Node_List;
      Target_Events   : DOM.Core.Node_List)
      return SEM.Map;

   -------------------------------------------------------------------------

   procedure Add_Source_Event
     (Event  :        Source_Event_Type;
      Buffer : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      use Ada.Strings.Unbounded;
   begin
      Buffer := Buffer & "(" & ASCII.LF
        & Indent (N => 4) & "Source_Action  => "
        & Mutools.Utils.To_Ada_Identifier (Str => Event.Source_Action'Img)
        & "," & ASCII.LF
        & Indent (N => 4) & "Target_Subject =>"
        & (if Event.Target_Subject = Invalid_Target_Subject
           then " Invalid_Subject" else Event.Target_Subject'Img)
        & "," & ASCII.LF
        & Indent (N => 4) & "Target_Event   =>"
        & (if Event.Target_Event = Invalid_Target_Event
           then " Invalid_Target_Event" else Event.Target_Event'Img)
        & ",";

      Buffer := Buffer & ASCII.LF & Indent (N => 4) & "Handover       => "
        & (if Event.Handover then "True" else "False") & ",";

      Buffer := Buffer & ASCII.LF & Indent (N => 4) & "Send_IPI       => "
        & (if Event.Send_IPI then "True" else "False");

      Buffer := Buffer & "," & ASCII.LF
        & Indent (N => 4) & "IRQ_Number     =>" & Event.IRQ_Number'Img & ")";
   end Add_Source_Event;

   -------------------------------------------------------------------------

   function To_Source_Event
     (Event           : DOM.Core.Node;
      Physical_Events : DOM.Core.Node_List;
      Target_Events   : DOM.Core.Node_List)
      return Source_Event_Type
   is
      use type DOM.Core.Node;

      Phys_Event_Ref : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Event,
           Name => "physical");
      Notify_Mode : constant String
        := Muxml.Utils.Get_Attribute
          (Nodes     => Physical_Events,
           Ref_Attr  => "name",
           Ref_Value => Phys_Event_Ref,
           Attr_Name => "mode");
      Src_Action : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Event,
           XPath => "*");
      Src_Action_Kind_Str : constant String
        := (if Src_Action /= null then Mutools.Utils.To_Ada_Identifier
            (Str => DOM.Core.Nodes.Node_Name (N => Src_Action))
            else "No_Action");
      Event_Target : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Nodes     => Target_Events,
           Ref_Attr  => "physical",
           Ref_Value => Phys_Event_Ref);
      Target_Subj_ID : constant Natural
        := (if Event_Target = null then Invalid_Target_Subject
            else Natural'Value (DOM.Core.Elements.Get_Attribute
              (Elem => Muxml.Utils.Ancestor_Node
               (Node  => Event_Target,
                Level => 3),
               Name => "globalId")));
      Has_Target_Action : constant Boolean
        := (if Event_Target = null then False
            else Muxml.Utils.Get_Element
              (Doc   => Event_Target,
               XPath => "*") /= null);
      Target_Event_ID : constant Natural
        := (if Event_Target = null or
              (Notify_Mode = "switch" and not Has_Target_Action)
            then Invalid_Target_Event
            else Natural'Value (DOM.Core.Elements.Get_Attribute
              (Elem => Event_Target,
               Name => "id")));
      IRQ_Num : constant Natural
        := (if Src_Action_Kind_Str = "Unmask_Irq" then
               Natural'Value (DOM.Core.Elements.Get_Attribute
              (Elem => Src_Action,
               Name => "number"))
            else 0);
   begin
      return Source_Event_Type'
        (Source_Action  => Mutools.Types.Event_Action_Kind'Value
           (Src_Action_Kind_Str),
         Target_Subject => Target_Subj_ID,
         Target_Event   => Target_Event_ID,
         Handover       => Notify_Mode = "switch",
         Send_IPI       => Notify_Mode = "ipi",
         IRQ_Number     => IRQ_Num);
   end To_Source_Event;

   -------------------------------------------------------------------------

   function To_Source_Event_Map
     (Events          : DOM.Core.Node_List;
      Event_Group     : Mutools.Types.Event_Group_Type;
      Physical_Events : DOM.Core.Node_List;
      Target_Events   : DOM.Core.Node_List)
      return SEM.Map
   is
      Event_Map : SEM.Map;
   begin
      declare
         ID_Set : SON.Set;
      begin

         --  Add reserved events.

         for I in 0 .. Mutools.Types.Get_Max_ID (Group => Event_Group) loop
            if not Mutools.Types.Is_Valid_Event_ID (Group => Event_Group,
                                                    ID    => I)
            then
               ID_Set.Insert (New_Item => I);
            end if;
         end loop;
         if not ID_Set.Is_Empty then
            Event_Map.Insert (Key      => Invalid_Event,
                              New_Item => ID_Set);
         end if;
      end;

      for I in 0 .. DOM.Core.Nodes.Length (List => Events) - 1 loop
         declare
            use type SEM.Cursor;

            Event : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Events,
                                      Index => I);
            ID : constant Natural := Natural'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Event,
                  Name => "id"));
            Src_Evt : constant Source_Event_Type
              := To_Source_Event (Event           => Event,
                                  Physical_Events => Physical_Events,
                                  Target_Events   => Target_Events);
            Cursor : constant SEM.Cursor := Event_Map.Find (Key => Src_Evt);

            --  Append current event ID to list.
            procedure Append_ID
              (Key     :        Source_Event_Type;
               Element : in out SON.Set);

            -------------------------------------------------------------

            procedure Append_ID
              (Key     :        Source_Event_Type;
               Element : in out SON.Set)
            is
               pragma Unreferenced (Key);
            begin
               Element.Insert (New_Item => ID);
            end Append_ID;
         begin
            if Cursor = SEM.No_Element then
               declare
                  ID_Set : SON.Set;
               begin
                  ID_Set.Insert (New_Item => ID);
                  Event_Map.Insert (Key      => Src_Evt,
                                    New_Item => ID_Set);
               end;
            else
               Event_Map.Update_Element (Position => Cursor,
                                         Process  => Append_ID'Access);
            end if;
         end;
      end loop;
      return Event_Map;
   end To_Source_Event_Map;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;

      Max_Event_Count : constant Natural
        := 2 ** Mutools.Constants.Event_Bits;
      Event_Bits_Str : constant String
        := Ada.Strings.Fixed.Trim
          (Source => Mutools.Constants.Event_Bits'Img,
           Side   => Ada.Strings.Left);
      Subjects       : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Subj_Count     : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);
      Phys_Events    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/events/event");
      Target_Events  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject/events/target/event");

      Ev_Buf : Unbounded_String;
      Buffer : Unbounded_String;
      Tmpl   : Mutools.Templates.Template_Type;

      --  Add source event table for given events to specified template buffer.
      procedure Add_Source_Events
        (Events :        DOM.Core.Node_List;
         Buffer : in out Unbounded_String);

      --  Add trap table for given traps to specified template buffer.
      procedure Add_Traps
        (Traps  :        DOM.Core.Node_List;
         Buffer : in out Unbounded_String);

      --  Add event action entry to template buffer.
      procedure Add_Event_Action_Entry
        (Event :     DOM.Core.Node;
         Added : out Boolean);

      --  Append SPARK events spec of given subject to template buffer.
      procedure Write_Subject_Event_Spec (Subject : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Add_Event_Action_Entry
        (Event :     DOM.Core.Node;
         Added : out Boolean)
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
         Vector_Str : constant String :=
           (if Action_Kind = "inject_interrupt" then
               DOM.Core.Elements.Get_Attribute
              (Elem => Action,
               Name => "vector")
            else "Invalid_Vector");
      begin
         if not (Action_Kind = "No_Action" and Vector_Str = "Invalid_Vector")
         then
            Ev_Buf := Ev_Buf & Indent (N => 3)  & " "
              & Event_ID & " => ";
            Ev_Buf := Ev_Buf & "Target_Event_Type'(" & ASCII.LF
              & Indent (N => 4) & "Kind   => "
              & Mutools.Utils.To_Ada_Identifier (Str => Action_Kind) & ",";
            Ev_Buf := Ev_Buf & ASCII.LF & Indent (N => 4)
              & "Vector => " & Vector_Str & ")";
            Added := True;
         else
            Added := False;
         end if;
      end Add_Event_Action_Entry;

      ----------------------------------------------------------------------

      procedure Add_Source_Events
        (Events :        DOM.Core.Node_List;
         Buffer : in out Unbounded_String)
      is
         Src_Evts_Map : constant SEM.Map
           := To_Source_Event_Map (Events          => Events,
                                   Event_Group     => Mutools.Types.Vmcall,
                                   Physical_Events => Phys_Events,
                                   Target_Events   => Target_Events);
         Src_Ev_Count : Natural := 0;

         --  Add given source event entry to buffer.
         procedure Add_Entry (Cursor : SEM.Cursor);

         -------------------------------------------------------------------

         procedure Add_Entry (Cursor : SEM.Cursor)
         is
            use type SEM.Cursor;

            Event  : constant Source_Event_Type
              := SEM.Key (Position => Cursor);
            IDs    : constant SON.Set := SEM.Element (Position => Cursor);
            Cur_ID : Natural := 0;
         begin
            if Cursor /= Src_Evts_Map.First then
               Buffer := Buffer & ",";
            end if;

            for ID of IDs loop
               if Cur_ID = 0 then
                  Buffer := Buffer & ASCII.LF & Indent (N => 3);
               end if;
               if Cur_ID = 5 then
                  Cur_ID := 0;
               else
                  Cur_ID := Cur_ID + 1;
               end if;

               --  Pad single digit IDs for alignment.

               Buffer := Buffer & (if ID'Img'Length = 2 then " " else "")
                 & ID'Img;

               if ID /= IDs.Last_Element then
                  Buffer := Buffer & " |";
               end if;
            end loop;
            Buffer := Buffer & " => ";
            Add_Source_Event (Event  => Event,
                              Buffer => Buffer);
            Src_Ev_Count := Src_Ev_Count + 1;
         end Add_Entry;
      begin
         Buffer := Buffer & "Source_Event_Table_Type'(";
         Src_Evts_Map.Iterate (Process => Add_Entry'Access);
         if Src_Ev_Count /= Max_Event_Count then
            Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
              & " others => Null_Source_Event";
         end if;
         Buffer := Buffer & "),";
      end Add_Source_Events;

      ----------------------------------------------------------------------

      procedure Add_Traps
        (Traps  :        DOM.Core.Node_List;
         Buffer : in out Unbounded_String)
      is
         use type Ada.Containers.Count_Type;

         Trap_Map : constant SEM.Map
           := To_Source_Event_Map (Events          => Traps,
                                   Event_Group     => Mutools.Types.Vmx_Exit,
                                   Physical_Events => Phys_Events,
                                   Target_Events   => Target_Events);

         --  Add given source event entry to buffer.
         procedure Add_Entry (Cursor : SEM.Cursor);

         -------------------------------------------------------------------

         procedure Add_Entry (Cursor : SEM.Cursor)
         is
            use type SEM.Cursor;

            Event  : constant Source_Event_Type
              := SEM.Key (Position => Cursor);
            IDs    : constant SON.Set := SEM.Element (Position => Cursor);
            Cur_ID : Natural := 0;
         begin
            if Cursor /= Trap_Map.First then
               Buffer := Buffer & ",";
            end if;

            for ID of IDs loop
               if Cur_ID = 0 then
                  Buffer := Buffer & ASCII.LF & Indent (N => 3);
               end if;
               if Cur_ID = 5 then
                  Cur_ID := 0;
               else
                  Cur_ID := Cur_ID + 1;
               end if;

               --  Pad single digit IDs for alignment.

               Buffer := Buffer & (if ID'Img'Length = 2 then " " else "")
                 & ID'Img;

               if ID /= IDs.Last_Element then
                  Buffer := Buffer & " |";
               end if;
            end loop;
            Buffer := Buffer & " => ";
            Add_Source_Event (Event  => Event,
                              Buffer => Buffer);
         end Add_Entry;
      begin
         Buffer := Buffer & "Trap_Table_Type'(";
         if Trap_Map.Length = 1 then
            Buffer := Buffer & ASCII.LF & Indent (N => 3) & "others => ";
            Add_Source_Event (Event  => SEM.Key (Position => Trap_Map.First),
                              Buffer => Buffer);
         else
            Trap_Map.Iterate (Process => Add_Entry'Access);
         end if;
         Buffer := Buffer & "),";
      end Add_Traps;

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
         Target_Added : Boolean;
         Skip_Count : Natural := 0;
      begin
         Ev_Buf := To_Unbounded_String (Indent (N => 2)) & Subj_ID
           & " => Subject_Events_Type'(" & ASCII.LF
           & Indent & "    Source_Traps  => ";

         Add_Traps (Traps  => Traps,
                    Buffer => Ev_Buf);

         Ev_Buf := Ev_Buf & ASCII.LF
           & Indent & "    Source_Events => ";

         if Src_Ev_Count = 0 then
            Ev_Buf := Ev_Buf & "Null_Source_Event_Table,";
         else
            Add_Source_Events (Events => Src_Events,
                               Buffer => Ev_Buf);
         end if;

         Ev_Buf := Ev_Buf & ASCII.LF
           & Indent & "    Target_Events => ";

         if Target_Ev_Count = 0 then
            Ev_Buf := Ev_Buf & "Null_Target_Event_Table)";
         else
            Ev_Buf := Ev_Buf & "Target_Event_Table_Type'(" & ASCII.LF;
            for I in 0 .. Target_Ev_Count - 1 loop
               Add_Event_Action_Entry (Event => DOM.Core.Nodes.Item
                                       (List  => Target_Events,
                                        Index => I),
                                       Added => Target_Added);
               if Target_Added and
                 (I < Target_Ev_Count - 1 or else Skip_Count > 0)
               then
                  Ev_Buf := Ev_Buf & "," & ASCII.LF;
               end if;
               if not Target_Added then
                  Skip_Count := Skip_Count + 1;
               end if;
            end loop;

            if Target_Ev_Count /= Max_Event_Count then
               if Skip_Count = 0 then
                  Ev_Buf := Ev_Buf & "," & ASCII.LF;
               end if;

               Ev_Buf := Ev_Buf & Indent (N => 3)
                 & " others => Null_Target_Event";
            end if;
            Ev_Buf := Ev_Buf & "))";
         end if;
      end Write_Subject_Event_Spec;
   begin
      Mulog.Log (Msg => "Writing event spec to '"
                 & Output_Dir & "/skp-events.adb'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_events_ads);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__event_bits__",
         Content  => Event_Bits_Str);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__event_kind_types__",
         Content  => Mutools.Utils.Get_Event_Kind_Types_String);
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
            Ev_Buf := Ev_Buf & "," & ASCII.LF;
         end if;
         Buffer := Buffer & Ev_Buf;
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
