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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents.Local;

with McKae.XML.XPath.XIA;

with Mulog;
with Mucfgvcpu;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;

with Expanders.Types;

package body Expanders.Components
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   procedure Add_Channel_Arrays (Data : in out Muxml.XML_Data_Type)
   is
      Channel_Arrays : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/*/requires/channels/array");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Channel_Arrays) - 1 loop
         declare
            Arr_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Channel_Arrays,
                 Index => I);
            Arr_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Arr_Node,
                 Name => "logical");
            Parent_Chan : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Arr_Node);
            Comp_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Muxml.Utils.Ancestor_Node
                   (Node  => Parent_Chan,
                    Level => 2),
                 Name => "name");
            Address : Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "virtualAddressBase"));
            Element_Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "elementSize"));
            Event_Base_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Arr_Node,
                 Name => "eventBase");
            Vector_Base_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Arr_Node,
                 Name => "vectorBase");
            Has_Event_Base : constant Boolean
              := Event_Base_Str'Length > 0;
            Has_Vector_Base : constant Boolean
              := Vector_Base_Str'Length > 0;
            Event_Base : Natural
              := (if Has_Event_Base
                  then Natural'Value (Event_Base_Str)
                  else 0);
            Vector_Base : Natural
              := (if Has_Vector_Base
                  then Natural'Value (Vector_Base_Str)
                  else 0);
            Children : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Arr_Node,
                 XPath => "*[self::reader or self::writer]");
            Child_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Children);
         begin
            Mulog.Log (Msg => "Adding" & Child_Count'Img & " channel(s) "
                       & "of array '" & Arr_Name & "' to component '"
                       & Comp_Name & "'");
            for J in 0 .. Child_Count - 1 loop
               declare
                  use type Interfaces.Unsigned_64;

                  Chan_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Children,
                       Index => J);
                  New_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Clone_Node
                      (N    => Chan_Node,
                       Deep => False);
               begin
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => New_Node,
                     Name  => "virtualAddress",
                     Value => Mutools.Utils.To_Hex (Number => Address));
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => New_Node,
                     Name  => "size",
                     Value => Mutools.Utils.To_Hex (Number => Element_Size));
                  if Has_Vector_Base then
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => New_Node,
                        Name  => "vector",
                        Value => Ada.Strings.Fixed.Trim
                          (Source => Vector_Base'Img,
                           Side   => Ada.Strings.Left));
                     Vector_Base := Vector_Base + 1;
                  end if;
                  if Has_Event_Base then
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => New_Node,
                        Name  => "event",
                        Value => Ada.Strings.Fixed.Trim
                          (Source => Event_Base'Img,
                           Side   => Ada.Strings.Left));
                     Event_Base := Event_Base + 1;
                  end if;
                  Muxml.Utils.Append_Child
                    (Node      => Parent_Chan,
                     New_Child => New_Node);
                  Address := Address + Element_Size;
               end;
            end loop;
         end;
      end loop;
   end Add_Channel_Arrays;

   -------------------------------------------------------------------------

   procedure Add_Channels (Data : in out Muxml.XML_Data_Type)
   is
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0' and component]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Subj_Channel_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "channels");

            Comp_Ref_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "component");
            Comp_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Ref_Node,
                 Name => "ref");
            Mappings : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Ref_Node,
                 XPath => "map");
            Comp_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Ref);
            Comp_Channels : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "requires/channels/*[self::reader or self::writer]");
            Log_Channel_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Comp_Channels);
         begin
            if Log_Channel_Count > 0 then
               Mulog.Log (Msg => "Expanding" & Log_Channel_Count'Img
                          & " logical channel(s) of component '" & Comp_Ref
                          & "' to subject '" & Subj_Name & "'");

               for J in 0 .. Log_Channel_Count - 1 loop
                  declare
                     Logical_Channel : constant DOM.Core.Node
                       := DOM.Core.Nodes.Clone_Node
                         (N    => DOM.Core.Nodes.Item
                            (List  => Comp_Channels,
                             Index => J),
                          Deep => False);
                     Logical_Channel_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Logical_Channel,
                          Name => "logical");
                     Physical_Channel_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Muxml.Utils.Get_Element
                            (Nodes     => Mappings,
                             Ref_Attr  => "logical",
                             Ref_Value => Logical_Channel_Name),
                          Name => "physical");
                  begin
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Logical_Channel,
                        Name  => "physical",
                        Value => Physical_Channel_Name);
                     Muxml.Utils.Append_Child
                       (Node      => Subj_Channel_Node,
                        New_Child => Logical_Channel);
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Add_Channels;

   -------------------------------------------------------------------------

   procedure Add_Devices (Data : in out Muxml.XML_Data_Type)
   is

      --  Add given component device resources to specified parent node using
      --  given mappings.
      procedure Add_Component_Device_Resource_Mapping
        (Parent_Node         : DOM.Core.Node;
         Component_Resources : DOM.Core.Node_List;
         Mappings            : DOM.Core.Node_List);

      ----------------------------------------------------------------------

      procedure Add_Component_Device_Resource_Mapping
        (Parent_Node         : DOM.Core.Node;
         Component_Resources : DOM.Core.Node_List;
         Mappings            : DOM.Core.Node_List)
      is
         Res_Count : constant Natural
           := DOM.Core.Nodes.Length (List => Component_Resources);
      begin
         for I in 0 .. Res_Count - 1 loop
            declare
               Res_Node : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Component_Resources,
                    Index => I);
               Log_Res : constant DOM.Core.Node
                 := DOM.Core.Nodes.Clone_Node (N    => Res_Node,
                                               Deep => False);
               Log_Res_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Log_Res,
                    Name => "logical");
               Res_Mapping : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Mappings,
                    Ref_Attr  => "logical",
                    Ref_Value => Log_Res_Name);
               Phys_Res_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Res_Mapping,
                    Name => "physical");
               Res_Children : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Res_Node,
                    XPath => "*");
               Res_Mapping_Children : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Res_Mapping,
                    XPath => "map");
               Node_Name : constant String
                 := DOM.Core.Nodes.Node_Name (N => Log_Res);
            begin

               --  Recursively add resource mappings.

               Add_Component_Device_Resource_Mapping
                 (Parent_Node         => Log_Res,
                  Component_Resources => Res_Children,
                  Mappings            => Res_Mapping_Children);

               DOM.Core.Elements.Set_Attribute
                 (Elem  => Log_Res,
                  Name  => "physical",
                  Value => Phys_Res_Name);

               if Node_Name = "memory" then
                  DOM.Core.Elements.Remove_Attribute
                    (Elem => Log_Res,
                     Name => "size");
               elsif Node_Name = "ioPort" then
                  DOM.Core.Elements.Remove_Attribute
                    (Elem => Log_Res,
                     Name => "start");
                  DOM.Core.Elements.Remove_Attribute
                    (Elem => Log_Res,
                     Name => "end");
               end if;

               Muxml.Utils.Append_Child
                 (Node      => Parent_Node,
                  New_Child => Log_Res);
            end;
         end loop;
      end Add_Component_Device_Resource_Mapping;

      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0' and component]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Subj_Dev_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "devices");
            Comp_Ref_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "component");
            Comp_Ref      : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Ref_Node,
                 Name => "ref");
            Mappings      : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Ref_Node,
                 XPath => "map");
            Comp_Node     : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Ref);
            Comp_Devices  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "requires/devices/device");
            Log_Dev_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Comp_Devices);
         begin
            if Log_Dev_Count > 0 then
               Mulog.Log (Msg => "Expanding" & Log_Dev_Count'Img & " logical "
                          & "device(s) of component '" & Comp_Ref
                          & "' to subject '" & Subj_Name & "'");

               Add_Component_Device_Resource_Mapping
                 (Parent_Node         => Subj_Dev_Node,
                  Component_Resources => Comp_Devices,
                  Mappings            => Mappings);
            end if;
         end;
      end loop;
   end Add_Devices;

   -------------------------------------------------------------------------

   procedure Add_Events (Data : in out Muxml.XML_Data_Type)
   is
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0' and component]");

      --  Add given mappings of component events to subject events node.
      procedure Add_Mapped_Events
        (Subject_Node   : DOM.Core.Node;
         Component_Node : DOM.Core.Node;
         Mappings       : DOM.Core.Node_List;
         Event_Kind     : String)
      with Pre => Event_Kind = "source" or Event_Kind = "target";

      ----------------------------------------------------------------------

      procedure Add_Mapped_Events
        (Subject_Node   : DOM.Core.Node;
         Component_Node : DOM.Core.Node;
         Mappings       : DOM.Core.Node_List;
         Event_Kind     : String)
      is
         use type DOM.Core.Node;

         Subj_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Subject_Node,
              Name => "name");
         Subject_Event_XPath : constant String := "events/" & Event_Kind
           & (if Event_Kind = "source" then "/group[@name='vmcall']"
              else "");
         Subj_Event_Node : DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Subject_Node,
              XPath => Subject_Event_XPath);
         Comp_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Component_Node,
              Name => "name");
         Comp_Events_XPath : constant String := "requires/events/" & Event_Kind
           & "/event";
         Comp_Events : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Component_Node,
              XPath => Comp_Events_XPath);
         Log_Ev_Count : constant Natural
           := DOM.Core.Nodes.Length (List => Comp_Events);

         --  Create missing subject XML elements for inserting source/target
         --  events.
         procedure Create_Missing_Element;

         -------------------------------------------------------------------

         procedure Create_Missing_Element
         is
            Events_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subject_Node,
                 XPath => "events");
         begin
            if Event_Kind = "source" then
               Subj_Event_Node := Muxml.Utils.Get_Element
                 (Doc   => Events_Node,
                  XPath => Event_Kind);
               if Subj_Event_Node = null then
                  Subj_Event_Node := DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => Event_Kind);
                  Muxml.Utils.Insert_Before
                    (Parent    => Events_Node,
                     New_Child => Subj_Event_Node,
                     Ref_Names => (1 => U ("target")));
               end if;

               declare
                  Parent_Node : constant DOM.Core.Node := Subj_Event_Node;
               begin
                  Subj_Event_Node := DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "group");
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => Subj_Event_Node,
                     Name  => "name",
                     Value => "vmcall");
                  Muxml.Utils.Append_Child
                    (Node      => Parent_Node,
                     New_Child => Subj_Event_Node);
               end;
            else
               Subj_Event_Node := DOM.Core.Nodes.Append_Child
                 (N         => Events_Node,
                  New_Child => DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => Event_Kind));
            end if;
         end Create_Missing_Element;
      begin
         if Log_Ev_Count > 0 then
            Mulog.Log (Msg => "Expanding" & Log_Ev_Count'Img & " logical "
                       & Event_Kind & " event(s) of component '" & Comp_Name
                       & "' to subject '" & Subj_Name & "'");

            if Subj_Event_Node = null then
               Create_Missing_Element;
            end if;

            for I in 0 .. Log_Ev_Count - 1 loop
               declare
                  Log_Ev : constant DOM.Core.Node
                    := DOM.Core.Nodes.Clone_Node
                      (N    => DOM.Core.Nodes.Item
                         (List  => Comp_Events,
                          Index => I),
                       Deep => True);
                  Log_Ev_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Log_Ev,
                       Name => "logical");
                  Phys_Ev_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Muxml.Utils.Get_Element
                         (Nodes     => Mappings,
                          Ref_Attr  => "logical",
                          Ref_Value => Log_Ev_Name),
                       Name => "physical");
               begin
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => Log_Ev,
                     Name  => "physical",
                     Value => Phys_Ev_Name);
                  Muxml.Utils.Append_Child
                    (Node      => Subj_Event_Node,
                     New_Child => Log_Ev);
               end;
            end loop;
         end if;
      end Add_Mapped_Events;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Comp_Ref_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "component");
            Comp_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Ref_Node,
                 Name => "ref");
            Mappings : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Ref_Node,
                 XPath => "map");
            Comp_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Ref);
         begin
            Add_Mapped_Events
              (Subject_Node   => Subj_Node,
               Component_Node => Comp_Node,
               Mappings       => Mappings,
               Event_Kind     => "source");
            Add_Mapped_Events
              (Subject_Node   => Subj_Node,
               Component_Node => Comp_Node,
               Mappings       => Mappings,
               Event_Kind     => "target");
         end;
      end loop;
   end Add_Events;

   -------------------------------------------------------------------------

   procedure Add_Library_Resources (Data : in out Muxml.XML_Data_Type)
   is
      Libraries : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/library");

      package Deps_Map is new Ada.Containers.Hashed_Maps
        (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
         Element_Type    => DOM.Core.Node,
         Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
         Equivalent_Keys => Ada.Strings.Unbounded.Equal_Case_Insensitive,
         "="             => DOM.Core."=");

      --  Merge all child nodes of right into child nodes of left. If no child
      --  with given tag name exists in left, it is created. Child nodes are
      --  assumed to be sequences.
      procedure Merge_Childs (Left, Right : DOM.Core.Node);

      --  Recursively resolve dependencies of given component/library node by
      --  adding their names to the Resolved map.
      procedure Resolve_Depends
        (Node     :        DOM.Core.Node;
         Resolved : in out Deps_Map.Map);

      ----------------------------------------------------------------------

      procedure Merge_Childs (Left, Right : DOM.Core.Node)
      is
         use Ada.Strings.Unbounded;

         type Node_Type is
           (Config,
            Depends,
            Requires);

         subtype Child_Range is Positive range 1 .. 3;

         Ref_Children : constant Muxml.Utils.Tags_Type (Child_Range)
           := (1 => To_Unbounded_String ("config"),
               2 => To_Unbounded_String ("requires"),
               3 => To_Unbounded_String ("provides"));

         First_Child_Index : constant array (Node_Type) of Child_Range
           := (Config   => 1,
               Depends  => 2,
               Requires => 3);

         R_Childs : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Right,
              XPath => "*");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => R_Childs) - 1 loop
            declare
               use type DOM.Core.Node;

               R_Child : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => R_Childs,
                    Index => I);
               Child_Tag : constant String
                 := DOM.Core.Elements.Get_Tag_Name (Elem => R_Child);
               L_Child : DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => Left,
                    XPath => Child_Tag);
            begin
               if L_Child = null then
                  Muxml.Utils.Add_Child
                    (Parent     => Left,
                     Child_Name => Child_Tag,
                     Ref_Names  => Ref_Children (First_Child_Index
                       (Node_Type'Value (Child_Tag)) .. Ref_Children'Last));
                  L_Child := Muxml.Utils.Get_Element
                    (Doc   => Left,
                     XPath => Child_Tag);

                  if L_Child = null then

                     --  Append child if no reference child was present.

                     Muxml.Utils.Add_Child
                       (Parent     => Left,
                        Child_Name => Child_Tag);
                     L_Child := Muxml.Utils.Get_Element
                       (Doc   => Left,
                        XPath => Child_Tag);
                  end if;
               end if;
               Muxml.Utils.Merge
                 (Left      => L_Child,
                  Right     => R_Child,
                  List_Tags => (1 => To_Unbounded_String ("memory"),
                                2 => To_Unbounded_String ("reader"),
                                3 => To_Unbounded_String ("writer"),
                                4 => To_Unbounded_String ("device"),
                                5 => To_Unbounded_String ("array"),
                                6 => To_Unbounded_String ("library"),
                                7 => To_Unbounded_String ("event")));
            end;
         end loop;
      end Merge_Childs;

      ----------------------------------------------------------------------

      procedure Resolve_Depends
        (Node     :        DOM.Core.Node;
         Resolved : in out Deps_Map.Map)
      is
         use type DOM.Core.Node;

         Deps_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element (Doc   => Node,
                                       XPath => "depends");
         Deps : DOM.Core.Node_List;
      begin
         if Deps_Node = null then
            return;
         end if;

         Deps := McKae.XML.XPath.XIA.XPath_Query
             (N     => Deps_Node,
              XPath => "library");

         for I in 0 .. DOM.Core.Nodes.Length (List => Deps) - 1 loop
            declare
               Cur_Dep  : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Deps,
                                         Index => I);
               Dep_Name : constant String
                 := DOM.Core.Elements.Get_Attribute (Elem => Cur_Dep,
                                                     Name => "ref");
               Dep_Node : DOM.Core.Node;
            begin
               if not Resolved.Contains (Key => U (Dep_Name)) then
                  Dep_Node := Muxml.Utils.Get_Element
                    (Nodes     => Libraries,
                     Ref_Attr  => "name",
                     Ref_Value => Dep_Name);
                  Resolved.Insert (Key      => U (Dep_Name),
                                   New_Item => Dep_Node);
                  Resolve_Depends
                    (Node     => Dep_Node,
                     Resolved => Resolved);
               end if;
            end;
         end loop;
      end Resolve_Depends;

      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/component[depends/library]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Components) - 1 loop
         declare
            Comp_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Components,
                 Index => I);
            Comp_Name : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Comp_Node,
                                                  Name => "name");
            Resolved_Libs : Deps_Map.Map;
            Cur_Lib : Deps_Map.Cursor;
         begin
            Resolve_Depends (Node     => Comp_Node,
                             Resolved => Resolved_Libs);

            Cur_Lib := Resolved_Libs.First;
            while Deps_Map.Has_Element (Position => Cur_Lib) loop
               declare
                  Lib_Name : constant String
                    := Ada.Strings.Unbounded.To_String
                      (Deps_Map.Key (Position => Cur_Lib));
                  Lib_Node : constant DOM.Core.Node
                    := Deps_Map.Element (Position => Cur_Lib);
               begin
                  Mulog.Log (Msg => "Adding library '" & Lib_Name
                             & "' resources to component '" & Comp_Name & "'");
                  Merge_Childs
                    (Left  => Comp_Node,
                     Right => Lib_Node);
                  Deps_Map.Next (Position => Cur_Lib);
               end;
            end loop;
         end;
      end loop;
   end Add_Library_Resources;

   -------------------------------------------------------------------------

   procedure Add_Memory (Data : in out Muxml.XML_Data_Type)
   is
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0' and component]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Subj_Mem_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "memory");

            Comp_Ref_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "component");
            Comp_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Ref_Node,
                 Name => "ref");
            Mappings : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Ref_Node,
                 XPath => "map");
            Comp_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Ref);
            Comp_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "requires/memory/memory");
            Log_Mem_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Comp_Memory);
         begin
            if Log_Mem_Count > 0 then
               Mulog.Log (Msg => "Expanding" & Log_Mem_Count'Img & " logical "
                          & "memory region(s) of component '" & Comp_Ref
                          & "' to subject '" & Subj_Name & "'");

               for J in 0 .. Log_Mem_Count - 1 loop
                  declare
                     Log_Mem : constant DOM.Core.Node
                       := DOM.Core.Nodes.Clone_Node
                         (N    => DOM.Core.Nodes.Item
                            (List  => Comp_Memory,
                             Index => J),
                          Deep => False);
                     Log_Mem_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Mem,
                          Name => "logical");
                     Phys_Mem_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Muxml.Utils.Get_Element
                            (Nodes     => Mappings,
                             Ref_Attr  => "logical",
                             Ref_Value => Log_Mem_Name),
                          Name => "physical");
                  begin
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Log_Mem,
                        Name  => "physical",
                        Value => Phys_Mem_Name);
                     DOM.Core.Elements.Remove_Attribute
                       (Elem => Log_Mem,
                        Name => "size");
                     Muxml.Utils.Append_Child
                       (Node      => Subj_Mem_Node,
                        New_Child => Log_Mem);
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Add_Memory;

   -------------------------------------------------------------------------

   procedure Add_Memory_Arrays (Data : in out Muxml.XML_Data_Type)
   is
      Memarrays : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/*/requires/memory/array");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Memarrays) - 1 loop
         declare
            Arr_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Memarrays,
                 Index => I);
            Arr_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Arr_Node,
                 Name => "logical");
            Parent_Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Arr_Node);
            Comp_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Muxml.Utils.Ancestor_Node
                   (Node  => Arr_Node,
                    Level => 2),
                 Name => "name");
            Address : Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "virtualAddressBase"));
            Element_Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "elementSize"));
            Executable : constant Boolean
              := Boolean'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "executable"));
            Writable : constant Boolean
              := Boolean'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "writable"));
            Children : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Arr_Node,
                 XPath => "*");
            Child_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Children);
         begin
            Mulog.Log (Msg => "Adding" & Child_Count'Img & " memory region(s) "
                       & "of array '" & Arr_Name & "' to component '"
                       & Comp_Name & "'");
            for J in 0 .. Child_Count - 1 loop
               declare
                  use type Interfaces.Unsigned_64;

                  package XML renames Mutools.XML_Utils;

                  Mem_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Children,
                       Index => J);
                  Mem_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Mem_Node,
                       Name => "logical");
               begin
                  Muxml.Utils.Append_Child
                    (Node      => Parent_Mem,
                     New_Child => XML.Create_Component_Memory_Node
                       (Policy       => Data,
                        Logical_Name => Mem_Name,
                        Address      => Mutools.Utils.To_Hex
                          (Number => Address),
                        Size         => Mutools.Utils.To_Hex
                          (Number => Element_Size),
                        Executable   => Executable,
                        Writable     => Writable));
                  Address := Address + Element_Size;
               end;
            end loop;
         end;
      end loop;
   end Add_Memory_Arrays;

   -------------------------------------------------------------------------

   procedure Add_Provided_Memory (Data : in out Muxml.XML_Data_Type)
   is
      Physical_Mem : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/memory");
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/component");
      Subjects_Refs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0']/component");

      --  Add given component memory node as physical memory region and mapping
      --  to each subject in the specified component references list.
      procedure Add_Component_Memory
        (Memory    : DOM.Core.Node;
         Comp_Refs : DOM.Core.Node_List);

      ----------------------------------------------------------------------

      procedure Add_Component_Memory
        (Memory    : DOM.Core.Node;
         Comp_Refs : DOM.Core.Node_List)
      is
         Mem_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Memory,
              Name => "logical");
         Is_Executable : constant Boolean
           := Boolean'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Memory,
                 Name => "executable"));
         Is_Writable : constant Boolean
           := Boolean'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Memory,
                 Name => "writable"));
         Virtual_Address : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Memory,
              Name => "virtualAddress");
         Size : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Memory,
              Name => "size");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Comp_Refs) - 1 loop
            declare
               Subj_Node     : constant DOM.Core.Node
                 := DOM.Core.Nodes.Parent_Node
                   (N => DOM.Core.Nodes.Item
                      (List  => Comp_Refs,
                       Index => I));
               Subj_Name     : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Subj_Node,
                    Name => "name");
               Subj_Mem_Node : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => Subj_Node,
                    XPath => "memory");
               Phys_Mem      : constant DOM.Core.Node
                 := DOM.Core.Documents.Local.Clone_Node
                   (N    => Memory,
                    Deep => True);
               Phys_Name     : constant String := Subj_Name & "|" & Mem_Name;
            begin
               Mulog.Log (Msg => "Mapping component memory '" & Mem_Name
                          & "' with size " & Size
                          & " at virtual address " & Virtual_Address
                          & " of subject '" & Subj_Name & "'");

               DOM.Core.Elements.Remove_Attribute
                 (Elem => Phys_Mem,
                  Name => "virtualAddress");
               DOM.Core.Elements.Remove_Attribute
                 (Elem => Phys_Mem,
                  Name => "executable");
               DOM.Core.Elements.Remove_Attribute
                 (Elem => Phys_Mem,
                  Name => "logical");
               DOM.Core.Elements.Remove_Attribute
                 (Elem => Phys_Mem,
                  Name => "writable");
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Phys_Mem,
                  Name  => "caching",
                  Value => "WB");
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Phys_Mem,
                  Name  => "name",
                  Value => Phys_Name);

               Muxml.Utils.Append_Child
                 (Node      => Physical_Mem,
                  New_Child => Phys_Mem);
               Muxml.Utils.Append_Child
                 (Node      => Subj_Mem_Node,
                  New_Child =>
                    Mutools.XML_Utils.Create_Virtual_Memory_Node
                      (Policy        => Data,
                       Logical_Name  => Mem_Name,
                       Physical_Name => Phys_Name,
                       Address       => Virtual_Address,
                       Writable      => Is_Writable,
                       Executable    => Is_Executable));
            end;
         end loop;
      end Add_Component_Memory;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Components) - 1 loop
         declare
            Comp_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Components,
                 Index => I);
            Comp_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Node,
                 Name => "name");
            Comp_Mem : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "provides/memory");
            Comp_Ref_Nodes : constant DOM.Core.Node_List
              := Muxml.Utils.Get_Elements
                (Nodes     => Subjects_Refs,
                 Ref_Attr  => "ref",
                 Ref_Value => Comp_Name);
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Comp_Mem) - 1 loop
               Add_Component_Memory (Memory    => DOM.Core.Nodes.Item
                                     (List  => Comp_Mem,
                                      Index => J),
                                     Comp_Refs => Comp_Ref_Nodes);
            end loop;
         end;
      end loop;
   end Add_Provided_Memory;

   -------------------------------------------------------------------------

   procedure Add_Subject_Profile_VCPU (Data : in out Muxml.XML_Data_Type)
   is
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0'and component]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            use type DOM.Core.Node;

            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Comp_Ref_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "component");
            Comp_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Ref_Node,
                 Name => "ref");
            Comp_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Ref);
            Profile_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Node,
                 Name => "profile");
            Profile : constant Types.Subject_Profile_Type
              := Types.Subject_Profile_Type'Value (Profile_Str);
            Comp_VCPU_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Comp_Node,
                 XPath => "requires/vcpu");
            Subj_VCPU_Node : DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "vcpu");
         begin
            Mulog.Log (Msg => "Setting profile of subject '" & Subj_Name
                       & "' to " & Profile'Img & " (VCPU profile "
                       & Types.Subj_VCPU_Profile_Map (Profile)'Img & ")");
            DOM.Core.Elements.Set_Attribute
              (Elem  => Subj_Node,
               Name  => "profile",
               Value => Profile_Str);

            if Comp_VCPU_Node = null then
               Mucfgvcpu.Set_VCPU_Profile
                 (Profile => Types.Subj_VCPU_Profile_Map (Profile),
                  Node    => Subj_VCPU_Node);
            else
               declare
                  VCPU_Node : DOM.Core.Node
                    := DOM.Core.Nodes.Clone_Node
                      (N    => Comp_VCPU_Node,
                       Deep => True);
               begin
                  VCPU_Node := DOM.Core.Nodes.Insert_Before
                    (N         => Subj_Node,
                     New_Child => VCPU_Node,
                     Ref_Child => Subj_VCPU_Node);
                  Mucfgvcpu.Set_VCPU_Profile
                    (Profile => Types.Subj_VCPU_Profile_Map (Profile),
                     Node    => VCPU_Node);
                  Muxml.Utils.Merge
                    (Left      => VCPU_Node,
                     Right     => Subj_VCPU_Node,
                     List_Tags => (1 => U ("msr")));
                  VCPU_Node := DOM.Core.Nodes.Remove_Child
                    (N         => Subj_Node,
                     Old_Child => Subj_VCPU_Node);
                  DOM.Core.Nodes.Free (N => VCPU_Node);
               end;
            end if;
         end;
      end loop;
   end Add_Subject_Profile_VCPU;

   -------------------------------------------------------------------------

   procedure Remove_Component_Reference (Data : in out Muxml.XML_Data_Type)
   is
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[component]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjects,
                                      Index => I);
         begin
            Muxml.Utils.Remove_Child
              (Node       => Subj_Node,
               Child_Name => "component");
         end;
      end loop;
   end Remove_Component_Reference;

   -------------------------------------------------------------------------

   procedure Remove_Components (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Muxml.Utils.Remove_Child
        (Node       => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
         Child_Name => "components");
   end Remove_Components;

end Expanders.Components;
