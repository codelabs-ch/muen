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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Types;
with Mucfgvcpu;
with Mucfgcheck.Events;

with Expanders.XML_Utils;
with Expanders.Subjects.Profiles;

package body Expanders.Subjects
is

   --  Add element with given name to subjects missing such an element. The new
   --  node is inserted before the reference node with the specified name.
   procedure Add_Optional_Element
     (Data         : in out Muxml.XML_Data_Type;
      Element_Name :        String;
      Ref_Name     :        String);

   -------------------------------------------------------------------------

   procedure Add_Binaries (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/binary");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Bin_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Filename : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Bin_Node,
                 Name => "filename");
            Filesize : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Bin_Node,
                 Name => "size");
            Virtual_Address : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Bin_Node,
                 Name => "virtualAddress");
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Bin_Node);
            Subj_Mem_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  =>  McKae.XML.XPath.XIA.XPath_Query
                 (N     => Subj_Node,
                  XPath => "memory"),
               Index => 0);
            Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Subj_Node,
               Name => "name");
         begin
            Mulog.Log (Msg => "Mapping binary '" & Filename & "' with size "
                       & Filesize & " at virtual address " & Virtual_Address
                       & " of subject '" & Subj_Name & "'");
            XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|bin",
               Address     => "",
               Size        => Filesize,
               Caching     => "WB",
               Alignment   => "16#1000#",
               File_Name   => Filename,
               File_Format => "bin_raw",
               File_Offset => "none");
            Muxml.Utils.Append_Child
              (Node      => Subj_Mem_Node,
               New_Child => XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "binary",
                  Physical_Name => Subj_Name & "|bin",
                  Address       => Virtual_Address,
                  Writable      => True,
                  Executable    => True));

            Muxml.Utils.Remove_Child
              (Node       => Subj_Node,
               Child_Name => "binary");
         end;
      end loop;
   end Add_Binaries;

   -------------------------------------------------------------------------

   procedure Add_Channel_Events (Data : in out Muxml.XML_Data_Type)
   is
      Events_Node : constant DOM.Core.Node
        := DOM.Core.Nodes.Item
          (List  => McKae.XML.XPath.XIA.XPath_Query
             (N     => Data.Doc,
              XPath => "/system/events"),
           Index => 0);
      Channels    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/channels/channel[@hasEvent]");
   begin
      Mulog.Log (Msg => "Adding channel events for" & DOM.Core.Nodes.Length
                 (List => Channels)'Img & " channel(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Channels) - 1 loop
         declare
            use Ada.Strings.Unbounded;
            use type DOM.Core.Node;

            Channel_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Channels,
                 Index => I);
            Channel_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "name");
            Channel_Mode : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "hasEvent");
            Event_Node  : DOM.Core.Node;
            Writer_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Data.Doc,
                    XPath => "/system/subjects/subject/channels/writer[@ref='"
                    & Channel_Name & "']"),
                 Index => 0);
            Reader_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Data.Doc,
                    XPath => "/system/subjects/subject/channels/reader[@ref='"
                    & Channel_Name & "']"),
                 Index => 0);
            Writer_Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Writer_Node,
                 Level => 2);
            Writer_Subj_Events_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Writer_Subj_Node,
                    XPath => "events"),
                 Index => 0);

            Writer_Subj_Source_Node  : DOM.Core.Node;
            Writer_Subj_Source_Group : DOM.Core.Node;

            Reader_Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Reader_Node,
                 Level => 2);
            Reader_Subj_Events_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Reader_Subj_Node,
                    XPath => "events"),
                 Index => 0);
            Reader_Subj_Target_Node : DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Reader_Subj_Events_Node,
                    XPath => "target"),
                 Index => 0);
         begin
            Event_Node := DOM.Core.Documents.Create_Element
              (Doc      => Data.Doc,
               Tag_Name => "event");
            DOM.Core.Elements.Set_Attribute
              (Elem  => Event_Node,
               Name  => "name",
               Value => Channel_Name);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Event_Node,
               Name  => "mode",
               Value => Channel_Mode);
            Muxml.Utils.Append_Child
              (Node      => Events_Node,
               New_Child => Event_Node);

            Writer_Subj_Source_Node := DOM.Core.Nodes.Item
              (List  => McKae.XML.XPath.XIA.XPath_Query
                 (N     => Writer_Subj_Events_Node,
                  XPath => "source"),
               Index => 0);
            if Writer_Subj_Source_Node = null then
               declare
                  Ref_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => McKae.XML.XPath.XIA.XPath_Query
                         (N     => Writer_Subj_Events_Node,
                          XPath => "target"),
                       Index => 0);
               begin
                  Writer_Subj_Source_Node := DOM.Core.Nodes.Insert_Before
                    (N         => Writer_Subj_Events_Node,
                     New_Child => DOM.Core.Documents.Create_Element
                       (Doc      => Data.Doc,
                        Tag_Name => "source"),
                     Ref_Child => Ref_Node);
               end;
            end if;

            Writer_Subj_Source_Group := DOM.Core.Nodes.Item
              (List  => McKae.XML.XPath.XIA.XPath_Query
                 (N     => Writer_Subj_Source_Node,
                  XPath => "group[@name='vmcall']"),
               Index => 0);
            if Writer_Subj_Source_Group = null then
               Writer_Subj_Source_Group := DOM.Core.Nodes.Append_Child
                 (N         => Writer_Subj_Source_Node,
                  New_Child => DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "group"));
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Writer_Subj_Source_Group,
                  Name  => "name",
                  Value => "vmcall");
            end if;

            if Reader_Subj_Target_Node = null then
               Reader_Subj_Target_Node := DOM.Core.Nodes.Append_Child
                 (N         => Reader_Subj_Events_Node,
                  New_Child => DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "target"));
            end if;

            declare
               ID : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Writer_Node,
                    Name => "event");
               Vector : Unbounded_String
                 := To_Unbounded_String
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Reader_Node,
                       Name => "vector"));
               Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Writer_Node,
                    Name => "ref");
            begin
               if Vector = Null_Unbounded_String then
                  Vector := To_Unbounded_String ("none");
               end if;

               Muxml.Utils.Append_Child
                 (Node      => Writer_Subj_Source_Group,
                  New_Child => XML_Utils.Create_Source_Event_Node
                    (Policy        => Data,
                     ID            => ID,
                     Logical_Name  => "channel_event_" & Name,
                     Physical_Name => Name,
                     Action        => "continue"));
               Muxml.Utils.Append_Child
                 (Node      => Reader_Subj_Target_Node,
                  New_Child => XML_Utils.Create_Target_Event_Node
                    (Policy        => Data,
                     Logical_Name  => "channel_event_" & Name,
                     Physical_Name => Name,
                     Vector        => To_String (Vector)));
            end;
         end;
      end loop;
   end Add_Channel_Events;

   -------------------------------------------------------------------------

   procedure Add_Channel_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/channels/*");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Channel_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Channel_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "ref");
            Channel_Addr : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "virtualAddress");
            Channel_Writer : constant Boolean
              := DOM.Core.Nodes.Node_Name (N => Channel_Node) = "writer";
            Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Channel_Node,
                 Level => 2);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Mem_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Subj_Node,
                    XPath => "memory"),
                 Index => 0);
         begin
            Mulog.Log (Msg => "Mapping channel '" & Channel_Name & "' "
                       & (if Channel_Writer then "writable" else "readable")
                       & " to virtual address " & Channel_Addr
                       & " of subject '" & Subj_Name & "'");
            Muxml.Utils.Append_Child
              (Node      => Mem_Node,
               New_Child => XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => Channel_Name,
                  Physical_Name => Channel_Name,
                  Address       => Channel_Addr,
                  Writable      => Channel_Writer,
                  Executable    => False));
         end;
      end loop;
   end Add_Channel_Mappings;

   -------------------------------------------------------------------------

   procedure Add_Default_Events (Data : in out Muxml.XML_Data_Type)
   is
      Nodes  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/events/source/group/default");

      --  Returns True if an event with specified reference ID exists in the
      --  given node list.
      function ID_Exists
        (Nodes  : DOM.Core.Node_List;
         Ref_ID : Natural)
         return Boolean;

      ----------------------------------------------------------------------

      function ID_Exists
        (Nodes  : DOM.Core.Node_List;
         Ref_ID : Natural)
         return Boolean
      is
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
            declare
               Node : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Nodes,
                    Index => I);
               ID_Str : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "id");
            begin
               if Natural'Value (ID_Str) = Ref_ID then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end ID_Exists;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Def_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Action : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Def_Node,
                 Name => "action");
            Physical_Name : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Def_Node,
                 XPath => "notify",
                 Name  => "physical");
            Group_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Def_Node);
            Group_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Group_Node,
                 Name => "name");
            Group : constant Mutools.Types.Event_Group_Type
              := Mutools.Types.Event_Group_Type'Value (Group_Name);
            Group_Events : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Group_Node,
                 XPath => "event");
            Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Def_Node,
                 Level => 4);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Group_Max_Event : constant Natural := Mucfgcheck.Events.Get_Max_ID
              (Group => Group);
         begin
            Mulog.Log (Msg => "Adding default events to event group '"
                       & Group_Name & "' of subject '" & Subj_Name & "'");

            for ID in Natural range 0 .. Group_Max_Event loop
               declare
                  ID_Str : constant String := Ada.Strings.Fixed.Trim
                    (Source => ID'Img,
                     Side   => Ada.Strings.Left);
               begin
                  if Mucfgcheck.Events.Is_Valid_Event_ID
                    (Group => Group,
                     ID    => ID)
                    and then
                      not ID_Exists (Nodes  => Group_Events,
                                     Ref_ID => ID)
                  then
                     Muxml.Utils.Append_Child
                       (Node      => Group_Node,
                        New_Child => XML_Utils.Create_Source_Event_Node
                          (Policy        => Data,
                           ID            => ID_Str,
                           Logical_Name  => "default_event_" & ID_Str,
                           Physical_Name => Physical_Name,
                           Action        => Action));
                  end if;
               end;
            end loop;

            Muxml.Utils.Remove_Child (Node       => Group_Node,
                                      Child_Name => "default");
         end;
      end loop;

   end Add_Default_Events;

   -------------------------------------------------------------------------

   procedure Add_Ids (Data : in out Muxml.XML_Data_Type)
   is
      Nodes  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[not (@id)]");
      Cur_Id : Positive := 1;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Subj_Node,
               Name => "name");
            Id_Str    : constant String := Ada.Strings.Fixed.Trim
              (Source => Cur_Id'Img,
               Side   => Ada.Strings.Left);
         begin
            Mulog.Log (Msg => "Setting id of subject '" & Subj_Name & "' to "
                       & Id_Str);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Subj_Node,
               Name  => "id",
               Value => Id_Str);
            Cur_Id := Cur_Id + 1;
         end;
      end loop;
   end Add_Ids;

   -------------------------------------------------------------------------

   procedure Add_Missing_Elements (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Add_Optional_Element (Data         => Data,
                            Element_Name => "devices",
                            Ref_Name     => "events");
      Add_Optional_Element (Data         => Data,
                            Element_Name => "memory",
                            Ref_Name     => "devices");
      Add_Optional_Element (Data         => Data,
                            Element_Name => "bootparams",
                            Ref_Name     => "memory");
   end Add_Missing_Elements;

   -------------------------------------------------------------------------

   procedure Add_Optional_Element
     (Data         : in out Muxml.XML_Data_Type;
      Element_Name :        String;
      Ref_Name     :        String)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[not (" & Element_Name & ")]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Ref_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Subj_Node,
                    XPath => Ref_Name),
                 Index => 0);
            Elem_Node : DOM.Core.Node
              := DOM.Core.Documents.Create_Element
                (Doc      => Data.Doc,
                 Tag_Name => Element_Name);
         begin
            Elem_Node := DOM.Core.Nodes.Insert_Before
              (N         => Subj_Node,
               New_Child => Elem_Node,
               Ref_Child => Ref_Node);
            pragma Unreferenced (Elem_Node);
         end;
      end loop;
   end Add_Optional_Element;

   -------------------------------------------------------------------------

   procedure Add_Tau0 (Data : in out Muxml.XML_Data_Type)
   is
      Tau0_CPU : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Data.Doc,
           XPath => "/system/scheduling/majorFrame/cpu/"
           & "minorFrame[@subject='tau0']/..",
           Name  => "id");
      Subjects_Node : constant DOM.Core.Node
        := DOM.Core.Nodes.Item
          (List  => McKae.XML.XPath.XIA.XPath_Query
             (N     => Data.Doc,
              XPath => "/system/subjects"),
           Index => 0);
      Tau0_Node : DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "subject");
      Mem_Node  : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "memory");
      Bin_Node  : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "binary");
   begin
      Mulog.Log (Msg => "Adding tau0 subject");

      Tau0_Node := DOM.Core.Nodes.Insert_Before
        (N         => Subjects_Node,
         New_Child => Tau0_Node,
         Ref_Child => DOM.Core.Nodes.First_Child (N => Subjects_Node));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "id",
         Value => "0");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "name",
         Value => "tau0");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "profile",
         Value => "native");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "cpu",
         Value => Tau0_CPU);

      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "bootparams"));

      Muxml.Utils.Append_Child
        (Node      => Mem_Node,
         New_Child => XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "sys_interface",
            Physical_Name => "sys_interface",
            Address       => "16#001f_f000#",
            Writable      => True,
            Executable    => False));
      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => Mem_Node);

      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "devices"));
      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "events"));

      DOM.Core.Elements.Set_Attribute
        (Elem  => Bin_Node,
         Name  => "filename",
         Value => "tau0");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Bin_Node,
         Name  => "size",
         Value => "16#0001_4000#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Bin_Node,
         Name  => "virtualAddress",
         Value => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => Bin_Node);
   end Add_Tau0;

   -------------------------------------------------------------------------

   procedure Handle_Monitors (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/monitor/state");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Monitored_Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Monitored_Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Monitored_Subj_Node,
                 Name => "subject");
            Address   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Monitored_Subj_Node,
                 Name => "virtualAddress");
            Writable  : constant Boolean := Boolean'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Monitored_Subj_Node,
                  Name => "writable"));
            Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Monitored_Subj_Node,
                 Level => 2);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Mem_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  =>  McKae.XML.XPath.XIA.XPath_Query
                   (N     => Subj_Node,
                    XPath => "memory"),
                 Index => 0);
         begin
            Mulog.Log (Msg => "Mapping state of subject '"
                       & Monitored_Subj_Name & "' "
                       & (if Writable then "writable" else "readable")
                       & " to virtual address " & Address
                       & " of subject '" & Subj_Name & "'");

            Muxml.Utils.Append_Child
              (Node      => Mem_Node,
               New_Child => XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => Monitored_Subj_Name & "_state",
                  Physical_Name => Monitored_Subj_Name & "_state",
                  Address       => Address,
                  Writable      => Writable,
                  Executable    => False));

            Muxml.Utils.Remove_Child
              (Node       => Subj_Node,
               Child_Name => "monitor");
         end;
      end loop;
   end Handle_Monitors;

   -------------------------------------------------------------------------

   procedure Handle_Profile (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            use type DOM.Core.Node;
            use type Mucfgvcpu.Profile_Type;

            Subj : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj,
                 Name => "name");
            Profile : constant Mucfgvcpu.Profile_Type
              := Mucfgvcpu.Profile_Type'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Subj,
                    Name => "profile"));
            VCPU_Node : DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Subj,
                    XPath => "vcpu"),
                 Index => 0);
         begin
            if VCPU_Node = null then
               VCPU_Node := DOM.Core.Nodes.Insert_Before
                 (N         => Subj,
                  New_Child => DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "vcpu"),
                  Ref_Child => DOM.Core.Nodes.First_Child (N => Subj));
            end if;

            Mulog.Log (Msg => "Setting profile of subject '" & Subj_Name
                       & "' to " & Profile'Img);
            Mucfgvcpu.Set_VCPU_Profile (Profile => Profile,
                                        Node    => VCPU_Node);

            if Profile = Mucfgvcpu.Linux then
               Profiles.Handle_Linux_Profile
                 (Data    => Data,
                  Subject => Subj);
            end if;

            DOM.Core.Elements.Remove_Attribute
              (Elem => Subj,
               Name => "profile");
         end;
      end loop;
   end Handle_Profile;

   -------------------------------------------------------------------------

   procedure Remove_Channel_Elements (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/channels/..");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         Muxml.Utils.Remove_Child
           (Node       => DOM.Core.Nodes.Item
              (List  => Nodes,
               Index => I),
            Child_Name => "channels");
      end loop;
   end Remove_Channel_Elements;

end Expanders.Subjects;
