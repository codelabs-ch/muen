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

with Interfaces;

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.Types;
with Mutools.XML_Utils;
with Mutools.Constants;
with Mucfgvcpu;
with Mucfgcheck.Events;

with Expanders.Config;
with Expanders.Utils;
with Expanders.XML_Utils;
with Expanders.Subjects.Config;
with Expanders.Subjects.Profiles;

package body Expanders.Subjects
is

   use Ada.Strings.Unbounded;

   type Subject_Profile_Type is (Native, VM, Linux);

   --  Mapping of subject profiles to VCPU profiles.
   Subj_VCPU_Profile_Map : constant array
     (Subject_Profile_Type) of Mucfgvcpu.Profile_Type
     := (Native     => Mucfgvcpu.Native,
         VM | Linux => Mucfgvcpu.VM);

   --  Mapping of subject profiles to IRQ vector remapping offset.
   --  Note: Linux uses IRQ0 (vector 48) for the timer.
   Subj_IRQ_Remap_Offset : constant array
     (Subject_Profile_Type) of Natural
     := (Native     => Mutools.Constants.Host_IRQ_Remap_Offset,
         VM | Linux => 48);

   -------------------------------------------------------------------------

   procedure Add_Channel_Events (Data : in out Muxml.XML_Data_Type)
   is

      --  Add optional events/source/group[@name='vmcall'] elements.
      function Add_Optional_Events_Source_Group
        (Subject : DOM.Core.Node)
         return DOM.Core.Node;

      --  Add optional events/target element.
      function Add_Optional_Events_Target
        (Subject : DOM.Core.Node)
         return DOM.Core.Node;

      ----------------------------------------------------------------------

      function Add_Optional_Events_Source_Group
        (Subject : DOM.Core.Node)
         return DOM.Core.Node
      is
         use type DOM.Core.Node;

         Writer_Subj_Source_Node  : DOM.Core.Node;
         Writer_Subj_Source_Group : DOM.Core.Node;
         Writer_Subj_Events_Node  : constant DOM.Core.Node
           := Muxml.Utils.Get_Element (Doc   => Subject,
                                       XPath => "events");
      begin
         Writer_Subj_Source_Node := Muxml.Utils.Get_Element
           (Doc   => Writer_Subj_Events_Node,
            XPath => "source");
         if Writer_Subj_Source_Node = null then
            Writer_Subj_Source_Node := DOM.Core.Documents.Create_Element
              (Doc      => Data.Doc,
               Tag_Name => "source");
            Muxml.Utils.Insert_Before
              (Parent    => Writer_Subj_Events_Node,
               New_Child => Writer_Subj_Source_Node,
               Ref_Child => "target");
         end if;

         Writer_Subj_Source_Group := Muxml.Utils.Get_Element
           (Doc   =>  Writer_Subj_Source_Node,
            XPath => "group[@name='vmcall']");
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

         return Writer_Subj_Source_Group;
      end Add_Optional_Events_Source_Group;

      ----------------------------------------------------------------------

      function Add_Optional_Events_Target
        (Subject : DOM.Core.Node)
         return DOM.Core.Node
      is
         use type DOM.Core.Node;

         Reader_Subj_Events_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Subject,
              XPath => "events");
         Reader_Subj_Target_Node : DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Reader_Subj_Events_Node,
              XPath => "target");
      begin
         if Reader_Subj_Target_Node = null then
            Reader_Subj_Target_Node := DOM.Core.Nodes.Append_Child
              (N         => Reader_Subj_Events_Node,
               New_Child => DOM.Core.Documents.Create_Element
                 (Doc      => Data.Doc,
                  Tag_Name => "target"));
         end if;

         return Reader_Subj_Target_Node;
      end Add_Optional_Events_Target;

      ----------------------------------------------------------------------

      Events_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/events");
      Channels    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/channels/channel[@hasEvent]");
   begin
      Mulog.Log (Msg => "Adding events for" & DOM.Core.Nodes.Length
                 (List => Channels)'Img & " channel(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Channels) - 1 loop
         declare
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
              := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/subjects/subject/channels/writer"
                 & "[@physical='" & Channel_Name & "']");
            Reader_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/subjects/subject/channels/reader"
                 & "[@physical='" & Channel_Name & "']");
            Writer_Subj_Source_Group, Reader_Subj_Target_Node : DOM.Core.Node;
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

            Writer_Subj_Source_Group := Add_Optional_Events_Source_Group
              (Subject => Muxml.Utils.Ancestor_Node
                 (Node  => Writer_Node,
                  Level => 2));
            Reader_Subj_Target_Node := Add_Optional_Events_Target
              (Subject => Muxml.Utils.Ancestor_Node
                 (Node  => Reader_Node,
                  Level => 2));

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
                    Name => "physical");
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
                 Name => "physical");
            Logical_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "logical");
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
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "memory");
         begin
            Mulog.Log (Msg => "Mapping channel '" & Channel_Name & "' "
                       & (if Channel_Writer then "writable" else "readable")
                       & " to virtual address " & Channel_Addr
                       & " of subject '" & Subj_Name & "'");
            Muxml.Utils.Append_Child
              (Node      => Mem_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => Logical_Name,
                  Physical_Name => Channel_Name,
                  Address       => Channel_Addr,
                  Writable      => Channel_Writer,
                  Executable    => False));
         end;
      end loop;
   end Add_Channel_Mappings;

   -------------------------------------------------------------------------

   procedure Add_CPU_Ids (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            use type DOM.Core.Node;

            Subj_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            CPU_Id     : constant Integer
              := Mutools.XML_Utils.Get_Executing_CPU
                (Data    => Data,
                 Subject => Subj_Node);
            CPU_Id_Str : constant String
              := Ada.Strings.Fixed.Trim (Source => CPU_Id'Img,
                                         Side   => Ada.Strings.Left);
         begin
            Mulog.Log (Msg => "Setting cpu of subject '" & Subj_Name
                       & "' to " & CPU_Id_Str);
            DOM.Core.Elements.Set_Attribute (Elem  => Subj_Node,
                                             Name  => "cpu",
                                             Value => CPU_Id_Str);
         end;
      end loop;
   end Add_CPU_Ids;

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

   procedure Add_Device_BDFs (Data : in out Muxml.XML_Data_Type)
   is
      PCI_Devices  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device[pci]");
      Subj_Devices : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device[not (pci)]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subj_Devices) - 1 loop
         declare
            use type DOM.Core.Node;

            Subj_Dev  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subj_Devices,
                 Index => I);
            Log_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Dev,
                 Name => "logical");
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Dev,
                 Name => "physical");
            Phys_Dev  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => PCI_Devices,
                 Ref_Attr  => "name",
                 Ref_Value => Phys_Name);
         begin
            if Phys_Dev /= null then
               declare
                  PCI_Node  : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => Phys_Dev,
                       XPath => "pci");
                  Bus_Nr    : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => PCI_Node,
                       Name => "bus");
                  Device_Nr : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => PCI_Node,
                       Name => "device");
                  Func_Nr   : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => PCI_Node,
                       Name => "function");
                  BDF_Node  : DOM.Core.Node
                    := DOM.Core.Nodes.Clone_Node
                      (N    => PCI_Node,
                       Deep => True);
               begin
                  DOM.Core.Elements.Remove_Attribute
                    (Elem => BDF_Node,
                     Name => "msi");
                  Mulog.Log
                    (Msg => "Setting BDF of logical device '" & Log_Name
                     & "' to "
                     & Mutools.Utils.To_Hex
                       (Number     => Interfaces.Unsigned_64'Value (Bus_Nr),
                        Normalize  => False,
                        Byte_Short => True) & ":"
                     & Mutools.Utils.To_Hex
                       (Number     => Interfaces.Unsigned_64'Value (Device_Nr),
                        Normalize  => False,
                        Byte_Short => True) & "."
                     & Mutools.Utils.To_Hex
                       (Number     => Interfaces.Unsigned_64'Value (Func_Nr),
                        Normalize  => False,
                        Byte_Short => True));

                  BDF_Node := DOM.Core.Nodes.Insert_Before
                    (N         => Subj_Dev,
                     New_Child => BDF_Node,
                     Ref_Child => DOM.Core.Nodes.First_Child (N => Subj_Dev));
               end;
            end if;
         end;
      end loop;
   end Add_Device_BDFs;

   -------------------------------------------------------------------------

   procedure Add_Device_Memory_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      Unmapped_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device/memory"
           & "[not (@virtualAddress)]");
      Count : constant Natural := DOM.Core.Nodes.Length
        (List => Unmapped_Memory);
   begin
      if Count = 0 then
         return;
      end if;

      Mulog.Log (Msg => "Adding" & Count'Img & " identity mapping(s) for "
                 & "device memory");

      for I in 0 .. Count - 1 loop
         declare
            Memory_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Unmapped_Memory,
                 Index => I);
            Memory_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Memory_Node,
                 Name => "physical");
            Dev_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Memory_Node);
            Dev_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dev_Node,
                 Name => "physical");
            Physmem_Addr : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Data.Doc,
                 XPath => "/system/hardware/devices/device[@name='" & Dev_Ref
                 & "']/memory[@name='" & Memory_Ref & "']",
                 Name  => "physicalAddress");
         begin
            DOM.Core.Elements.Set_Attribute
              (Elem  => Memory_Node,
               Name  => "virtualAddress",
               Value => Physmem_Addr);
         end;
      end loop;
   end Add_Device_Memory_Mappings;

   -------------------------------------------------------------------------

   procedure Add_Device_Resources (Data : in out Muxml.XML_Data_Type)
   is
      Phys_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device[*]");
      Subj_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device[not(*)"
           & " or (count(*)=1 and pci)]");
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Subj_Devs) loop
         declare
            use type DOM.Core.Node;

            Subj_Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subj_Devs,
                                      Index => I - 1);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Muxml.Utils.Ancestor_Node
                   (Node  => Subj_Dev,
                    Level => 2),
                 Name => "name");
            Log_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Dev,
                 Name => "logical");
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Dev,
                 Name => "physical");
            Phys_Dev : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Phys_Devs,
                 Ref_Attr  => "name",
                 Ref_Value => Phys_Name);
         begin
            if Phys_Dev /= null then
               Mulog.Log (Msg => "Adding device resources of physical device '"
                          & Phys_Name & "' to logical device '" & Log_Name
                          & "' of subject '" & Subj_Name & "'");
               declare
                  Phys_Resources : constant DOM.Core.Node_List
                    := McKae.XML.XPath.XIA.XPath_Query
                      (N     => Phys_Dev,
                       XPath => "memory|irq|ioPort");
                  Phys_Res_Count : constant Natural
                    := DOM.Core.Nodes.Length (List => Phys_Resources);
               begin
                  for J in 1 .. Phys_Res_Count loop
                     Mutools.XML_Utils.Add_Resource
                       (Logical_Device    => Subj_Dev,
                        Physical_Resource => DOM.Core.Nodes.Item
                          (List  => Phys_Resources,
                           Index => J - 1));
                  end loop;
               end;
            end if;
         end;
      end loop;
   end Add_Device_Resources;

   -------------------------------------------------------------------------

   procedure Add_Device_Vectors (Data : in out Muxml.XML_Data_Type)
   is
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[devices/device]");
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Subjects) loop
         declare
            Subject        : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I - 1);
            Subject_Name   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subject,
                 Name => "name");
            Subj_Profile   : constant Subject_Profile_Type
              := Subject_Profile_Type'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Subject,
                    Name => "profile"));
            Alloc_Irqs     : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subject,
                 XPath => "devices/device/irq[not(@vector)]");
            Alloc_Count    : constant Natural
              := DOM.Core.Nodes.Length (List => Alloc_Irqs);
            Device_Vectors : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subject,
                 XPath => "devices/device/irq[@vector]");
            Event_Vectors  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subject,
                 XPath => "events/target/event[@vector!='none']");
            Irq_Allocator  : Utils.Number_Allocator_Type
              (Range_Start => Subj_IRQ_Remap_Offset (Subj_Profile),
               Range_End   => 255);
         begin
            if Alloc_Count > 0 then
               Mulog.Log (Msg => "Allocating" & Alloc_Count'Img
                          & " logical IRQ vector(s) for subject '"
                          & Subject_Name & "'");

               Utils.Reserve_Numbers (Allocator => Irq_Allocator,
                                      Nodes     => Device_Vectors,
                                      Attribute => "vector");
               Utils.Reserve_Numbers (Allocator => Irq_Allocator,
                                      Nodes     => Event_Vectors,
                                      Attribute => "vector");

               if Subj_Profile = Linux then

                  --  Exclude Linux timer IRQ0 (vector 48) which is injected by
                  --  the kernel and thus not present as device or event in the
                  --  policy.

                  Utils.Reserve_Number (Allocator => Irq_Allocator,
                                        Number    => 48);
               end if;

               for J in 1 .. DOM.Core.Nodes.Length (List => Alloc_Irqs) loop
                  declare
                     Cur_Irq    : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                         (List  => Alloc_Irqs,
                          Index => J - 1);
                     Cur_Vector : Natural;
                  begin
                     Utils.Allocate (Allocator => Irq_Allocator,
                                     Number    => Cur_Vector);
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Cur_Irq,
                        Name  => "vector",
                        Value => Ada.Strings.Fixed.Trim
                          (Source => Cur_Vector'Img,
                           Side   => Ada.Strings.Left));
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Add_Device_Vectors;

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
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
         begin
            Muxml.Utils.Add_Child
              (Parent     => Subj_Node,
               Child_Name => "devices",
               Ref_Names  => (1 => To_Unbounded_String ("events")));
            Muxml.Utils.Add_Child
              (Parent     => Subj_Node,
               Child_Name => "memory",
               Ref_Names  => (1 => To_Unbounded_String ("devices")));
            Muxml.Utils.Add_Child
              (Parent     => Subj_Node,
               Child_Name => "bootparams",
               Ref_Names  => (1 => To_Unbounded_String ("memory")));
            Muxml.Utils.Add_Child
              (Parent     => Subj_Node,
               Child_Name => "channels",
               Ref_Names  => (1 => To_Unbounded_String ("monitor"),
                              2 => To_Unbounded_String ("component")));
         end;
      end loop;
   end Add_Missing_Elements;

   -------------------------------------------------------------------------

   procedure Add_Sinfo_Regions (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj,
                 Name => "name");
            Subj_Mem_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj,
                 XPath => "memory");
         begin
            Mulog.Log (Msg => "Adding info region for subject '"
                       & Subj_Name & "'");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|sinfo",
               Address     => "",
               Size        => Mutools.Utils.To_Hex
                 (Number => Expanders.Config.Subject_Sinfo_Region_Size),
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "subject_info",
               File_Name   => Subj_Name & "_sinfo",
               File_Offset => "none");
            Muxml.Utils.Append_Child
              (Node      => Subj_Mem_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "sinfo",
                  Physical_Name => Subj_Name & "|sinfo",
                  Address       => Mutools.Utils.To_Hex
                    (Number => Config.Subject_Info_Virtual_Addr),
                  Writable      => False,
                  Executable    => False));
         end;
      end loop;
   end Add_Sinfo_Regions;

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
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/subjects");
      Tau0_Node : DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "subject");
      Mem_Node  : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "memory");
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
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "sys_interface",
            Physical_Name => "sys_interface",
            Address       => Mutools.Utils.To_Hex
              (Number => Expanders.Config.Tau0_Interface_Virtual_Addr),
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

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "tau0|bin",
         Address     => "",
         Size        => "16#0001_4000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_binary",
         File_Name   => "tau0",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "binary",
            Physical_Name => "tau0|bin",
            Address       => "16#1000#",
            Writable      => True,
            Executable    => True));
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
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "memory");
         begin
            Mulog.Log (Msg => "Mapping state of subject '"
                       & Monitored_Subj_Name & "' "
                       & (if Writable then "writable" else "readable")
                       & " to virtual address " & Address
                       & " of subject '" & Subj_Name & "'");

            Muxml.Utils.Append_Child
              (Node      => Mem_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => Monitored_Subj_Name & "|state",
                  Physical_Name => Monitored_Subj_Name & "|state",
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
            Profile : constant Subject_Profile_Type
              := Subject_Profile_Type'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Subj,
                    Name => "profile"));
            VCPU_Node : DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj,
                 XPath => "vcpu");
         begin
            if VCPU_Node = null then
               VCPU_Node := DOM.Core.Nodes.Insert_Before
                 (N         => Subj,
                  New_Child => DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "vcpu"),
                  Ref_Child => DOM.Core.Nodes.First_Child (N => Subj));
            else
               Mulog.Log (Msg => "Subject '" & Subj_Name & "' provides "
                          & "user-defined VCPU element");
            end if;

            Mulog.Log (Msg => "Setting profile of subject '" & Subj_Name
                       & "' to " & Profile'Img & " (VCPU profile "
                       & Subj_VCPU_Profile_Map (Profile)'Img & ")");
            Mucfgvcpu.Set_VCPU_Profile
              (Profile => Subj_VCPU_Profile_Map (Profile),
               Node    => VCPU_Node);

            case Profile is
               when Native => null;
               when VM     => null;
               when Linux =>
                  Profiles.Handle_Linux_Profile
                    (Data    => Data,
                     Subject => Subj);
            end case;

            DOM.Core.Elements.Remove_Attribute
              (Elem => Subj,
               Name => "profile");
         end;
      end loop;
   end Handle_Profile;

   -------------------------------------------------------------------------

   procedure Handle_Timers (Data : in out Muxml.XML_Data_Type)
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
            Subj_Mem_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj,
                 XPath => "memory");
         begin
            Mulog.Log
              (Msg => "Adding timer page for subject '" & Subj_Name & "'");

            Muxml.Utils.Append_Child
              (Node      => Subj_Mem_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "timer",
                  Physical_Name => Subj_Name & "|timer",
                  Address       => Mutools.Utils.To_Hex
                    (Number => Config.Subject_Timer_Virtual_Addr),
                  Writable      => True,
                  Executable    => False));
         end;
      end loop;
   end Handle_Timers;

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
