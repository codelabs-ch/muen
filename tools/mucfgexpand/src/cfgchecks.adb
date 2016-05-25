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

with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mucfgcheck;

package body Cfgchecks
is

   --  Check the existence of channel endpoint (reader or writer) event
   --  attributes given by name. The XPath query specifies which global
   --  channels should be checked.
   procedure Check_Channel_Events_Attr
     (XML_Data  : Muxml.XML_Data_Type;
      XPath     : String;
      Endpoint  : String;
      Attr_Name : String);

   --  Check subject mappings of given logical component resources against
   --  specified physical resources.
   procedure Check_Component_Resource_Mappings
     (Logical_Resources  : DOM.Core.Node_List;
      Physical_Resources : DOM.Core.Node_List;
      Resource_Type      : String;
      Subject            : DOM.Core.Node);

   --  Calls the Check_Resources procedure for each component resource with
   --  the corresponding physical resource as parameter.
   procedure Check_Component_Resources
     (Logical_Resources  : DOM.Core.Node_List;
      Physical_Resources : DOM.Core.Node_List;
      Subject            : DOM.Core.Node;
      Check_Resource     : not null access procedure
        (Logical_Resource  : DOM.Core.Node;
         Physical_Resource : DOM.Core.Node));

   --  The procedure checks for all existing subjects in the specified policy
   --  that a given attribute of component resource mappings is unique
   --  per-subject.
   procedure Check_Subject_Resource_Maps_Attr_Uniqueness
     (XML_Data : Muxml.XML_Data_Type;
      Attr     : String);

   -------------------------------------------------------------------------

   procedure Channel_Reader_Has_Event_Vector (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Channel_Events_Attr
        (XML_Data  => XML_Data,
         XPath     => "/system/channels/channel[@hasEvent!='switch']",
         Endpoint  => "reader",
         Attr_Name => "vector");
   end Channel_Reader_Has_Event_Vector;

   -------------------------------------------------------------------------

   procedure Channel_Reader_Writer (XML_Data : Muxml.XML_Data_Type)
   is
      Channels : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/channels/channel");
      Readers  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/channels/reader");
      Writers  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/channels/writer");
   begin
      Mulog.Log (Msg => "Checking" & DOM.Core.Nodes.Length
                 (List => Channels)'Img & " channel(s) for reader/writer "
                 & "count");
      for I in 0 .. DOM.Core.Nodes.Length (List => Channels) - 1 loop
         declare
            Channel      : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Channels,
                 Index => I);
            Channel_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel,
                 Name => "name");
            Has_Event    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel,
                 Name => "hasEvent");
            Reader_Count : constant Natural
              := DOM.Core.Nodes.Length
                (List => Muxml.Utils.Get_Elements
                   (Nodes     => Readers,
                    Ref_Attr  => "physical",
                    Ref_Value => Channel_Name));
            Writer_Count : constant Natural
              := DOM.Core.Nodes.Length
                (List => Muxml.Utils.Get_Elements
                   (Nodes     => Writers,
                    Ref_Attr  => "physical",
                    Ref_Value => Channel_Name));
         begin
            if (Has_Event'Length > 0 and then Reader_Count /= 1)
              or (Has_Event'Length = 0 and then Reader_Count < 1)
            then
               raise Mucfgcheck.Validation_Error with "Invalid number of "
                 & "readers for channel '" & Channel_Name & "':"
                 & Reader_Count'Img;
            end if;

            if Writer_Count /= 1 then
               raise Mucfgcheck.Validation_Error with "Invalid number of "
                 & "writers for channel '" & Channel_Name & "':"
                 & Writer_Count'Img;
            end if;
         end;
      end loop;
   end Channel_Reader_Writer;

   -------------------------------------------------------------------------

   procedure Channel_Writer_Has_Event_ID (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Channel_Events_Attr
        (XML_Data  => XML_Data,
         XPath     => "/system/channels/channel[@hasEvent]",
         Endpoint  => "writer",
         Attr_Name => "event");
   end Channel_Writer_Has_Event_ID;

   -------------------------------------------------------------------------

   procedure Check_Channel_Events_Attr
     (XML_Data  : Muxml.XML_Data_Type;
      XPath     : String;
      Endpoint  : String;
      Attr_Name : String)
   is
      Channels  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => XPath);
      Endpoints : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/channels/" & Endpoint);
   begin
      Mulog.Log (Msg => "Checking '" & Attr_Name & "' attribute of"
                 & DOM.Core.Nodes.Length (List => Channels)'Img & " channel "
                 & Endpoint & "(s) with associated event");

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
            Node         : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Endpoints,
                 Ref_Attr  => "physical",
                 Ref_Value => Channel_Name);
         begin
            if DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => Attr_Name) = ""
            then
               raise Mucfgcheck.Validation_Error with "Missing '" & Attr_Name
                 & "' attribute for " & Endpoint & " of channel '"
                 & Channel_Name & "'";
            end if;
         end;
      end loop;
   end Check_Channel_Events_Attr;

   -------------------------------------------------------------------------

   procedure Check_Component_Resource_Mappings
     (Logical_Resources  : DOM.Core.Node_List;
      Physical_Resources : DOM.Core.Node_List;
      Resource_Type      : String;
      Subject            : DOM.Core.Node)
   is
      Subj_Name     : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Subject,
           Name => "name");
      Comp_Name     : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Subject,
           XPath => "component",
           Name  => "ref");
      Mappings      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Subject,
           XPath => "component/map");
      Log_Res_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Logical_Resources);
   begin
      if Log_Res_Count = 0 then
         return;
      end if;

      Mulog.Log (Msg => "Checking mapping(s) of" & Log_Res_Count'Img
                 & " component logical " & Resource_Type & " resource(s) of "
                 & "subject '" & Subj_Name & "' with component '" & Comp_Name
                 & "'");
      for I in 0 .. Log_Res_Count - 1 loop
         declare
            use type DOM.Core.Node;

            Log_Res   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Logical_Resources,
                 Index => I);
            Log_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Log_Res,
                 Name => "logical");
            Phys_Name : constant String
              := Muxml.Utils.Get_Attribute
                (Nodes     => Mappings,
                 Ref_Attr  => "logical",
                 Ref_Value => Log_Name,
                 Attr_Name => "physical");
            Phys_Res  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Resources,
                 Ref_Attr  => "name",
                 Ref_Value => Phys_Name);
         begin
            if Phys_Name'Length = 0 then
               raise Mucfgcheck.Validation_Error with "Subject '" & Subj_Name
                 & "' does not map logical " & Resource_Type & " '" & Log_Name
                 & "' as requested by referenced component '"& Comp_Name
                 & "'";
            end if;

            if Phys_Res = null then
               raise Mucfgcheck.Validation_Error with "Physical "
                 & Resource_Type & " '" & Phys_Name & "' referenced by mapping"
                 & " of component logical resource '" & Log_Name
                 & "' by subject" & " '" & Subj_Name & "' does not exist";
            end if;
         end;
      end loop;
   end Check_Component_Resource_Mappings;

   -------------------------------------------------------------------------

   procedure Check_Component_Resources
     (Logical_Resources  : DOM.Core.Node_List;
      Physical_Resources : DOM.Core.Node_List;
      Subject            : DOM.Core.Node;
      Check_Resource     : not null access procedure
        (Logical_Resource  : DOM.Core.Node;
         Physical_Resource : DOM.Core.Node))
   is
      Mappings      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Subject,
           XPath => "component/map");
      Log_Res_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Logical_Resources);
   begin
      for I in 0 .. Log_Res_Count - 1 loop
         declare
            use type DOM.Core.Node;

            Log_Res   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Logical_Resources,
                 Index => I);
            Log_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Log_Res,
                 Name => "logical");
            Phys_Name : constant String
              := Muxml.Utils.Get_Attribute
                (Nodes     => Mappings,
                 Ref_Attr  => "logical",
                 Ref_Value => Log_Name,
                 Attr_Name => "physical");
            Phys_Res  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Resources,
                 Ref_Attr  => "name",
                 Ref_Value => Phys_Name);
         begin
            Check_Resource (Logical_Resource  => Log_Res,
                            Physical_Resource => Phys_Res);
         end;
      end loop;
   end Check_Component_Resources;

   -------------------------------------------------------------------------

   procedure Check_Subject_Resource_Maps_Attr_Uniqueness
     (XML_Data : Muxml.XML_Data_Type;
      Attr     : String)
   is
      --  Check inequality of specified mappings attribute.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => Attr);
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => Attr);
      begin
         if Left_Name = Right_Name then
            raise Mucfgcheck.Validation_Error with "Multiple " & Attr
              & " resource mappings with name '" & Left_Name & "' in subject '"
              & DOM.Core.Elements.Get_Attribute
              (Elem => Muxml.Utils.Ancestor_Node
                 (Node  => Left,
                  Level => 2),
               Name => "name") & "'";
         end if;
      end Check_Inequality;

      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjects,
                                      Index => I);
            Mappings  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "component/map");
         begin
            if DOM.Core.Nodes.Length (List => Mappings) > 1 then
               Mulog.Log (Msg => "Checking uniqueness of"
                          & DOM.Core.Nodes.Length (List => Mappings)'Img
                          & " subject " & Attr & " resource mappings");
               Mucfgcheck.Compare_All (Nodes      => Mappings,
                                       Comparator => Check_Inequality'Access);
            end if;
         end;
      end loop;
   end Check_Subject_Resource_Maps_Attr_Uniqueness;

   -------------------------------------------------------------------------

   procedure Component_Channel_Name_Uniqueness
     (XML_Data : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;

      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");

      Component_Name : Unbounded_String;

      --  Check inequality of logical channel names.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "logical");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "logical");
      begin
         if Left_Name = Right_Name then
            raise Mucfgcheck.Validation_Error with "Multiple channels with "
              & "name '" & Left_Name & "' in component '"
              & To_String (Component_Name) & "'";
         end if;
      end Check_Inequality;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Components) - 1 loop
         declare
            Comp_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Components,
                                      Index => I);
            Channels  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "channels/*");
         begin
            Component_Name := To_Unbounded_String
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Comp_Node,
                  Name => "name"));
            if DOM.Core.Nodes.Length (List => Channels) > 1 then
               Mulog.Log (Msg => "Checking uniqueness of"
                          & DOM.Core.Nodes.Length (List => Channels)'Img
                          & " channel names in component '"
                          & To_String (Component_Name) & "'");
               Mucfgcheck.Compare_All (Nodes      => Channels,
                                       Comparator => Check_Inequality'Access);
            end if;
         end;
      end loop;
   end Component_Channel_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Component_Channel_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Components    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");
      Phys_Channels : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/channels/channel");
      Subjects      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
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
            Comp_Name     : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Subj_Node,
                 XPath => "component",
                 Name  => "ref");
            Comp_Node     : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Name);
            Comp_Channels : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "channels/*");
            Channel_Count : constant Natural
              := DOM.Core.Nodes.Length (Comp_Channels);

            --  Check equality of logical and physical channel size.
            procedure Check_Channel_Size
              (Logical_Resource  : DOM.Core.Node;
               Physical_Resource : DOM.Core.Node);

            ----------------------------------------------------------------

            procedure Check_Channel_Size
              (Logical_Resource  : DOM.Core.Node;
               Physical_Resource : DOM.Core.Node)
            is
               use type Interfaces.Unsigned_64;

               Log_Channel_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Resource,
                    Name => "logical");
               Log_Channel_Size : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Resource,
                    Name => "size");
               Phys_Channel_Size : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Physical_Resource,
                    Name => "size");
               Phys_Channel_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Physical_Resource,
                    Name => "name");
            begin
               if Interfaces.Unsigned_64'Value (Log_Channel_Size)
                 /= Interfaces.Unsigned_64'Value (Phys_Channel_Size)
               then
                  raise Mucfgcheck.Validation_Error with "Component '"
                    & Comp_Name & "' referenced by subject '" & Subj_Name
                    & "' requests size " & Log_Channel_Size & " for "
                    & "logical channel '" & Log_Channel_Name & "' but "
                    & "linked physical channel '" & Phys_Channel_Name
                    & "' " & "has size " & Phys_Channel_Size;
               end if;
            end Check_Channel_Size;
         begin
            if Channel_Count > 0 then
               Mulog.Log (Msg => "Checking size of" & Channel_Count'Img
                          & " component '" & Comp_Name & "' channel(s) "
                          & "referenced by subject '" & Subj_Name & "'");

               Check_Component_Resources
                 (Logical_Resources  => Comp_Channels,
                  Physical_Resources => Phys_Channels,
                  Subject            => Subj_Node,
                  Check_Resource     => Check_Channel_Size'Access);
            end if;
         end;
      end loop;
   end Component_Channel_Size;

   -------------------------------------------------------------------------

   procedure Device_RMRR_Domain_Assignment (XML_Data : Muxml.XML_Data_Type)
   is
      Regions   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/memory/reservedMemory");
      Reg_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Regions);
      RMRR_Refs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device/reservedMemory");
   begin
      if Reg_Count = 0 then
         return;
      end if;

      Mulog.Log (Msg => "Checking device domain assignment of" & Reg_Count'Img
                 & " reserved memory region(s)");

      for I in 1 .. Reg_Count loop
         declare
            Region      : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Regions,
                                      Index => I - 1);
            Region_Name : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Region,
                                                  Name => "name");
            Refs        : constant DOM.Core.Node_List
              := Muxml.Utils.Get_Elements (Nodes     => RMRR_Refs,
                                           Ref_Attr  => "ref",
                                           Ref_Value => Region_Name);
            Refs_Count  : constant Natural
              := DOM.Core.Nodes.Length (List => Refs);
            Cur_Domain  : DOM.Core.Node;
         begin
            if Refs_Count < 2 then
               return;
            end if;

            for J in 1 .. Refs_Count loop
               declare
                  use type DOM.Core.Node;

                  Ref        : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Refs,
                                            Index => J - 1);
                  Dev_Name   : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => DOM.Core.Nodes.Parent_Node (N => Ref),
                       Name => "name");
                  Ref_Domain : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => XML_Data.Doc,
                       XPath => "/system/deviceDomains/domain"
                       & "[devices/device/@physical='" & Dev_Name & "']");
               begin
                  if Ref_Domain /= null then

                     --  Device is actually assigned to device domain.

                     if Cur_Domain = null then
                        Cur_Domain := Ref_Domain;
                     elsif Cur_Domain /= Ref_Domain then
                        declare
                           Cur_Dom_Name : constant String
                             := DOM.Core.Elements.Get_Attribute
                               (Elem => Cur_Domain,
                                Name => "name");
                           Ref_Dom_Name : constant String
                             := DOM.Core.Elements.Get_Attribute
                               (Elem => Ref_Domain,
                                Name => "name");
                        begin
                           raise Mucfgcheck.Validation_Error with "Device '"
                             & Dev_Name & "' referencing reserved memory "
                             & "region '" & Region_Name & "' assigned to "
                             & "different device domain than other device(s) "
                             & "referencing the same region: '" & Ref_Dom_Name
                             & "' vs '" & Cur_Dom_Name & "'";
                        end;
                     end if;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Device_RMRR_Domain_Assignment;

   -------------------------------------------------------------------------

   procedure Hardware_CPU_Count_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      Attr_Path : constant String := "/system/hardware/processor/@cpuCores";
      Attr      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => Attr_Path);
   begin
      Mulog.Log (Msg => "Checking presence of '" & Attr_Path & "' attribute");

      if DOM.Core.Nodes.Length (List => Attr) /= 1 then
         raise Mucfgcheck.Validation_Error with "Required "
           & "'" & Attr_Path & "' attribute not found, add it or use "
           & "mucfgmerge tool";
      end if;
   end Hardware_CPU_Count_Presence;

   -------------------------------------------------------------------------

   procedure Hardware_IOAPIC_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      Device : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device[@name='ioapic']/memory");
      Count  : constant Natural := DOM.Core.Nodes.Length (List => Device);
   begin
      Mulog.Log (Msg => "Checking presence of I/O APIC device");

      if Count < 1 then
         raise Mucfgcheck.Validation_Error with "Required I/O APIC device with"
           & " memory region missing";
      elsif Count > 1 then
         raise Mucfgcheck.Validation_Error with "Multiple I/O APIC devices"
           & " or I/O APIC device with multiple memory regions present";
      end if;
   end Hardware_IOAPIC_Presence;

   -------------------------------------------------------------------------

   procedure Hardware_IOMMU_Memory (XML_Data : Muxml.XML_Data_Type)
   is
      IOMMUs    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device[capabilities/"
           & "capability/@name='iommu']");
      Dev_Count : constant Natural := DOM.Core.Nodes.Length (List => IOMMUs);
   begin
      Mulog.Log (Msg => "Checking presence of" & Dev_Count'Img
                 & " IOMMU memory region(s)");

      for I in 0 .. Dev_Count - 1 loop
         declare
            IOMMU     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => IOMMUs,
                 Index => I);
            Dev_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => IOMMU,
                 Name => "name");
            Memory    : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => IOMMU,
                 XPath => "memory");
            Mem_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Memory);
         begin
            if Mem_Count < 1 then
               raise Mucfgcheck.Validation_Error with "IOMMU device '"
                 & Dev_Name & "' has no memory region";
            elsif Mem_Count > 1 then
               raise Mucfgcheck.Validation_Error with "IOMMU device '"
                 & Dev_Name & "' has multiple memory regions";
            end if;
         end;
      end loop;
   end Hardware_IOMMU_Memory;

   -------------------------------------------------------------------------

   procedure Hardware_Reserved_Memory_Region_Name_Uniqueness
     (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/memory/reservedMemory");

      --  Check inequality of memory region names.
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
            raise Mucfgcheck.Validation_Error with "Multiple reserved memory "
              & "regions with name '" & Left_Name & "'";
         end if;
      end Check_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " reserved memory region name(s)");

      Mucfgcheck.Compare_All (Nodes      => Nodes,
                              Comparator => Check_Inequality'Access);
   end Hardware_Reserved_Memory_Region_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Hardware_Reserved_Memory_Region_References
     (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Match name of reference and reserved memory region.
      function Match_Region_Name (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Ref_Region_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "ref");
         Dev_Name        : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
            Name => "name");
      begin
         return "Reserved region '" & Ref_Region_Name & "' referenced by "
           & "device '" & Dev_Name & "' does not exist";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_Region_Name (Left, Right : DOM.Core.Node) return Boolean
      is
         Ref_Name    : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "ref");
         Region_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         return Ref_Name = Region_Name;
      end Match_Region_Name;
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/hardware/devices/device/reservedMemory",
         Ref_XPath    => "/system/hardware/memory/reservedMemory",
         Log_Message  => "reserved memory region reference(s)",
         Error        => Error_Msg'Access,
         Match        => Match_Region_Name'Access);
   end Hardware_Reserved_Memory_Region_References;

   -------------------------------------------------------------------------

   procedure Kernel_Diagnostics_Dev_Reference (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Mulog.Log (Msg => "Checking presence of kernel diagnostics device");

      declare
         use type DOM.Core.Node;

         Kernel_Diag_Dev  : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => XML_Data.Doc,
              XPath => "/system/kernelDiagnosticsDevice");
         Kernel_Diag_Port : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Kernel_Diag_Dev,
              XPath => "ioPort");
         Dev_Name         : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Kernel_Diag_Dev,
              Name => "physical");
         Port_Name        : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Kernel_Diag_Port,
              Name => "physical");
         Physical_Port    : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => XML_Data.Doc,
              XPath => "/system/hardware/devices/device[@name='" & Dev_Name
              & "' and ioPort/@name='" & Port_Name & "']");
         Alias_Port       : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => XML_Data.Doc,
              XPath => "/system/platform/mappings/aliases/alias[@name='"
              & Dev_Name & "' and resource/@name='" & Port_Name & "']");
      begin
         if Physical_Port = null and then Alias_Port = null then
            raise Mucfgcheck.Validation_Error with "Kernel diagnostics device "
              & "'" & Dev_Name & "' with I/O port resource '" & Port_Name
              & "' does not reference a physical I/O device or alias";
         end if;
      end;
   end Kernel_Diagnostics_Dev_Reference;

   -------------------------------------------------------------------------

   procedure Subject_Channel_Exports (XML_Data : Muxml.XML_Data_Type)
   is
      Phys_Channels : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/channels/channel");
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1
      loop
         declare
            Subj_Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Comp_Name     : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Subj_Node,
                 XPath => "component",
                 Name  => "ref");
            Comp_Node     : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Name);
            Comp_Channels : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "channels/*");
         begin
            Check_Component_Resource_Mappings
              (Logical_Resources  => Comp_Channels,
               Physical_Resources => Phys_Channels,
               Resource_Type      => "channel",
               Subject            => Subj_Node);
         end;
      end loop;
   end Subject_Channel_Exports;

   -------------------------------------------------------------------------

   procedure Subject_Channel_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Match name of reference and channel.
      function Match_Channel_Name (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Ref_Channel_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
         Subj_Name        : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Muxml.Utils.Ancestor_Node
              (Node  => Node,
               Level => 2),
            Name => "name");
      begin
         return "Channel '" & Ref_Channel_Name & "' referenced by subject '"
           & Subj_Name & "' does not exist";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_Channel_Name (Left, Right : DOM.Core.Node) return Boolean
      is
         Ref_Name     : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "physical");
         Channel_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         return Ref_Name = Channel_Name;
      end Match_Channel_Name;
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject/channels/*",
         Ref_XPath    => "/system/channels/channel",
         Log_Message  => "subject channel reference(s)",
         Error        => Error_Msg'Access,
         Match        => Match_Channel_Name'Access);
   end Subject_Channel_References;

   -------------------------------------------------------------------------

   procedure Subject_Component_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Match name of reference and component.
      function Match_Component_Name
        (Left, Right : DOM.Core.Node)
         return Boolean;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Ref_Comp_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "ref");
         Subj_Name     : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
            Name => "name");
      begin
         return "Component '" & Ref_Comp_Name & "' referenced by subject '"
           & Subj_Name & "' does not exist";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_Component_Name
        (Left, Right : DOM.Core.Node)
         return Boolean
      is
         Ref_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "ref");
         Comp_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         return Ref_Name = Comp_Name;
      end Match_Component_Name;
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject/component",
         Ref_XPath    => "/system/components/component",
         Log_Message  => "subject component reference(s)",
         Error        => Error_Msg'Access,
         Match        => Match_Component_Name'Access);
   end Subject_Component_References;

   -------------------------------------------------------------------------

   procedure Subject_Monitor_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Ref_Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "subject");
         Subj_Name     : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Muxml.Utils.Ancestor_Node
              (Node  => Node,
               Level => 2),
            Name => "name");
      begin
         return "Subject '" & Ref_Subj_Name & "' referenced by subject monitor"
           & " '" & Subj_Name & "' does not exist";
      end Error_Msg;
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject/monitor/state",
         Ref_XPath    => "/system/subjects/subject",
         Log_Message  => "subject monitor reference(s)",
         Error        => Error_Msg'Access,
         Match        => Mucfgcheck.Match_Subject_Name'Access);
   end Subject_Monitor_References;

   -------------------------------------------------------------------------

   procedure Subject_Resource_Maps_Logical_Uniqueness
     (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Resource_Maps_Attr_Uniqueness
        (XML_Data => XML_Data,
         Attr     => "logical");
   end Subject_Resource_Maps_Logical_Uniqueness;

   -------------------------------------------------------------------------

   procedure Subject_Resource_Maps_Physical_Uniqueness
     (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Resource_Maps_Attr_Uniqueness
        (XML_Data => XML_Data,
         Attr     => "physical");
   end Subject_Resource_Maps_Physical_Uniqueness;

   -------------------------------------------------------------------------

   procedure Tau0_Presence_In_Scheduling (XML_Data : Muxml.XML_Data_Type)
   is
      Tau0_Node : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/scheduling/majorFrame/cpu/minorFrame"
           & "[@subject='tau0']");
   begin
      Mulog.Log
        (Msg => "Checking presence of tau0 subject in scheduling plan");
      if DOM.Core.Nodes.Length (List => Tau0_Node) = 0 then
         raise Mucfgcheck.Validation_Error with "Subject tau0 not present in "
           & "scheduling plan";
      end if;
   end Tau0_Presence_In_Scheduling;

end Cfgchecks;
