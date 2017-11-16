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

with Ada.Exceptions;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded.Hash;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mucfgcheck;
with Mutools.Match;
with Mutools.Utils;

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

   procedure No_Check
     (Logical_Resource  : DOM.Core.Node;
      Physical_Resource : DOM.Core.Node;
      Mapping           : DOM.Core.Node) is null;

   --  Check subject mappings of given logical component resources against
   --  specified physical resources. The specified additional check is invoked
   --  after the basic checks are successful. By default no additional checks
   --  are performed.
   procedure Check_Component_Resource_Mappings
     (Logical_Resources  : DOM.Core.Node_List;
      Physical_Resources : DOM.Core.Node_List;
      Resource_Type      : String;
      Subject            : DOM.Core.Node;
      Additional_Check   : not null access procedure
        (Logical_Resource  : DOM.Core.Node;
         Physical_Resource : DOM.Core.Node;
         Mapping           : DOM.Core.Node) := No_Check'Access);

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

   --  Checks the uniqueness of the specified attribute for all given nodes.
   --  The specified description is used in exception and log messages.
   procedure Check_Attribute_Uniqueness
     (Nodes       : DOM.Core.Node_List;
      Attr_Name   : String;
      Description : String);

   --  Returns True if the left node's 'ref' attribute matches the 'name'
   --  attribute of the right node.
   function Match_Ref_Name (Left, Right : DOM.Core.Node) return Boolean;

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

   procedure Check_Attribute_Uniqueness
     (Nodes       : DOM.Core.Node_List;
      Attr_Name   : String;
      Description : String)
   is
      --  Check inequality of desired node attributes.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Attr  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => Attr_Name);
         Right_Attr : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => Attr_Name);
      begin
         if Left_Attr = Right_Attr then
            raise Mucfgcheck.Validation_Error with Mutools.Utils.Capitalize
              (Description) & " " & Attr_Name & " '" & Left_Attr
              & "' is not unique";
         end if;
      end Check_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " " & Description & " "
                 & Attr_Name & "(s)");
      Mucfgcheck.Compare_All (Nodes      => Nodes,
                              Comparator => Check_Inequality'Access);
   end Check_Attribute_Uniqueness;

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
      Subject            : DOM.Core.Node;
      Additional_Check   : not null access procedure
        (Logical_Resource  : DOM.Core.Node;
         Physical_Resource : DOM.Core.Node;
         Mapping           : DOM.Core.Node) := No_Check'Access)

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

            Log_Res  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Logical_Resources,
                 Index => I);
            Log_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Log_Res,
                 Name => "logical");
            Mapping  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Mappings,
                 Ref_Attr  => "logical",
                 Ref_Value => Log_Name);
         begin
            if Mapping = null then
               raise Mucfgcheck.Validation_Error with "Subject '" & Subj_Name
                 & "' does not map logical " & Resource_Type & " '" & Log_Name
                 & "' as requested by referenced component '"& Comp_Name
                 & "'";
            end if;

            declare
               Phys_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Mapping,
                    Name => "physical");
               Phys_Res  : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Physical_Resources,
                    Ref_Attr  => "name",
                    Ref_Value => Phys_Name);
            begin
               if Phys_Res = null then
                  raise Mucfgcheck.Validation_Error with "Physical "
                    & Resource_Type & " '" & Phys_Name & "' referenced by "
                    & "mapping of component logical resource '" & Log_Name
                    & "' by subject" & " '" & Subj_Name & "' does not exist";
               end if;

               Additional_Check (Logical_Resource  => Log_Res,
                                 Physical_Resource => Phys_Res,
                                 Mapping           => Mapping);
            end;
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
                 XPath => "requires/channels/*");
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
                 XPath => "requires/channels/*[self::reader or self::writer]");
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

   procedure Component_Device_IO_Port_Range (XML_Data : Muxml.XML_Data_Type)
   is
      Components   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");
      Phys_Devices : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device");
      Subjects     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node    : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Comp_Name    : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Subj_Node,
                 XPath => "component",
                 Name  => "ref");
            Comp_Node    : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Name);
            Comp_Devices : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "devices/device");
            Dev_Count    : constant Natural
              := DOM.Core.Nodes.Length (Comp_Devices);

            --  Check equality of logical and physical device I/O port range.
            procedure Check_Dev_IO_Port_Range
              (Logical_Dev  : DOM.Core.Node;
               Physical_Dev : DOM.Core.Node);

            ----------------------------------------------------------------

            procedure Check_Dev_IO_Port_Range
              (Logical_Dev  : DOM.Core.Node;
               Physical_Dev : DOM.Core.Node)
            is
               Log_Dev_Name   : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Dev,
                    Name => "logical");
               Log_Dev_Ports  : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Logical_Dev,
                    XPath => "ioPort");
               Phys_Dev_Ports : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Physical_Dev,
                    XPath => "ioPort");
               Phys_Dev_Name  : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Physical_Dev,
                    Name => "name");
               Mappings       : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Subj_Node,
                    XPath => "component/map[@logical='" & Log_Dev_Name
                    & "']/map");
            begin
               for I in 0 .. DOM.Core.Nodes.Length (List => Log_Dev_Ports) - 1
               loop
                  declare
                     use type Interfaces.Unsigned_64;

                     Log_Dev_Port        : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                         (List  => Log_Dev_Ports,
                          Index => I);
                     Log_Dev_Port_Name   : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Dev_Port,
                          Name => "logical");
                     Log_Dev_Port_Start  : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Dev_Port,
                          Name => "start");
                     Log_Dev_Port_End    : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Dev_Port,
                          Name => "end");
                     Phys_Dev_Port_Name  : constant String
                       := Muxml.Utils.Get_Attribute
                         (Nodes     => Mappings,
                          Ref_Attr  => "logical",
                          Ref_Value => Log_Dev_Port_Name,
                          Attr_Name => "physical");
                     Phys_Dev_Port       : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Nodes     => Phys_Dev_Ports,
                          Ref_Attr  => "name",
                          Ref_Value => Phys_Dev_Port_Name);
                     Phys_Dev_Port_Start : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Dev_Port,
                          Name => "start");
                     Phys_Dev_Port_End   : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Dev_Port,
                          Name => "end");
                  begin
                     if Interfaces.Unsigned_64'Value (Log_Dev_Port_Start)
                       /= Interfaces.Unsigned_64'Value (Phys_Dev_Port_Start)
                       or Interfaces.Unsigned_64'Value (Log_Dev_Port_End)
                       /= Interfaces.Unsigned_64'Value (Phys_Dev_Port_End)
                     then
                        raise Mucfgcheck.Validation_Error with "Component '"
                          & Comp_Name & "' referenced by subject '" & Subj_Name
                          & "' requests I/O range " & Log_Dev_Port_Start
                          & ".." & Log_Dev_Port_End & " for '" & Log_Dev_Name
                          & "->" & Log_Dev_Port_Name
                          & "' but physical device '" & Phys_Dev_Name & "->"
                          & Phys_Dev_Port_Name & "' " & "has "
                          & Phys_Dev_Port_Start & ".." & Phys_Dev_Port_End;
                     end if;
                  end;
               end loop;
            end Check_Dev_IO_Port_Range;
         begin
            if Dev_Count > 0 then
               Mulog.Log (Msg => "Checking I/O port ranges of" & Dev_Count'Img
                          & " component '" & Comp_Name & "' device(s) "
                          & "referenced by subject '" & Subj_Name & "'");

               Check_Component_Resources
                 (Logical_Resources  => Comp_Devices,
                  Physical_Resources => Phys_Devices,
                  Subject            => Subj_Node,
                  Check_Resource     => Check_Dev_IO_Port_Range'Access);
            end if;
         end;
      end loop;
   end Component_Device_IO_Port_Range;

   -------------------------------------------------------------------------

   procedure Component_Device_Memory_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Components   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");
      Phys_Devices : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device");
      Subjects     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node    : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Comp_Name    : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Subj_Node,
                 XPath => "component",
                 Name  => "ref");
            Comp_Node    : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Name);
            Comp_Devices : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "devices/device");
            Dev_Count    : constant Natural
              := DOM.Core.Nodes.Length (Comp_Devices);

            --  Check equality of logical and physical device memory size.
            procedure Check_Dev_Mem_Size
              (Logical_Dev  : DOM.Core.Node;
               Physical_Dev : DOM.Core.Node);

            ----------------------------------------------------------------

            procedure Check_Dev_Mem_Size
              (Logical_Dev  : DOM.Core.Node;
               Physical_Dev : DOM.Core.Node)
            is
               Log_Dev_Name  : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Dev,
                    Name => "logical");
               Log_Dev_Mem   : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Logical_Dev,
                    XPath => "memory");
               Phys_Dev_Mem  : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Physical_Dev,
                    XPath => "memory");
               Phys_Dev_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Physical_Dev,
                    Name => "name");
               Mappings      : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Subj_Node,
                    XPath => "component/map[@logical='" & Log_Dev_Name
                    & "']/map");
            begin
               for I in 0 .. DOM.Core.Nodes.Length (List => Log_Dev_Mem) - 1
               loop
                  declare
                     use type Interfaces.Unsigned_64;

                     Log_Dev_Memory    : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item
                         (List  => Log_Dev_Mem,
                          Index => I);
                     Log_Dev_Mem_Name  : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Dev_Memory,
                          Name => "logical");
                     Log_Dev_Mem_Size  : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Dev_Memory,
                          Name => "size");
                     Phys_Dev_Mem_Name : constant String
                       := Muxml.Utils.Get_Attribute
                         (Nodes     => Mappings,
                          Ref_Attr  => "logical",
                          Ref_Value => Log_Dev_Mem_Name,
                          Attr_Name => "physical");
                     Phys_Dev_Mem_Size : constant String
                       := Muxml.Utils.Get_Attribute
                         (Nodes     => Phys_Dev_Mem,
                          Ref_Attr  => "name",
                          Ref_Value => Phys_Dev_Mem_Name,
                          Attr_Name => "size");
                  begin
                     if Interfaces.Unsigned_64'Value (Log_Dev_Mem_Size)
                       /= Interfaces.Unsigned_64'Value (Phys_Dev_Mem_Size)
                     then
                        raise Mucfgcheck.Validation_Error with "Component '"
                          & Comp_Name & "' referenced by subject '" & Subj_Name
                          & "' requests size " & Log_Dev_Mem_Size & " for "
                          & "logical device memory '" & Log_Dev_Name & "->"
                          & Log_Dev_Mem_Name & "' but linked physical device"
                          & " memory '" & Phys_Dev_Name & "->"
                          & Phys_Dev_Mem_Name & "' " & "has size "
                          & Phys_Dev_Mem_Size;
                     end if;
                  end;
               end loop;
            end Check_Dev_Mem_Size;
         begin
            if Dev_Count > 0 then
               Mulog.Log (Msg => "Checking memory size of" & Dev_Count'Img
                          & " component '" & Comp_Name & "' device(s) "
                          & "referenced by subject '" & Subj_Name & "'");

               Check_Component_Resources
                 (Logical_Resources  => Comp_Devices,
                  Physical_Resources => Phys_Devices,
                  Subject            => Subj_Node,
                  Check_Resource     => Check_Dev_Mem_Size'Access);
            end if;
         end;
      end loop;
   end Component_Device_Memory_Size;

   -------------------------------------------------------------------------

   procedure Component_Library_Cyclic_References
     (XML_Data : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;

      Libraries  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/library");
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/*[depends/library]");

      Count : constant Natural := DOM.Core.Nodes.Length (List => Components);

      package SOCN is new Ada.Containers.Hashed_Sets
        (Element_Type        => Unbounded_String,
         Hash                => Ada.Strings.Unbounded.Hash,
         Equivalent_Elements => Ada.Strings.Unbounded."=");

      Active_Nodes : SOCN.Set;

      --  Recursively resolve dependencies of given component/library node.
      procedure Resolve_Depends (Node : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Resolve_Depends (Node : DOM.Core.Node)
      is
         use type DOM.Core.Node;

         function U
           (Source : String)
            return Unbounded_String
            renames To_Unbounded_String;

         Name : constant String
           := DOM.Core.Elements.Get_Attribute (Elem => Node,
                                               Name => "name");
         Deps_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element (Doc   => Node,
                                       XPath => "depends");
         Deps : DOM.Core.Node_List;
      begin
         if Deps_Node = null then
            return;
         end if;

         if Active_Nodes.Contains (Item => U (Source => Name)) then
            raise Mucfgcheck.Validation_Error with Name;
         end if;

         Active_Nodes.Insert (New_Item => U (Source => Name));
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
               Lib_Node : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element (Nodes     => Libraries,
                                             Ref_Attr  => "name",
                                             Ref_Value => Dep_Name);
            begin
               Resolve_Depends (Node => Lib_Node);

            exception
               when E : Mucfgcheck.Validation_Error =>
                  raise Mucfgcheck.Validation_Error with Name & "->" &
                    Ada.Exceptions.Exception_Message (X => E);
            end;
         end loop;
         Active_Nodes.Delete (Item => U (Source => Name));
      end Resolve_Depends;
   begin
      Mulog.Log (Msg => "Checking cyclic dependencies of " & Count'Img
                 & " component(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Components) - 1 loop
         declare
            Comp_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Components,
                 Index => I);
         begin
            Resolve_Depends (Node => Comp_Node);
         end;
      end loop;

   exception
      when E : Mucfgcheck.Validation_Error =>
         raise Mucfgcheck.Validation_Error with
           "Cyclic component dependency detected: "
           & Ada.Exceptions.Exception_Message (X => E);
   end Component_Library_Cyclic_References;

   -------------------------------------------------------------------------

   procedure Component_Library_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Ref_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "ref");
         Comp_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Muxml.Utils.Ancestor_Node
              (Node  => Node,
               Level => 2),
            Name => "name");
      begin
         return "Library '" & Ref_Name & "' referenced by component '"
           & Comp_Name & "' does not exist";
      end Error_Msg;
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/components/*/depends/library",
         Ref_XPath    => "/system/components/library",
         Log_Message  => "component library reference(s)",
         Error        => Error_Msg'Access,
         Match        => Match_Ref_Name'Access);
   end Component_Library_References;

   -------------------------------------------------------------------------

   procedure Component_Memory_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Components  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");
      Phys_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
      Subjects    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Comp_Name   : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Subj_Node,
                 XPath => "component",
                 Name  => "ref");
            Comp_Node   : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Name);
            Comp_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "requires/memory/memory");
            Mem_Count   : constant Natural
              := DOM.Core.Nodes.Length (Comp_Memory);

            --  Check equality of logical and physical memory size.
            procedure Check_Mem_Size
              (Logical_Resource  : DOM.Core.Node;
               Physical_Resource : DOM.Core.Node);

            ----------------------------------------------------------------

            procedure Check_Mem_Size
              (Logical_Resource  : DOM.Core.Node;
               Physical_Resource : DOM.Core.Node)
            is
               use type Interfaces.Unsigned_64;

               Log_Mem_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Resource,
                    Name => "logical");
               Log_Mem_Size : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Resource,
                    Name => "size");
               Phys_Mem_Size : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Physical_Resource,
                    Name => "size");
               Phys_Mem_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Physical_Resource,
                    Name => "name");
            begin
               if Interfaces.Unsigned_64'Value (Log_Mem_Size)
                 /= Interfaces.Unsigned_64'Value (Phys_Mem_Size)
               then
                  raise Mucfgcheck.Validation_Error with "Component '"
                    & Comp_Name & "' referenced by subject '" & Subj_Name
                    & "' requests size " & Log_Mem_Size & " for logical "
                    & "memory '" & Log_Mem_Name & "' but linked physical "
                    & "memory region '" & Phys_Mem_Name & "' " & "has size "
                    & Phys_Mem_Size;
               end if;
            end Check_Mem_Size;
         begin
            if Mem_Count > 0 then
               Mulog.Log (Msg => "Checking size of" & Mem_Count'Img
                          & " component '" & Comp_Name & "' memory region(s) "
                          & "referenced by subject '" & Subj_Name & "'");

               Check_Component_Resources
                 (Logical_Resources  => Comp_Memory,
                  Physical_Resources => Phys_Memory,
                  Subject            => Subj_Node,
                  Check_Resource     => Check_Mem_Size'Access);
            end if;
         end;
      end loop;
   end Component_Memory_Size;

   -------------------------------------------------------------------------

   procedure Component_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");
   begin
      Check_Attribute_Uniqueness
        (Nodes       => Nodes,
         Attr_Name   => "name",
         Description => "component");
   end Component_Name_Uniqueness;

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
      Mappings : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/deviceDomains/domain/devices/device"
           & "[@mapReservedMemory='true']");
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
                    := Muxml.Utils.Ancestor_Node
                      (Node  => Muxml.Utils.Get_Element
                         (Nodes     => Mappings,
                          Ref_Attr  => "physical",
                          Ref_Value => Dev_Name),
                       Level => 2);
               begin
                  if Ref_Domain /= null then

                     --  Device is actually assigned to device domain and maps
                     --  RMRR.

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
   begin
      Check_Attribute_Uniqueness
        (Nodes       => Nodes,
         Attr_Name   => "name",
         Description => "reserved memory region");
   end Hardware_Reserved_Memory_Region_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Hardware_Reserved_Memory_Region_References
     (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

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
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/hardware/devices/device/reservedMemory",
         Ref_XPath    => "/system/hardware/memory/reservedMemory",
         Log_Message  => "reserved memory region reference(s)",
         Error        => Error_Msg'Access,
         Match        => Match_Ref_Name'Access);
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

   procedure Library_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/library");
   begin
      Check_Attribute_Uniqueness
        (Nodes       => Nodes,
         Attr_Name   => "name",
         Description => "library");
   end Library_Name_Uniqueness;

   -------------------------------------------------------------------------

   function Match_Ref_Name (Left, Right : DOM.Core.Node) return Boolean
   is
      Ref  : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "ref");
      Name : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "name");
   begin
      return Ref = Name;
   end Match_Ref_Name;

   ----------------------------------------------------------------------

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
                 XPath => "channels//*[self::reader or self::writer]");
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
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject/channels/*",
         Ref_XPath    => "/system/channels/channel",
         Log_Message  => "subject channel reference(s)",
         Error        => Error_Msg'Access,
         Match        => Mutools.Match.Is_Valid_Reference'Access);
   end Subject_Channel_References;

   -------------------------------------------------------------------------

   procedure Subject_Component_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

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
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject/component",
         Ref_XPath    => "/system/components/component",
         Log_Message  => "subject component reference(s)",
         Error        => Error_Msg'Access,
         Match        => Match_Ref_Name'Access);
   end Subject_Component_References;

   -------------------------------------------------------------------------

   procedure Subject_Device_Exports (XML_Data : Muxml.XML_Data_Type)
   is
      Phys_Devices : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device");
      Components   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");
      Subjects     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node    : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Comp_Name    : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Subj_Node,
                 XPath => "component",
                 Name  => "ref");
            Comp_Node    : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Name);
            Comp_Devices : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "devices/device");

            --  Check that all logical device resources are mapped to physical
            --  device resources of the same type.
            procedure Check_Device_Resource_Mappings
              (Logical_Device  : DOM.Core.Node;
               Physical_Device : DOM.Core.Node;
               Mapping         : DOM.Core.Node);

            ----------------------------------------------------------------

            procedure Check_Device_Resource_Mappings
              (Logical_Device  : DOM.Core.Node;
               Physical_Device : DOM.Core.Node;
               Mapping         : DOM.Core.Node)
            is
               Subj_Name     : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Muxml.Utils.Ancestor_Node
                      (Node  => Mapping,
                       Level => 2),
                    Name => "name");
               Log_Dev_Name  : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Device,
                    Name => "logical");
               Phys_Dev_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Physical_Device,
                    Name => "name");
               Res_Mappings  : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Mapping,
                    XPath => "*");
               Log_Dev_Res   : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Logical_Device,
                    XPath => "*");
               Phys_Dev_Res  : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Physical_Device,
                    XPath => "*");
            begin
               for I in 0 .. DOM.Core.Nodes.Length (List => Log_Dev_Res) - 1
               loop
                  declare
                     use type DOM.Core.Node;

                     Log_Res : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item (List  => Log_Dev_Res,
                                               Index => I);
                     Log_Res_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Res,
                          Name => "logical");
                     Phys_Res_Name : constant String
                       := Muxml.Utils.Get_Attribute
                         (Nodes     => Res_Mappings,
                          Ref_Attr  => "logical",
                          Ref_Value => Log_Res_Name,
                          Attr_Name => "physical");
                     Phys_Res : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Nodes     => Phys_Dev_Res,
                          Ref_Attr  => "name",
                          Ref_Value => Phys_Res_Name);
                  begin
                     if Phys_Res_Name'Length = 0 then
                        raise Mucfgcheck.Validation_Error with "Subject '"
                          & Subj_Name & "' does not map logical device "
                          & "resource '" & Log_Dev_Name & "->" & Log_Res_Name
                          & "' as requested by referenced component '"
                          & Comp_Name & "'";
                     end if;

                     if Phys_Res = null then
                        raise Mucfgcheck.Validation_Error with "Physical "
                          & "device resource '" & Phys_Dev_Name & "->"
                          & Phys_Res_Name & "' referenced by "
                          & "mapping of component logical resource '"
                          & Log_Dev_Name & "->" & Log_Res_Name
                          & "' by subject" & " '" & Subj_Name
                          & "' does not exist";
                     end if;

                     if DOM.Core.Nodes.Node_Name (N => Log_Res)
                       /= DOM.Core.Nodes.Node_Name (N => Phys_Res)
                     then
                        raise Mucfgcheck.Validation_Error with "Physical "
                          & "device resource '" & Phys_Dev_Name & "->"
                          & Phys_Res_Name & "' and component logical resource"
                          & " '" & Log_Dev_Name & "->" & Log_Res_Name
                          & "' mapped by subject" & " '" & Subj_Name
                          & "' have different type";
                     end if;
                  end;
               end loop;
            end Check_Device_Resource_Mappings;
         begin
            Check_Component_Resource_Mappings
              (Logical_Resources  => Comp_Devices,
               Physical_Resources => Phys_Devices,
               Resource_Type      => "device",
               Subject            => Subj_Node,
               Additional_Check   => Check_Device_Resource_Mappings'Access);
         end;
      end loop;
   end Subject_Device_Exports;

   -------------------------------------------------------------------------

   procedure Subject_Memory_Exports (XML_Data : Muxml.XML_Data_Type)
   is
      Phys_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Comp_Name  : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Subj_Node,
                 XPath => "component",
                 Name  => "ref");
            Comp_Node  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Name);
            Comp_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "memory//memory");
         begin
            Check_Component_Resource_Mappings
              (Logical_Resources  => Comp_Memory,
               Physical_Resources => Phys_Memory,
               Resource_Type      => "memory region",
               Subject            => Subj_Node);
         end;
      end loop;
   end Subject_Memory_Exports;

   -------------------------------------------------------------------------

   procedure Subject_Monitor_Loader_Addresses (XML_Data : Muxml.XML_Data_Type)
   is
      subtype Valid_Address_Range is Interfaces.Unsigned_64 range
        16#1_0000_0000# .. 16#6fff_ffff_ffff#;

      Loader_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/monitor/loader");
      Loader_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Loader_Nodes);
   begin
      if Loader_Count = 0 then
         return;
      end if;

      Mulog.Log (Msg => "Checking range of" & Loader_Count'Img
                 & " loader virtual addresse(s)");

      for I in Natural range 0 .. Loader_Count - 1 loop
         declare
            Ldr_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Loader_Nodes,
                                      Index => I);
            Virt_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute (Elem => Ldr_Node,
                                                  Name => "virtualAddress"));
         begin
            if Virt_Addr not in Valid_Address_Range then
               declare
                  Ldr_Logical  : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Ldr_Node,
                       Name => "logical");
                  Subject_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Muxml.Utils.Ancestor_Node (Node  => Ldr_Node,
                                                          Level => 2),
                       Name => "name");
               begin
                  raise Mucfgcheck.Validation_Error with "Loader mapping '"
                    & Ldr_Logical & "' of subject '" & Subject_Name & "' not "
                    & "in valid range " & Mutools.Utils.To_Hex
                    (Number => Valid_Address_Range'First)
                    & " .. " & Mutools.Utils.To_Hex
                    (Number => Valid_Address_Range'Last);
               end;
            end if;
         end;
      end loop;
   end Subject_Monitor_Loader_Addresses;

   -------------------------------------------------------------------------

   procedure Subject_Monitor_Loader_States
     (XML_Data : Muxml.XML_Data_Type)
   is
      Ldr_Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject[monitor/loader]");
      Ldr_Subj_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Ldr_Subjects);
   begin
      if Ldr_Subj_Count = 0 then
         return;
      end if;

      Mulog.Log (Msg => "Checking" & Ldr_Subj_Count'Img
                 & " loader monitor state(s)");

      for I in 0 .. Ldr_Subj_Count - 1 loop
         declare
            Ldr_Subj    : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Ldr_Subjects,
                                      Index => I);
            Ldr_Nodes   : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Ldr_Subj,
                 XPath => "monitor/loader");
            State_Nodes : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Ldr_Subj,
                 XPath => "monitor/state");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Ldr_Nodes) - 1 loop
               declare
                  use type DOM.Core.Node;

                  Ldr_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Ldr_Nodes,
                                            Index => J);
                  Loadee_Name : constant String
                    := DOM.Core.Elements.Get_Attribute (Elem => Ldr_Node,
                                                        Name => "subject");
                  State_Node : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Nodes     => State_Nodes,
                       Ref_Attr  => "subject",
                       Ref_Value => Loadee_Name);
                  Ref_Logical_Name : constant String
                    := "monitor_state_" & Loadee_Name;
               begin
                  if State_Node = null then
                     declare
                        Ldr_Name : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Ldr_Subj,
                             Name => "name");
                     begin
                        raise Mucfgcheck.Validation_Error with "No monitor "
                          & "state for subject '" & Loadee_Name & "' required"
                          & " by loader subject '" & Ldr_Name & "'";
                     end;
                  end if;

                  if DOM.Core.Elements.Get_Attribute
                    (Elem => State_Node,
                     Name => "logical") /= Ref_Logical_Name
                  then
                     declare
                        Ldr_Name : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Ldr_Subj,
                             Name => "name");
                     begin
                        raise Mucfgcheck.Validation_Error with "Monitor "
                          & "state for subject '" & Loadee_Name & "' of loader"
                          & " subject '" & Ldr_Name & "' has invalid logical"
                          & " name '" & DOM.Core.Elements.Get_Attribute
                          (Elem => State_Node,
                           Name => "logical")
                          & "', must be '" & Ref_Logical_Name & "'";
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Subject_Monitor_Loader_States;

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
         Source_XPath => "/system/subjects/subject/monitor/*[@subject]",
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
