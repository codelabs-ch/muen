--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with DOM.Core.Append_Node;

with McKae.XML.XPath.XIA;

with Muxml.Utils;
with Mutools.Constants;
with Mutools.XML_Utils;

with Mulog;

with Expanders.XML_Utils;

package body Expanders.Platform
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Platform_Node : DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/platform");
      Mappings_Node : DOM.Core.Node;
   begin
      if Platform_Node = null then
         Platform_Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "platform");
         Muxml.Utils.Insert_Before
           (Parent    => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
            New_Child => Platform_Node,
            Ref_Child => "memory");
      end if;

      Mappings_Node := Muxml.Utils.Get_Element
        (Doc   => Platform_Node,
         XPath => "mappings");
      if Mappings_Node = null then
         Mappings_Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "mappings");
         Muxml.Utils.Append_Child
           (Node      => Platform_Node,
            New_Child => Mappings_Node);
      end if;

      Muxml.Utils.Add_Child
        (Parent     => Mappings_Node,
         Child_Name => "classes");
      Muxml.Utils.Add_Child
        (Parent     => Mappings_Node,
         Child_Name => "aliases",
         Ref_Names  => (1 => To_Unbounded_String ("classes")));
   end Add_Section_Skeleton;

   -------------------------------------------------------------------------

   procedure Add_Subject_Device_Resources (Data : in out Muxml.XML_Data_Type)
   is
      Phys_Devs   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device[*]");
      Subj_Devs   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device[not(*)]");
      Dev_Aliases : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/platform/mappings/aliases/alias");
      Devices : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/hardware/devices");
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Dev_Aliases) loop
         declare
            use type DOM.Core.Node;

            Alias : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Dev_Aliases,
                 Index => I - 1);
            Alias_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Alias,
                 Name => "name");
            Subj_Dev : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Subj_Devs,
                 Ref_Attr  => "physical",
                 Ref_Value => Alias_Name);
         begin
            if Subj_Dev /= null then
               declare
                  Log_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Subj_Dev,
                       Name => "logical");
                  Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Muxml.Utils.Ancestor_Node
                         (Node  => Subj_Dev,
                          Level => 2),
                       Name => "name");
                  Phys_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Alias,
                       Name => "physical");
                  Phys_Dev : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Nodes     => Phys_Devs,
                       Ref_Attr  => "name",
                       Ref_Value => Phys_Name);
                  Phys_PCI : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => Phys_Dev,
                       XPath => "pci");
                  Alias_Resources : constant DOM.Core.Node_List
                    := McKae.XML.XPath.XIA.XPath_Query
                      (N     => Alias,
                       XPath => "resource");
                  Alias_Res_Count : constant Natural
                    := DOM.Core.Nodes.Length (List => Alias_Resources);
               begin
                  Mulog.Log (Msg => "Adding resources of device alias '"
                             & Alias_Name & "' to logical device '" & Log_Name
                             & "' of subject '" & Subj_Name & "'");
                  for J in 1 .. Alias_Res_Count loop
                     declare
                        Alias_Resource : constant DOM.Core.Node
                          := DOM.Core.Nodes.Item
                            (List  => Alias_Resources,
                             Index => J - 1);
                        Alias_Res_Name : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Alias_Resource,
                             Name => "name");
                        Phys_Res_Name : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Alias_Resource,
                             Name => "physical");
                        Phys_Res : constant DOM.Core.Node
                          := Muxml.Utils.Get_Element
                            (Doc   => Phys_Dev,
                             XPath => "*[@name='" & Phys_Res_Name & "']");
                        Mmconf_Base : constant
                          := Mutools.Constants.Subject_PCI_Config_Space_Addr;
                     begin
                        Mutools.XML_Utils.Add_Resource
                          (Logical_Device         => Subj_Dev,
                           Physical_Resource      => Phys_Res,
                           Logical_Resource_Name  => Alias_Res_Name,
                           Mmconf_Devices_Node    => Devices,
                           Mmconf_Device_PCI_Node => Phys_PCI,
                           Mmconf_Virt_Base       => Mmconf_Base);
                     end;
                  end loop;
               end;
            end if;
         end;
      end loop;
   end Add_Subject_Device_Resources;

   -------------------------------------------------------------------------

   procedure Resolve_Device_Aliases (Data : in out Muxml.XML_Data_Type)
   is
      C_Dev_Maps   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/component/map");
      Subj_Devs    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device");
      Domain_Devs  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/deviceDomains/domain/devices/device");
      Dev_Aliases  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/platform/mappings/aliases/alias");
      Knl_Diag_Dev : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/kernelDiagnosticsDevice");

      Device_Refs : DOM.Core.Node_List;

      --  Returns the owner information of the given alias reference node as
      --  string.
      function Get_Owner_Info (Alias_Reference : DOM.Core.Node) return String;

      --  Resolve names of device resources of the specified device reference
      --  using the given alias node.
      procedure Resolve_Device_Resource_Names
        (Alias      : DOM.Core.Node;
         Device_Ref : DOM.Core.Node);

      ----------------------------------------------------------------------

      function Get_Owner_Info (Alias_Reference : DOM.Core.Node) return String
      is
         Element_Name : constant String
           := DOM.Core.Nodes.Node_Name (N => Alias_Reference);

         Info : Unbounded_String;
      begin
         if Element_Name = "kernelDiagnosticsDevice" then
            Info := To_Unbounded_String ("kernel diagnostics device");
         else
            declare
               Owner : constant DOM.Core.Node
                 := Muxml.Utils.Ancestor_Node
                   (Node  => Alias_Reference,
                    Level => 2);
               Owner_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Owner,
                    Name => "name");
            begin
               if DOM.Core.Nodes.Node_Name (N => Owner) = "subject" then
                  Info := To_Unbounded_String ("subject");
               else
                  Info := To_Unbounded_String ("device domain");
               end if;

               Append (Source   => Info,
                       New_Item => " '" & Owner_Name & "'");
            end;
         end if;

         return To_String (Info);
      end Get_Owner_Info;

      ----------------------------------------------------------------------

      procedure Resolve_Device_Resource_Names
        (Alias      : DOM.Core.Node;
         Device_Ref : DOM.Core.Node)
      is
         Alias_Resources : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Alias,
              XPath => "resource");
         Dev_Resources : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Device_Ref,
              XPath => "*[not(self::pci)]");
      begin
         for I in 1 .. DOM.Core.Nodes.Length (List => Dev_Resources) loop
            declare
               Dev_Res : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Dev_Resources,
                                         Index => I - 1);
               Alias_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Dev_Res,
                    Name => "physical");
               Phys_Name : constant String := Muxml.Utils.Get_Attribute
                 (Nodes     => Alias_Resources,
                  Ref_Attr  => "name",
                  Ref_Value => Alias_Name,
                  Attr_Name => "physical");
            begin
               if Phys_Name'Length > 0 then
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => Dev_Res,
                     Name  => "physical",
                     Value => Phys_Name);
               end if;
            end;
         end loop;
      end Resolve_Device_Resource_Names;
   begin
      Muxml.Utils.Append (Left  => Device_Refs,
                          Right => C_Dev_Maps);
      Muxml.Utils.Append (Left  => Device_Refs,
                          Right => Subj_Devs);
      Muxml.Utils.Append (Left  => Device_Refs,
                          Right => Domain_Devs);
      DOM.Core.Append_Node (List => Device_Refs,
                            N    => Knl_Diag_Dev);

      for I in 1 .. DOM.Core.Nodes.Length (List => Dev_Aliases) loop
         declare
            use type DOM.Core.Node;

            Alias : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Dev_Aliases,
               Index => I - 1);
            Alias_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Alias,
               Name => "name");
            Phys_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Alias,
               Name => "physical");
            Alias_Refs : constant DOM.Core.Node_List
              := Muxml.Utils.Get_Elements
                (Nodes     => Device_Refs,
                 Ref_Attr  => "physical",
                 Ref_Value => Alias_Name);
         begin
            for J in 1 .. DOM.Core.Nodes.Length (List => Alias_Refs) loop
               declare
                  Alias_Ref : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Alias_Refs,
                       Index => J - 1);
                  Owner_Info : constant String
                    := Get_Owner_Info (Alias_Reference => Alias_Ref);
               begin
                  Mulog.Log (Msg => "Resolving device alias reference '"
                             & Alias_Name & "' of " & Owner_Info
                             & " to physical name '" & Phys_Name & "'");
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => Alias_Ref,
                     Name  => "physical",
                     Value => Phys_Name);
                  Resolve_Device_Resource_Names
                    (Alias      => Alias,
                     Device_Ref => Alias_Ref);
               end;
            end loop;
         end;
      end loop;
   end Resolve_Device_Aliases;

   -------------------------------------------------------------------------

   procedure Resolve_Device_Classes (Data : in out Muxml.XML_Data_Type)
   is
      Subj_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device[not(*)]");
      Domain_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/deviceDomains/domain/devices/device");
      Dev_Classes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/platform/mappings/classes/class");

      Device_Refs : DOM.Core.Node_List;

      --  Add a device reference for each device in the given device class.
      procedure Add_Class_Device_References
        (Class      : DOM.Core.Node;
         Device_Ref : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Add_Class_Device_References
        (Class      : DOM.Core.Node;
         Device_Ref : DOM.Core.Node)
      is
         Class_Name    : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Class,
              Name => "name");
         Class_Devs    : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Class,
              XPath => "device");
         Subj_Dev_Node : constant DOM.Core.Node
           := DOM.Core.Nodes.Parent_Node (N => Device_Ref);
      begin
         for I in 1 .. DOM.Core.Nodes.Length (List => Class_Devs) loop
            declare
               Class_Dev : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Class_Devs,
                    Index => I - 1);
               Phys_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Class_Dev,
                    Name => "physical");
               Log_Name : constant String := Class_Name & "_"
                 & Ada.Strings.Fixed.Trim (Source => I'Img,
                                           Side   => Ada.Strings.Left);
            begin
               Muxml.Utils.Append_Child
                 (Node      => Subj_Dev_Node,
                  New_Child => XML_Utils.Create_Logical_Device_Node
                    (Policy        => Data,
                     Logical_Name  => Log_Name,
                     Physical_Name => Phys_Name));
            end;
         end loop;
      end Add_Class_Device_References;

      Nodes_To_Free : DOM.Core.Node_List;
   begin
      Muxml.Utils.Append (Left  => Device_Refs,
                          Right => Subj_Devs);
      Muxml.Utils.Append (Left  => Device_Refs,
                          Right => Domain_Devs);

      for I in 1 .. DOM.Core.Nodes.Length (List => Dev_Classes) loop
         declare
            use type DOM.Core.Node;

            Class : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Dev_Classes,
               Index => I - 1);
            Class_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Class,
               Name => "name");
            Class_Refs : constant DOM.Core.Node_List
              := Muxml.Utils.Get_Elements
                (Nodes     => Device_Refs,
                 Ref_Attr  => "physical",
                 Ref_Value => Class_Name);
         begin
            for J in 1 .. DOM.Core.Nodes.Length (List => Class_Refs) loop
               declare
                  Class_Ref : DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Class_Refs,
                       Index => J - 1);
                  Owner : constant DOM.Core.Node
                    := Muxml.Utils.Ancestor_Node
                      (Node  => Class_Ref,
                       Level => 2);
                  Owner_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Owner,
                       Name => "name");
                  Is_Subj : constant Boolean
                    := DOM.Core.Nodes.Node_Name (N => Owner) = "subject";
               begin
                  Mulog.Log (Msg => "Resolving device class reference '"
                             & Class_Name & "' of "
                             & (if Is_Subj then "subject" else "device domain")
                             & " '" & Owner_Name & "'");
                  Add_Class_Device_References
                    (Class      => Class,
                     Device_Ref => Class_Ref);

                  Class_Ref := DOM.Core.Nodes.Remove_Child
                    (N         => DOM.Core.Nodes.Parent_Node (N => Class_Ref),
                     Old_Child => Class_Ref);
                  DOM.Core.Append_Node (List => Nodes_To_Free,
                                        N    => Class_Ref);
               end;
            end loop;
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes_To_Free) - 1 loop
         declare
            Node : DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes_To_Free,
                                      Index => I);
         begin
            DOM.Core.Nodes.Free (N => Node);
         end;
      end loop;
   end Resolve_Device_Classes;

end Expanders.Platform;
