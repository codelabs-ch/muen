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

with McKae.XML.XPath.XIA;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with Mulog;
with Mutools.Match;
with Muxml.Utils;

package body Mucfgcheck.Platform
is

   -------------------------------------------------------------------------

   procedure Alias_Physical_Device_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Alias_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "name");
         Phys_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
      begin
         return "Physical device '" & Phys_Name & "' referenced by device "
           & "alias '" & Alias_Name & "' not found";
      end Error_Msg;
   begin
      For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/platform/mappings/aliases/alias",
         Ref_XPath    => "/system/hardware/devices/device",
         Log_Message  => "alias device reference(s)",
         Error        => Error_Msg'Access,
         Match        => Mutools.Match.Is_Valid_Reference'Access);
   end Alias_Physical_Device_References;

   -------------------------------------------------------------------------

   procedure Alias_Physical_Device_Resource_References
     (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Alias_Name     : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
            Name => "name");
         Alias_Res_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "name");
         Phys_Res_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
      begin
         return "Physical device resource '" & Phys_Res_Name
           & "' referenced by alias resource '" & Alias_Res_Name
           & "' of device alias '" & Alias_Name & "' not found";
      end Error_Msg;
   begin
      For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/platform/mappings/aliases/alias/resource",
         Ref_XPath    => "/system/hardware/devices/device/*",
         Log_Message  => "alias device resource reference(s)",
         Error        => Error_Msg'Access,
         Match        => Mutools.Match.Is_Valid_Resource_Ref'Access);
   end Alias_Physical_Device_Resource_References;

   -------------------------------------------------------------------------

   procedure Class_Physical_Device_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Class_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
            Name => "name");
         Phys_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
      begin
         return "Physical device '" & Phys_Name & "' referenced by device "
           & "class '" & Class_Name & "' not found";
      end Error_Msg;
   begin
      For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/platform/mappings/classes/class/device",
         Ref_XPath    => "/system/hardware/devices/device",
         Log_Message  => "class device reference(s)",
         Error        => Error_Msg'Access,
         Match        => Mutools.Match.Is_Valid_Reference'Access);
   end Class_Physical_Device_References;

   -------------------------------------------------------------------------

   procedure Kernel_Diagnostics_Device_Reference
     (XML_Data : Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Phys_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device");
      Phys_Name : constant String := Muxml.Utils.Get_Attribute
        (Doc   => XML_Data.Doc,
         XPath => "/system/platform/kernelDiagnostics/device",
         Name  => "physical");
   begin
      if Phys_Name'Length > 0 and then
        Muxml.Utils.Get_Element
          (Nodes     => Phys_Devs,
           Ref_Attr  => "name",
           Ref_Value => Phys_Name) = null
      then
         raise Validation_Error with "Physical device '" & Phys_Name
           & "' designated as kernel diagnostics device not found";
      end if;
   end Kernel_Diagnostics_Device_Reference;

   -------------------------------------------------------------------------

   procedure Subject_Alias_Resource_References (XML_Data : Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Subj_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject/devices/"
           & "device[memory or ioPort or irq]");
      Aliases : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/platform/mappings/aliases/alias");
      Aliases_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Aliases);

      --  Check that the resources referenced by the specified logical device
      --  are provided by the given device alias.
      procedure Check_Alias_Resources
        (Device_Alias   : DOM.Core.Node;
         Logical_Device : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Alias_Resources
        (Device_Alias   : DOM.Core.Node;
         Logical_Device : DOM.Core.Node)
      is
         Alias_Resources : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Device_Alias,
              XPath => "resource");
         Logical_Resources : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Logical_Device,
              XPath => "*[not(self::pci)]");
      begin
         for I in 1 .. DOM.Core.Nodes.Length (List => Logical_Resources) loop
            declare
               Logical_Res : constant DOM.Core.Node := DOM.Core.Nodes.Item
                 (List  => Logical_Resources,
                  Index => I - 1);
               Res_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Res,
                    Name => "physical");
               Alias_Res : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Alias_Resources,
                    Ref_Attr  => "name",
                    Ref_Value => Res_Name);
            begin
               if Alias_Res = null then
                  declare
                     Alias_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Device_Alias,
                          Name => "name");
                     Logical_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Logical_Device,
                          Name => "logical");
                     Subject_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Muxml.Utils.Ancestor_Node
                            (Node  => Logical_Device,
                             Level => 2),
                          Name => "name");
                  begin
                     raise Validation_Error with "Logical device '"
                       & Logical_Name & "' of subject '" & Subject_Name
                       & "' references resource '" & Res_Name & "' that is not"
                       & " provided by device alias '" & Alias_Name & "'";
                  end;
               end if;
            end;
         end loop;
      end Check_Alias_Resources;
   begin
      if Aliases_Count = 0 then
         return;
      end if;

      Mulog.Log (Msg => "Checking subject resource references of"
                 & Aliases_Count'Img & " aliase(s)");
      for I in 1 .. Aliases_Count loop
         declare
            Alias : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Aliases,
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
               Check_Alias_Resources
                 (Device_Alias   => Alias,
                  Logical_Device => Subj_Dev);
            end if;
         end;
      end loop;
   end Subject_Alias_Resource_References;

end Mucfgcheck.Platform;
