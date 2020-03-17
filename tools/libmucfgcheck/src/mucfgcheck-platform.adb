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
with Mutools.Types;
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
      Diag_Dev  : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => XML_Data.Doc,
         XPath => "/system/platform/kernelDiagnostics/device");
      Phys_Name : constant String
        := (if Diag_Dev = null then ""
            else DOM.Core.Elements.Get_Attribute
              (Elem => Diag_Dev,
               Name => "physical"));
      Phys_Dev  : constant DOM.Core.Node
        := Muxml.Utils.Get_Element (Nodes     => Phys_Devs,
                                    Ref_Attr  => "name",
                                    Ref_Value => Phys_Name);
   begin
      if Diag_Dev = null then
         return;
      end if;

      Mulog.Log (Msg => "Checking kernel diagnostics device references");

      if Phys_Dev = null then
         raise Validation_Error with "Physical device '" & Phys_Name
           & "' designated as kernel diagnostics device not found";
      end if;

      declare
         Diag_Resources : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Diag_Dev,
              XPath => "*");
         Phys_Resources : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Phys_Dev,
              XPath => "*");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Diag_Resources) - 1 loop
            declare
               Diag_Res : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Diag_Resources,
                                         Index => I);
               Diag_Res_Type : constant String
                 := DOM.Core.Nodes.Node_Name (N => Diag_Res);
               Phys_Res_Name : constant String
                 := DOM.Core.Elements.Get_Attribute (Elem => Diag_Res,
                                                     Name => "physical");
               Phys_Res : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element (Nodes     => Phys_Resources,
                                             Ref_Attr  => "name",
                                             Ref_Value => Phys_Res_Name);
            begin
               if Phys_Res = null then
                  raise Validation_Error with "Physical device resource '"
                    & Phys_Name & "->" & Phys_Res_Name & "' referenced by "
                    & "kernel diagnostics device not found";
               end if;

               if Diag_Res_Type /= DOM.Core.Nodes.Node_Name (N => Phys_Res)
               then
                  raise Validation_Error with "Physical device resource '"
                    & Phys_Name & "->" & Phys_Res_Name & "' referenced by "
                    & "kernel diagnostics device has different type: "
                    & Diag_Res_Type & " /= "
                    & DOM.Core.Nodes.Node_Name (N => Phys_Res);
               end if;
            end;
         end loop;
      end;
   end Kernel_Diagnostics_Device_Reference;

   -------------------------------------------------------------------------

   procedure Kernel_Diagnostics_Type_Resources
     (XML_Data : Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Diag_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => XML_Data.Doc,
           XPath => "/system/platform/kernelDiagnostics");
      Diag_Dev_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element (Doc   => Diag_Node,
                                       XPath => "device");
      Diag_Type_Str : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Diag_Node,
           Name => "type");
      Diag_Type : constant Mutools.Types.Kernel_Diagnostics_Kind
        := Mutools.Types.Kernel_Diagnostics_Kind'Value (Diag_Type_Str);

      --  Check that the diagnostics node has no child elements.
      procedure Check_Diag_None (Diag_Device : DOM.Core.Node);

      --  Check that the diagnostics node specifies a device reference with
      --  given Res_Kind resource type.
      procedure Check_Diag_Uart_Common
        (Diag_Device : DOM.Core.Node;
         Res_Kind    : String);

      --  Check that the diagnostics node specifies a device reference with I/O
      --  port resource.
      procedure Check_Diag_Uart (Diag_Device : DOM.Core.Node);

      --  Check that the diagnostics node specifies a device reference with
      --  memory resource.
      procedure Check_Diag_Hsuart (Diag_Device : DOM.Core.Node);

      --  Check that the diagnostics node specifies a device reference with
      --  I/O port and memory resource.
      procedure Check_Diag_Vga (Diag_Device : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Diag_Hsuart (Diag_Device : DOM.Core.Node)
      is
      begin
         Check_Diag_Uart_Common (Diag_Device => Diag_Device,
                                 Res_Kind    => "memory");
      end Check_Diag_Hsuart;

      ----------------------------------------------------------------------

      procedure Check_Diag_None (Diag_Device : DOM.Core.Node)
      is
      begin
         if Diag_Device /= null then
            raise Validation_Error with "Kernel diagnostics device of type '"
              & Diag_Type_Str & "' must not specify device reference";
         end if;
      end Check_Diag_None;

      ----------------------------------------------------------------------

      procedure Check_Diag_Uart (Diag_Device : DOM.Core.Node)
      is
      begin
         Check_Diag_Uart_Common (Diag_Device => Diag_Device,
                                 Res_Kind    => "ioPort");
      end Check_Diag_Uart;

      ----------------------------------------------------------------------

      procedure Check_Diag_Uart_Common
        (Diag_Device : DOM.Core.Node;
         Res_Kind    : String)
      is
      begin
         if Diag_Device = null then
            raise Validation_Error with "Kernel diagnostics device of type '"
              & Diag_Type_Str & "' must specify device reference";
         end if;

         declare
            Dev_Resources : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Diag_Device,
                 XPath => "*");
            Dev_Res_Count : constant Natural := DOM.Core.Nodes.Length
              (List => Dev_Resources);
            Dev_Res : DOM.Core.Node;
         begin
            if Dev_Res_Count /= 1 then
               raise Validation_Error with "Kernel diagnostics device of type "
                 & "'" & Diag_Type_Str & "' must specify exactly one device "
                 & "resource reference";
            end if;

            Dev_Res := DOM.Core.Nodes.Item (List  => Dev_Resources,
                                            Index => 0);
            if DOM.Core.Nodes.Node_Name (N => Dev_Res) /= Res_Kind then
               raise Validation_Error with "Kernel diagnostics device of type "
                 & "'" & Diag_Type_Str & "' must specify " & Res_Kind
                 & " device resource reference";
            end if;
         end;
      end Check_Diag_Uart_Common;

      ----------------------------------------------------------------------

      procedure Check_Diag_Vga (Diag_Device : DOM.Core.Node)
      is
      begin
         if Diag_Device = null then
            raise Validation_Error with "Kernel diagnostics device of type '"
              & Diag_Type_Str & "' must specify device reference";
         end if;

         declare
            Dev_Resources : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Diag_Device,
                 XPath => "*");
            Dev_Res_Count : constant Natural := DOM.Core.Nodes.Length
              (List => Dev_Resources);
            Dev_Res : DOM.Core.Node;
         begin
            if Dev_Res_Count /= 2 then
               raise Validation_Error with "Kernel diagnostics device of type "
                 & "'" & Diag_Type_Str & "' must specify exactly two device "
                 & "resource references";
            end if;

            Dev_Res := DOM.Core.Nodes.Item (List  => Dev_Resources,
                                            Index => 0);
            if DOM.Core.Nodes.Node_Name (N => Dev_Res) /= "memory" then
               raise Validation_Error with "Kernel diagnostics device of type "
                 & "'" & Diag_Type_Str & "' must specify a memory device "
                 & "resource reference";
            end if;

            Dev_Res := DOM.Core.Nodes.Item (List  => Dev_Resources,
                                            Index => 1);
            if DOM.Core.Nodes.Node_Name (N => Dev_Res) /= "ioPort" then
               raise Validation_Error with "Kernel diagnostics device of type "
                 & "'" & Diag_Type_Str & "' must specify an I/O port device "
                 & "resource reference";
            end if;
         end;
      end Check_Diag_Vga;
   begin
      case Diag_Type is
         when Mutools.Types.None =>
            Check_Diag_None (Diag_Device => Diag_Dev_Node);
         when Mutools.Types.Uart =>
            Check_Diag_Uart (Diag_Device => Diag_Dev_Node);
         when Mutools.Types.Hsuart =>
            Check_Diag_Hsuart (Diag_Device => Diag_Dev_Node);
         when Mutools.Types.Vga =>
            Check_Diag_Vga (Diag_Device => Diag_Dev_Node);
      end case;
   end Kernel_Diagnostics_Type_Resources;

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
