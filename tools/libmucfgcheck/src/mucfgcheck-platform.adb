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

with DOM.Core.Elements;
with DOM.Core.Nodes;

with Mutools.Match;

package body Mucfgcheck.Platform
is

   -------------------------------------------------------------------------

   procedure Physical_Device_References (XML_Data : Muxml.XML_Data_Type)
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
   end Physical_Device_References;

   -------------------------------------------------------------------------

   procedure Physical_Device_Resource_References
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
   end Physical_Device_Resource_References;

end Mucfgcheck.Platform;
