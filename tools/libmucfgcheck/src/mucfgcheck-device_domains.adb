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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;

package body Mucfgcheck.Device_Domains
is

   use McKae.XML.XPath.XIA;

   -------------------------------------------------------------------------

   procedure Device_Reference_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/deviceDomains/domain/devices/device");

      --  Check inequality of device reference physical names.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "physical");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "physical");
      begin
         if Left_Name = Right_Name then
            declare
               L_Dom_Name : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Muxml.Utils.Ancestor_Node (Node  => Left,
                                                     Level => 2),
                  Name => "name");
               R_Dom_Name : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Muxml.Utils.Ancestor_Node (Node  => Right,
                                                     Level => 2),
                  Name => "name");
            begin
               raise Validation_Error with "Device domains '" & L_Dom_Name
                 & "' and '" & R_Dom_Name & "' "
                 & "reference same physical device '" & Left_Name & "'";
            end;
         end if;
      end Check_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " security domain device reference(s)");

      Compare_All (Nodes      => Nodes,
                   Comparator => Check_Inequality'Access);
   end Device_Reference_Uniqueness;

   -------------------------------------------------------------------------

   procedure IOMMU_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      Domains   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/deviceDomains/domain");
      Dom_Count : constant Natural := DOM.Core.Nodes.Length (List => Domains);
   begin
      if Dom_Count > 0 then
         Mulog.Log (Msg => "Checking presence of IOMMU device(s)");
         declare
            IOMMUs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => XML_Data.Doc,
                 XPath => "/system/platform/devices/"
                 & "device[starts-with(@name,'iommu')]");
         begin
            if DOM.Core.Nodes.Length (List => IOMMUs) = 0 then
               raise Validation_Error with "Device domains specified but no"
                 & " IOMMU device provided by platform";
            end if;
         end;
      end if;
   end IOMMU_Presence;

end Mucfgcheck.Device_Domains;
