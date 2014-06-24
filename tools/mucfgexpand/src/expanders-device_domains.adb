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
with Ada.Containers.Ordered_Sets;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;

package body Expanders.Device_Domains
is

   type PCI_Bus_Range is range 0 .. 255;

   package PCI_Bus_Set is new Ada.Containers.Ordered_Sets
     (Element_Type => PCI_Bus_Range);

   --  Return set of occupied PCI bus numbers for given system policy.
   function Get_Occupied_PCI_Buses
     (Data : Muxml.XML_Data_Type)
      return PCI_Bus_Set.Set;

   -------------------------------------------------------------------------

   procedure Add_Domain_IDs (Data : in out Muxml.XML_Data_Type)
   is
      Domains : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/deviceDomains/domain");
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Domains) loop
         declare
            Domain : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Domains,
                 Index => I - 1);
            Name   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Domain,
                 Name => "name");
            ID_Str : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
         begin
            Mulog.Log (Msg => "Setting ID of device security domain '" & Name
                       & "' to " & ID_Str);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Domain,
               Name  => "id",
               Value => ID_Str);
         end;
      end loop;
   end Add_Domain_IDs;

   -------------------------------------------------------------------------

   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      DD_Node     : DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/deviceDomains");
      Events_Node : DOM.Core.Node;
   begin
      if DD_Node = null then
         Events_Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/events");

         DD_Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "deviceDomains");
         DD_Node := DOM.Core.Nodes.Insert_Before
           (N         => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
            New_Child => DD_Node,
            Ref_Child => Events_Node);
         pragma Unreferenced (DD_Node);
      end if;
   end Add_Section_Skeleton;

   -------------------------------------------------------------------------

   procedure Add_Tables (Data : in out Muxml.XML_Data_Type)
   is
      IOMMUs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/platform/devices/device[starts-with"
           & "(string(@name),'iommu')]");
   begin
      if DOM.Core.Nodes.Length (List => IOMMUs) = 0 then
         Mulog.Log (Msg => "No IOMMU device found, not adding VT-d tables");
         return;
      end if;

      Mulog.Log (Msg => "Adding VT-d DMAR root table");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "root|vtd",
         Address     => "",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "system_vtd_root",
         File_Name   => "vtd_root",
         File_Offset => "none");

      declare
         PCI_Buses : constant PCI_Bus_Set.Set := Get_Occupied_PCI_Buses
           (Data => Data);
         Curr_Idx  : PCI_Bus_Set.Cursor       := PCI_Buses.First;
         Curr_Bus  : PCI_Bus_Range;
      begin
         while PCI_Bus_Set.Has_Element (Position => Curr_Idx) loop
            Curr_Bus := PCI_Bus_Set.Element (Position => Curr_Idx);
            declare
               Curr_Bus_Hx : constant String := Mutools.Utils.To_Hex
                 (Number    => Interfaces.Unsigned_64 (Curr_Bus),
                  Normalize => False);
            begin
               Mulog.Log (Msg => "Adding VT-d DMAR context table for PCI bus "
                          & "16#" & Curr_Bus_Hx & "#");
               Mutools.XML_Utils.Add_Memory_Region
                 (Policy      => Data,
                  Name        => "context_" & Curr_Bus_Hx,
                  Address     => "",
                  Size        => "16#1000#",
                  Caching     => "WB",
                  Alignment   => "16#1000#",
                  Memory_Type => "system_vtd_context",
                  File_Name   => "vtd_context_bus_" & Curr_Bus_Hx,
                  File_Offset => "none");
            end;
            PCI_Bus_Set.Next (Position => Curr_Idx);
         end loop;
      end;
   end Add_Tables;

   -------------------------------------------------------------------------

   function Get_Occupied_PCI_Buses
     (Data : Muxml.XML_Data_Type)
      return PCI_Bus_Set.Set
   is
      Buses  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/platform/devices/device/pci/@bus");
      Result : PCI_Bus_Set.Set;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Buses) - 1 loop
         declare
            Bus : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Buses,
                 Index => I);
         begin
            Result.Insert
              (New_Item => PCI_Bus_Range'Value
                 (DOM.Core.Nodes.Node_Value (N => Bus)));

         exception
            when Constraint_Error => null;
         end;
      end loop;

      return Result;
   end Get_Occupied_PCI_Buses;

end Expanders.Device_Domains;
