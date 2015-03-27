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

with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;
with Mutools.Constants;

package body Mucfgcheck.Platform
is

   use McKae.XML.XPath.XIA;

   --  Returns the sum of all node values.
   function Sum (Nodes : DOM.Core.Node_List) return Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   procedure CPU_Count (XML_Data : Muxml.XML_Data_Type)
   is
      Active_CPUs  : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      Logical_CPUs : constant Positive
        := Positive'Value
          (Muxml.Utils.Get_Attribute
             (Doc   => XML_Data.Doc,
              XPath => "/system/platform/processor",
              Name  => "logicalCpus"));
   begin
      Mulog.Log (Msg => "Checking logical CPU count");

      if Active_CPUs > Logical_CPUs then
         raise Validation_Error with "System requires" & Active_CPUs'Img
           & " but platform only provides" & Logical_CPUs'Img
           & " CPU(s)";
      end if;
   end CPU_Count;

   -------------------------------------------------------------------------

   procedure IOMMU_Cap_Agaw (XML_Data : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;

      IOMMUs    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/platform/devices/device[capabilities/"
           & "capability/@name='iommu']");
      Last_Agaw : Unbounded_String;
   begin
      Mulog.Log (Msg => "Validating AGAW capability for"
                 & DOM.Core.Nodes.Length (List => IOMMUs)'Img & " IOMMU(s)");

      for I in 0 .. DOM.Core.Nodes.Length (IOMMUs) - 1 loop
         declare
            use type DOM.Core.Node;

            IOMMU : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => IOMMUs,
                 Index => I);
            Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => IOMMU,
                 Name => "name");
            Agaw  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => IOMMU,
                 XPath => "capabilities/capability[@name='agaw']");
         begin
            if Agaw = null or else DOM.Core.Nodes.Node_Value
              (N => DOM.Core.Nodes.First_Child (N => Agaw))'Length = 0
            then
               raise Validation_Error with "AGAW capability of IOMMU '"
                 & Name & "' is not set";
            end if;

            declare
               Agaw_Str : constant String := DOM.Core.Nodes.Node_Value
                 (N => DOM.Core.Nodes.First_Child (N => Agaw));
            begin
               if Agaw_Str /= "39" and then Agaw_Str /= "48" then
                  raise Validation_Error with "AGAW capability of IOMMU '"
                    & Name & "' set to invalid value '" & Agaw_Str & "'";
               end if;

               if Last_Agaw = Null_Unbounded_String then
                  Last_Agaw := To_Unbounded_String (Agaw_Str);
               elsif Last_Agaw /= Agaw_Str then
                  raise Validation_Error with "IOMMUs have different AGAW "
                    & "capabilities set ('" & To_String (Last_Agaw) & "' vs. '"
                    & Agaw_Str & "')";
               end if;
            end;
         end;
      end loop;
   end IOMMU_Cap_Agaw;

   -------------------------------------------------------------------------

   procedure IOMMU_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      IOMMUs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/platform/devices/device[capabilities/"
           & "capability/@name='iommu']");
   begin
      Mulog.Log (Msg => "Checking presence of IOMMU device(s)");
      if DOM.Core.Nodes.Length (List => IOMMUs) = 0 then
         raise Validation_Error with "No IOMMU device provided by platform";
      end if;
   end IOMMU_Presence;

   -------------------------------------------------------------------------

   procedure Memory_Block_Overlap (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/memory/memoryBlock");
   begin
      Check_Memory_Overlap (Nodes        => Nodes,
                            Region_Type  => "platform memory block",
                            Address_Attr => "physicalAddress");
   end Memory_Block_Overlap;

   -------------------------------------------------------------------------

   procedure Memory_Block_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/memory/memoryBlock");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "platform memory block",
                       Attr      => "size",
                       Name_Attr => "name",
                       Test      => Mod_Equal_Zero'Access,
                       B         => Mutools.Constants.Page_Size,
                       Error_Msg => "not multiple of page size (4K)");
   end Memory_Block_Size;

   -------------------------------------------------------------------------

   procedure Memory_Space (XML_Data : Muxml.XML_Data_Type)
   is
      Blocks    : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/memory/memoryBlock/@size");
      Mems      : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory/@size");
      Block_Sum : constant Interfaces.Unsigned_64 := Sum (Nodes => Blocks);
      Mem_Sum   : constant Interfaces.Unsigned_64 := Sum (Nodes => Mems);
   begin
      Mulog.Log (Msg => "Checking size of" & DOM.Core.Nodes.Length
                 (List => Mems)'Img & " physical memory region(s) against"
                 & DOM.Core.Nodes.Length (List => Blocks)'Img
                 & " memory block(s)");

      if Mem_Sum > Block_Sum then
         raise Validation_Error with "Allocated " & Mutools.Utils.To_Hex
           (Number => Mem_Sum) & " bytes of physical memory but only "
           & Mutools.Utils.To_Hex (Number => Block_Sum)
           & " bytes available by the platform";
      end if;
   end Memory_Space;

   -------------------------------------------------------------------------

   procedure PCI_Config_Space_Address (XML_Data : Muxml.XML_Data_Type)
   is
      Cfg_Address   : constant String := Muxml.Utils.Get_Attribute
        (Doc   => XML_Data.Doc,
         XPath => "/system/platform/devices",
         Name  => "pciConfigAddress");
      PCI_Dev_Count : constant Natural
        := DOM.Core.Nodes.Length
          (List => XPath_Query
             (N     => XML_Data.Doc,
              XPath => "/system/platform/devices/device/pci"));
   begin
      Mulog.Log (Msg => "Checking PCI configuration space address");
      if PCI_Dev_Count > 0 and then Cfg_Address'Length = 0 then
         raise Validation_Error with "Missing PCI configuration space address";
      end if;
   end PCI_Config_Space_Address;

   -------------------------------------------------------------------------

   function Sum (Nodes : DOM.Core.Node_List) return Interfaces.Unsigned_64
   is
      Node : DOM.Core.Node;
      Sum  : Interfaces.Unsigned_64 := 0;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         Node := DOM.Core.Nodes.Item
           (List  => Nodes,
            Index => I);
         Sum := Sum + Interfaces.Unsigned_64'Value
           (DOM.Core.Nodes.Node_Value (N => Node));
      end loop;

      return Sum;
   end Sum;

end Mucfgcheck.Platform;
