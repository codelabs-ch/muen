--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;

package body Expanders.Hardware
is

   -------------------------------------------------------------------------

   procedure Add_IOMMU_Default_Caps (Data : in out Muxml.XML_Data_Type)
   is
      IOMMUs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device[capabilities/"
           & "capability/@name='iommu']");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => IOMMUs) - 1 loop
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
            Caps  : DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => IOMMU,
                 XPath => "capabilities");
         begin
            if Caps = null then
               Caps := DOM.Core.Nodes.Append_Child
                 (N         => IOMMU,
                  New_Child => DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "capabilities"));
            end if;

            declare
               AGAW : DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => Caps,
                    XPath => "capability[@name='agaw']");
            begin
               if AGAW = null then
                  Mulog.Log (Msg => "Setting AGAW capability of IOMMU '"
                             & Name & "' to 39");

                  AGAW := DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "capability");
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => AGAW,
                     Name  => "name",
                     Value => "agaw");
                  AGAW := DOM.Core.Nodes.Append_Child
                    (N         => Caps,
                     New_Child => AGAW);
                  AGAW := DOM.Core.Nodes.Append_Child
                    (N         => AGAW,
                     New_Child => DOM.Core.Documents.Create_Text_Node
                       (Doc  => Data.Doc,
                        Data => "39"));
               end if;
            end;

            declare
               Fr_Offset_Cap : DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => Caps,
                    XPath => "capability[@name='fr_offset']");
            begin
               if Fr_Offset_Cap = null then
                  Mulog.Log (Msg => "Setting capability 'fr_offset' of IOMMU '"
                             & Name & "' to 512");

                  Fr_Offset_Cap := DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "capability");
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => Fr_Offset_Cap,
                     Name  => "name",
                     Value => "fr_offset");
                  Fr_Offset_Cap := DOM.Core.Nodes.Append_Child
                    (N         => Caps,
                     New_Child => Fr_Offset_Cap);
                  Fr_Offset_Cap := DOM.Core.Nodes.Append_Child
                    (N         => Fr_Offset_Cap,
                     New_Child => DOM.Core.Documents.Create_Text_Node
                       (Doc  => Data.Doc,
                        Data => "512"));
               end if;
            end;

            declare
               Iotlb_Inv_Cap : DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => Caps,
                    XPath => "capability[@name='iotlb_invalidate_offset']");
            begin
               if Iotlb_Inv_Cap = null then
                  Mulog.Log (Msg => "Setting capability "
                             & "'iotlb_invalidate_offset' of IOMMU '" & Name
                             & "' to 264");

                  Iotlb_Inv_Cap := DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "capability");
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => Iotlb_Inv_Cap,
                     Name  => "name",
                     Value => "iotlb_invalidate_offset");
                  Iotlb_Inv_Cap := DOM.Core.Nodes.Append_Child
                    (N         => Caps,
                     New_Child => Iotlb_Inv_Cap);
                  Iotlb_Inv_Cap := DOM.Core.Nodes.Append_Child
                    (N         => Iotlb_Inv_Cap,
                     New_Child => DOM.Core.Documents.Create_Text_Node
                       (Doc  => Data.Doc,
                        Data => "264"));
               end if;
            end;
         end;
      end loop;
   end Add_IOMMU_Default_Caps;

   -------------------------------------------------------------------------

   procedure Add_Reserved_Memory_Blocks (Data : in out Muxml.XML_Data_Type)
   is
      Mem_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/hardware/memory");
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Mem_Node,
           XPath => "reservedMemory");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Region_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Region_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Region_Node,
                 Name => "name");
            Phys_Address : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Region_Node,
                 Name => "physicalAddress");
            Size         : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Region_Node,
                 Name => "size");
            Block_Node   : constant DOM.Core.Node
              := DOM.Core.Documents.Create_Element
                (Doc      => Data.Doc,
                 Tag_Name => "memoryBlock");
         begin
            Mulog.Log (Msg => "Adding memory block for reserved memory region "
                       & "'" & Region_Name & "' with size " & Size
                       & " @ physical address " & Phys_Address);

            DOM.Core.Elements.Set_Attribute
              (Elem  => Block_Node,
               Name  => "allocatable",
               Value => "false");
            DOM.Core.Elements.Set_Attribute
              (Elem  => Block_Node,
               Name  => "name",
               Value => Region_Name);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Block_Node,
               Name  => "physicalAddress",
               Value => Phys_Address);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Block_Node,
               Name  => "size",
               Value => Size);
            Muxml.Utils.Append_Child
              (Node      => Mem_Node,
               New_Child => Block_Node);
         end;
      end loop;
   end Add_Reserved_Memory_Blocks;

   -------------------------------------------------------------------------

   procedure Remove_Device_MSIs (Data : in out Muxml.XML_Data_Type)
   is
      Device_IRQs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device/irq[msi]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Device_IRQs) - 1 loop
         declare
            IRQ_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Device_IRQs,
                 Index => I);
         begin
            Muxml.Utils.Remove_Elements
              (Doc   => IRQ_Node,
               XPath => "msi");
         end;
      end loop;
   end Remove_Device_MSIs;

   -------------------------------------------------------------------------

   procedure Remove_Reserved_Mem_References (Data : in out Muxml.XML_Data_Type)
   is
      References :  constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device/reservedMemory");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => References) - 1 loop
         declare
            Cur_Ref : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => References,
               Index => I);
            Parent  : constant DOM.Core.Node := DOM.Core.Nodes.Parent_Node
              (N => Cur_Ref);
         begin
            Muxml.Utils.Remove_Child
              (Node       => Parent,
               Child_Name => "reservedMemory");
         end;
      end loop;
   end Remove_Reserved_Mem_References;

   -------------------------------------------------------------------------

   procedure Remove_Reserved_Mem_Regions (Data : in out Muxml.XML_Data_Type)
   is
      Parent  : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/hardware/memory");
      Regions :  constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Parent,
           XPath => "reservedMemory");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Regions) - 1 loop
         Muxml.Utils.Remove_Child
           (Node       => Parent,
            Child_Name => "reservedMemory");
      end loop;
   end Remove_Reserved_Mem_Regions;

end Expanders.Hardware;
