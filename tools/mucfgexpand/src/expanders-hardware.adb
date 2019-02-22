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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Constants;

with Expanders.Utils;

package body Expanders.Hardware
is

   function  U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

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

   procedure Add_MSI_IRQ_Numbers (Data : in out Muxml.XML_Data_Type)
   is
      IOAPICs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device/capabilities/"
           & "capability[@name='ioapic']/..");
      MSIs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device/irq/msi");

      Num_Alloc : Utils.Number_Allocator_Type
        (Range_Start => 104,
         Range_End   => Mutools.Constants.Hardware_Max_IRQ_Number);
   begin
      for I in Natural range 0 .. DOM.Core.Nodes.Length (List => IOAPICs) - 1
      loop
         declare
            IOAPIC_Caps : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => IOAPICs,
                                      Index => I);
            GSI_Base    : constant Natural
              := Natural'Value
                (Muxml.Utils.Get_Element_Value
                                (Doc   => IOAPIC_Caps,
                                 XPath => "capability[@name='gsi_base']"));
            Max_Redir    : constant Natural
              := Natural'Value
                (Muxml.Utils.Get_Element_Value
                   (Doc   => IOAPIC_Caps,
                    XPath => "capability[@name='max_redirection_entry']"));
         begin
            for J in Natural range GSI_Base .. GSI_Base + Max_Redir loop
               Utils.Reserve_Number (Allocator => Num_Alloc,
                                     Number    => J);
            end loop;
         end;
      end loop;

      for I in Natural range 0 .. DOM.Core.Nodes.Length (List => MSIs) - 1 loop
         declare
            MSI : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => MSIs,
                                      Index => I);
            Num : Natural;
         begin
            Utils.Allocate (Allocator => Num_Alloc,
                            Number    => Num);
            DOM.Core.Elements.Set_Attribute
              (Elem  => MSI,
               Name  => "number",
               Value => Ada.Strings.Fixed.Trim
                 (Source => Num'Img,
                  Side   => Ada.Strings.Left));
         end;
      end loop;
   end Add_MSI_IRQ_Numbers;

   -------------------------------------------------------------------------

   procedure Add_PCI_Device_MSI_IRQs (Data : in out Muxml.XML_Data_Type)
   is
      Phys_PCIs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device/pci");
      Subj_Legacy_IRQ_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device/"
           & "irq[not (msi)]/..");
      Phys_PCI_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Phys_PCIs);
   begin
      for I in Natural range 0 .. Phys_PCI_Count - 1 loop
         declare
            use type DOM.Core.Node;

            Phys_PCI : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Phys_PCIs,
                                      Index => I);
            Phys_Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Phys_PCI);
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Phys_Dev,
                 Name => "name");
            Phys_MSIs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Phys_Dev,
                 XPath => "irq/msi");
            Phys_MSI_Count : constant Natural
              := DOM.Core.Nodes.Length
              (List => Phys_MSIs);
            Has_Legacy_IRQ_Ref : constant Boolean
              :=  Muxml.Utils.Get_Element
                (Nodes     => Subj_Legacy_IRQ_Devs,
                 Ref_Attr  => "physical",
                 Ref_Value => Phys_Name) /= null;
         begin
            if Has_Legacy_IRQ_Ref then
               Muxml.Utils.Remove_Elements (Doc   => Phys_Dev,
                                            XPath => "irq/msi");
            else
               for J in Natural range 0 .. Phys_MSI_Count - 1 loop
                  declare
                     MSI : constant DOM.Core.Node
                       := DOM.Core.Nodes.Item (List  => Phys_MSIs,
                                               Index => J);
                     MSI_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => MSI,
                          Name => "name");
                     MSI_Num : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => MSI,
                          Name => "number");
                     New_IRQ : constant DOM.Core.Node
                       := DOM.Core.Documents.Create_Element
                         (Doc      => Data.Doc,
                          Tag_Name => "irq");
                  begin
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => New_IRQ,
                        Name  => "name",
                        Value => MSI_Name);
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => New_IRQ,
                        Name  => "number",
                        Value => MSI_Num);
                     Muxml.Utils.Insert_Before
                       (Parent    => Phys_Dev,
                        New_Child => New_IRQ,
                        Ref_Names => (1 => U ("memory"),
                                      2 => U ("ioPort")));
                  end;
               end loop;

               Muxml.Utils.Remove_Elements
                 (Doc   => Phys_Dev,
                  XPath => "irq[msi]");
            end if;

            DOM.Core.Elements.Set_Attribute
              (Elem  => Phys_PCI,
               Name  => "msi",
               Value => Ada.Characters.Handling.To_Lower
                 (Item => Boolean'Image
                      (Phys_MSI_Count > 0 and not Has_Legacy_IRQ_Ref)));
         end;
      end loop;
   end Add_PCI_Device_MSI_IRQs;

   -------------------------------------------------------------------------

   procedure Add_Processor_CPU_IDs (Data : in out Muxml.XML_Data_Type)
   is
      Already_Set : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/processor/cpu[@cpuId]");
      Already_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Already_Set);
      To_Set : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/processor/cpu[not(@cpuId)]");
      To_Set_Count : constant Natural
        := DOM.Core.Nodes.Length (List => To_Set);
      Num_Alloc : Utils.Number_Allocator_Type
        (Range_Start => 0,
         Range_End   => Already_Count + To_Set_Count - 1);
   begin
      Utils.Reserve_Numbers (Allocator => Num_Alloc,
                             Nodes     => Already_Set,
                             Attribute => "cpuId");

      for I in 0 .. To_Set_Count -  1 loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => To_Set,
                                      Index => I);
            Num : Natural;
         begin
            Utils.Allocate (Allocator => Num_Alloc,
                            Number    => Num);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "cpuId",
               Value => Ada.Strings.Fixed.Trim
                 (Source => Num'Img,
                  Side   => Ada.Strings.Left));
         end;
      end loop;
   end Add_Processor_CPU_IDs;

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
