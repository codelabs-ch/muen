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

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;

package body Expanders.Hardware
is

   --  Calculate the PCI config space window address of the device with BDF as
   --  specified by the PCI node and the given base address
   function Calculate_PCI_Cfg_Address
     (Base_Address : Interfaces.Unsigned_64;
      PCI_Node     : DOM.Core.Node)
      return Interfaces.Unsigned_64;

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
            Agaw  : DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Caps,
                 XPath => "capability[@name='agaw']");
         begin
            if Agaw = null then
               Mulog.Log (Msg => "Setting default capabilities of IOMMU '"
                          & Name & "'");

               if Caps = null then
                  Caps := DOM.Core.Nodes.Append_Child
                    (N         => IOMMU,
                     New_Child => DOM.Core.Documents.Create_Element
                       (Doc      => Data.Doc,
                        Tag_Name => "capabilities"));
               end if;

               Agaw := DOM.Core.Documents.Create_Element
                 (Doc      => Data.Doc,
                  Tag_Name => "capability");
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Agaw,
                  Name  => "name",
                  Value => "agaw");
               Agaw := DOM.Core.Nodes.Append_Child
                 (N         => Caps,
                  New_Child => Agaw);
               Agaw := DOM.Core.Nodes.Append_Child
                 (N         => Agaw,
                  New_Child => DOM.Core.Documents.Create_Text_Node
                    (Doc  => Data.Doc,
                     Data => "39"));
            end if;
         end;
      end loop;
   end Add_IOMMU_Default_Caps;

   -------------------------------------------------------------------------

   procedure Add_PCI_Config_Space (Data : in out Muxml.XML_Data_Type)
   is
      Start_Addr_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Data.Doc,
           XPath => "/system/hardware/devices",
           Name  => "pciConfigAddress");
      PCI_Devices    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device[pci]");
      Cfg_Start_Addr : Interfaces.Unsigned_64;
   begin
      if Start_Addr_Str'Length = 0 then
         return;
      end if;

      Cfg_Start_Addr := Interfaces.Unsigned_64'Value (Start_Addr_Str);

      for I in 0 .. DOM.Core.Nodes.Length (List => PCI_Devices) - 1 loop
         declare
            use type DOM.Core.Node;

            Device : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => PCI_Devices,
               Index => I);
            Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Device,
                 Name => "name");
            BFD_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Device,
               XPath => "pci");
            PCI_Cfg_Addr : constant Interfaces.Unsigned_64
              := Calculate_PCI_Cfg_Address (Base_Address => Cfg_Start_Addr,
                                            PCI_Node     => BFD_Node);
            PCI_Cfg_Addr_Str : constant String
              := Mutools.Utils.To_Hex (Number => PCI_Cfg_Addr);
            Dev_Ref_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject/devices/device[@physical='"
               & Dev_Name & "']");
         begin
            Mulog.Log (Msg => "Adding PCI config space region to device '"
                       & Dev_Name & "' at physical address "
                       & PCI_Cfg_Addr_Str);
            Muxml.Utils.Append_Child
              (Node      => Device,
               New_Child => Mutools.XML_Utils.Create_Memory_Node
                 (Policy      => Data,
                  Name        => "mmconf",
                  Address     => PCI_Cfg_Addr_Str,
                  Size        => "16#1000#",
                  Caching     => "UC",
                  Alignment   => "",
                  Memory_Type => ""));
            if Dev_Ref_Node /= null then
               declare
                  Virtual_BFD_Node         : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => Dev_Ref_Node,
                       XPath => "pci");
                  Virtual_PCI_Cfg_Addr_Str : constant String
                    := Mutools.Utils.To_Hex
                      (Number => Calculate_PCI_Cfg_Address
                         (Base_Address => Cfg_Start_Addr,
                          PCI_Node     => Virtual_BFD_Node));
               begin
                  Muxml.Utils.Append_Child
                    (Node      => Dev_Ref_Node,
                     New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                       (Policy        => Data,
                        Logical_Name  => "mmconf",
                        Physical_Name => "mmconf",
                        Address       => Virtual_PCI_Cfg_Addr_Str,
                        Writable      => True,
                        Executable    => False));
               end;
            end if;
         end;
      end loop;
   end Add_PCI_Config_Space;

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

   function Calculate_PCI_Cfg_Address
     (Base_Address : Interfaces.Unsigned_64;
      PCI_Node     : DOM.Core.Node)
      return Interfaces.Unsigned_64
   is
      use type Interfaces.Unsigned_64;

      Bus_Nr    : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => PCI_Node,
              Name => "bus"));
      Device_Nr : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => PCI_Node,
              Name => "device"));
      Func_Nr   : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => PCI_Node,
              Name => "function"));
   begin
      return Base_Address +
        (Bus_Nr * 2 ** 20 + Device_Nr * 2 ** 15 + Func_Nr * 2 ** 12);
   end Calculate_PCI_Cfg_Address;

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
