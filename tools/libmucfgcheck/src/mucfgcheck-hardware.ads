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

with Muxml;

package Mucfgcheck.Hardware
is
   --D @Section Id => validation-hw, Label => Hardware, Parent => validation
   --D @Text Section => validation-hw
   --D The following checks are performed on the hardware section of the policy.
   --D @UL Id => validators_hw, Section => validation-hw

   --D @Item List => validators_hw
   --D Validate that memory regions fit into available hardware memory.
   procedure Memory_Space (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_hw
   --D Validate that no memory blocks overlap.
   procedure Memory_Block_Overlap (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_hw
   --D Validate that the size of memory blocks is a multiple of page size.
   procedure Memory_Block_Size (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_hw
   --D Validate that PCI config space address and size are specified if PCI
   --D devices are present.
   procedure PCI_Config_Space (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_hw
   --D Validate that the hardware provides enough physical CPU cores.
   procedure CPU_Count (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_hw
   --D Validate that the processor CPU sub-elements are correct.
   procedure CPU_Sub_Elements (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_hw
   --D Validate that at least one I/O APIC device is present.
   procedure IOAPIC_Presence (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_hw
   --D Validate that all I/O APICs have a valid source ID capability.
   procedure IOAPIC_Cap_SID (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_hw
   --D Validate that at least one and at most eight IOMMU devices are present.
   procedure IOMMU_Presence (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_hw
   --D Validate that all IOMMUs have the AGAW capability set correctly and that
   --D multiple IOMMUs specify the same value.
   procedure IOMMU_Cap_Agaw (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_hw
   --D Validate that all IOMMUs have correct register offset capabilities.
   procedure IOMMU_Cap_Register_Offsets (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_hw
   --D Check that the hardware contains a system board device providing the
   --D expected configuration.
   procedure System_Board_Presence (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Hardware;
