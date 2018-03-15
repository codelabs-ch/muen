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

   --  Validate that memory regions fit into available hardware memory.
   procedure Memory_Space (XML_Data : Muxml.XML_Data_Type);

   --  Validate that no memory blocks overlap.
   procedure Memory_Block_Overlap (XML_Data : Muxml.XML_Data_Type);

   --  Validate size of memory blocks.
   procedure Memory_Block_Size (XML_Data : Muxml.XML_Data_Type);

   --  Validate that PCI config space address is specified if PCI devices are
   --  present.
   procedure PCI_Config_Space_Address (XML_Data : Muxml.XML_Data_Type);

   --  Validate that the hardware provides enough physical CPU cores.
   procedure CPU_Count (XML_Data : Muxml.XML_Data_Type);

   --  Validate that at least one I/O APIC device is present.
   procedure IOAPIC_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that exactly two IOMMU devices are present.
   procedure IOMMU_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that all IOMMUs have the AGAW capability set correctly and that
   --  multiple IOMMUs specify the same value.
   procedure IOMMU_Cap_Agaw (XML_Data : Muxml.XML_Data_Type);

   --  Validate that all IOMMUs have correct register offset capabilities.
   procedure IOMMU_Cap_Register_Offsets (XML_Data : Muxml.XML_Data_Type);

   --  Check that the hardware contains a system board device providing the
   --  expected configuration.
   procedure System_Board_Presence (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Hardware;
