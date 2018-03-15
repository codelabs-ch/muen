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

with Muxml;

package Cfgchecks
is

   --  Check that tau0 is present in the scheduling plan.
   procedure Tau0_Presence_In_Scheduling (XML_Data : Muxml.XML_Data_Type);

   --  Check subject monitor references.
   procedure Subject_Monitor_References (XML_Data : Muxml.XML_Data_Type);

   --  Check subject channel references.
   procedure Subject_Channel_References (XML_Data : Muxml.XML_Data_Type);

   --  Check that a subject maps the logical channels requested by the
   --  referenced component to a valid physical channel.
   procedure Subject_Channel_Exports (XML_Data : Muxml.XML_Data_Type);

   --  Check that a subject maps the logical memory regions requested by the
   --  referenced component to valid physical memory regions.
   procedure Subject_Memory_Exports (XML_Data : Muxml.XML_Data_Type);

   --  Check that a subject maps the logical devices requested by the
   --  referenced component to valid physical devices (incl. all resources).
   procedure Subject_Device_Exports (XML_Data : Muxml.XML_Data_Type);

   --  Check that subject logical component resource mappings are unique.
   procedure Subject_Resource_Maps_Logical_Uniqueness
     (XML_Data : Muxml.XML_Data_Type);

   --  Check that subject physical component resource mappings are unique.
   procedure Subject_Resource_Maps_Physical_Uniqueness
     (XML_Data : Muxml.XML_Data_Type);

   --  Check that subject monitor loader virtual addresses are in the valid
   --  range.
   procedure Subject_Monitor_Loader_Addresses (XML_Data : Muxml.XML_Data_Type);

   --  Check that loader subjects also monitor subject state and that the
   --  logical name has the form monitor_state_SUBJECT_NAME.
   procedure Subject_Monitor_Loader_States (XML_Data : Muxml.XML_Data_Type);

   --  Check that each channel has exactly one reader and one writer.
   procedure Channel_Reader_Writer (XML_Data : Muxml.XML_Data_Type);

   --  Check that writer of a channel with hasEvent specifies an event ID.
   procedure Channel_Writer_Has_Event_ID (XML_Data : Muxml.XML_Data_Type);

   --  Check that reader of a channel with hasEvent modes async and ipi
   --  specifies a vector number.
   procedure Channel_Reader_Has_Event_Vector (XML_Data : Muxml.XML_Data_Type);

   --  Check that the cpuCores attribute of '/system/hardware/processor' is
   --  present.
   procedure Hardware_CPU_Count_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Check that the hardware provides an I/O APIC device.
   procedure Hardware_IOAPIC_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Check that reserved memory region names are unique.
   procedure Hardware_Reserved_Memory_Region_Name_Uniqueness
     (XML_Data : Muxml.XML_Data_Type);

   --  Check reserved memory region references.
   procedure Hardware_Reserved_Memory_Region_References
     (XML_Data : Muxml.XML_Data_Type);

   --  Check that devices referencing the same RMRR are assigned to the same
   --  device domain.
   procedure Device_RMRR_Domain_Assignment (XML_Data : Muxml.XML_Data_Type);

   --  Check subject component references.
   procedure Subject_Component_References (XML_Data : Muxml.XML_Data_Type);

   --  Check that library component names are unique.
   procedure Library_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Check that component names are unique.
   procedure Component_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Check that component logical names of required channels are unique.
   procedure Component_Channel_Name_Uniqueness
     (XML_Data : Muxml.XML_Data_Type);

   --  Check that requested logical channel sizes match the linked physical
   --  channel sizes.
   procedure Component_Channel_Size (XML_Data : Muxml.XML_Data_Type);

   --  Check that requested logical memory sizes match the linked physical
   --  memory sizes.
   procedure Component_Memory_Size (XML_Data : Muxml.XML_Data_Type);

   --  Check that requested logical device memory sizes match the linked
   --  physical device memory sizes.
   procedure Component_Device_Memory_Size (XML_Data : Muxml.XML_Data_Type);

   --  Check that requested logical device I/O port start and end addresses
   --  match the linked physical device I/O port range.
   procedure Component_Device_IO_Port_Range (XML_Data : Muxml.XML_Data_Type);

   --  Check component library references.
   procedure Component_Library_References (XML_Data : Muxml.XML_Data_Type);

   --  Check component library cyclic references.
   procedure Component_Library_Cyclic_References
     (XML_Data : Muxml.XML_Data_Type);

   --  Chech that the kernel diagnostics port references a physical I/O device.
   procedure Kernel_Diagnostics_Dev_Reference (XML_Data : Muxml.XML_Data_Type);

end Cfgchecks;
