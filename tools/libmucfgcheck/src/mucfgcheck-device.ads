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

package Mucfgcheck.Device
is
   --D @Section Id => validation-dev, Label => Devices, Parent => validation, Priority => 0
   --D @Text Section => validation-dev, Priority => 0
   --D The following checks are performed to guarantee that hardware devices
   --D are correctly configured in the system policy.
   --D @UL Id => validators_dev, Section => validation-dev, Priority => 0

   --D @Item List => validators_dev, Priority => 0
   --D Validate that devices referenced by logical devices exist.
   procedure Physical_Device_References (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that device names (including device aliases/classes) are
   --D unique.
   procedure Physical_Device_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that all physical IRQs are unique.
   procedure Physical_IRQ_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that physical device IRQs referenced by logical IRQs exist.
   procedure Physical_IRQ_References (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that ISA IRQs fulfill their constraints.
   procedure Physical_IRQ_Constraints_ISA (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that PCI LSI IRQs fulfill their constraints.
   procedure Physical_IRQ_Constraints_PCI_LSI (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that PCI MSI IRQs fulfill their constraints.
   procedure Physical_IRQ_Constraints_PCI_MSI (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that PCI MSI IRQs are consecutive.
   procedure Physical_IRQ_MSI_Consecutiveness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that physical IRQ names are unique per device.
   procedure Device_IRQ_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that all I/O start ports are smaller than end ports.
   procedure IO_Port_Start_Smaller_End (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that physical I/O ports referenced by logical I/O ports exist.
   procedure IO_Port_References (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that all physical I/O ports are unique.
   procedure IO_Port_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that physical I/O port names are unique per device.
   procedure Device_IO_Port_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that device memory names are unique.
   procedure Device_Memory_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that device memory referenced by logical device memory exists.
   procedure Device_Memory_References (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that PCI device bus, device, function triplets are unique.
   procedure PCI_Device_BDF_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that logical device references of each subject do not refer to
   --D the same physical device.
   procedure Device_Reference_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that PCI device reference bus, device, function triplets are
   --D unique per subject.
   procedure Device_Reference_BDF_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that all device references specifying a bus, device, function
   --D triplet are references to physical PCI devices.
   procedure PCI_Device_References (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that all device references to PCI multi-function devices belong
   --D to the same subject and have the same logical device number.
   procedure PCI_Multifunction_Device_Refs (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that all device references not specifying a bus, device,
   --D function triplet are references to physical legacy (non-PCI) devices.
   procedure Legacy_Device_References (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that all logical PCI devices specify bus number zero.
   procedure Device_References_PCI_Bus_Number (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_dev, Priority => 0
   --D Validate that all IOMMU memory-mapped IO regions have a size of 4K.
   procedure IOMMU_Region_Size (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Device;
