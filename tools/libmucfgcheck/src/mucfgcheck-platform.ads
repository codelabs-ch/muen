--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Mucfgcheck.Platform
is
   --D @Section Id => validation-platform, Label => Platform, Parent => validation, Priority => 0
   --D @Text Section => validation-platform, Priority => 0
   --D The following checks are performed to verify the correctness of the
   --D platform configuration in the system policy.
   --D @UL Id => validators_platform, Section => validation-platform, Priority => 0

   --D @Item List => validators_platform, Priority => 0
   --D Validate that physical devices referenced by device aliases exist.
   procedure Alias_Physical_Device_References (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_platform, Priority => 0
   --D Validate that physical device resources referenced by device aliases
   --D exist.
   procedure Alias_Physical_Device_Resource_References
     (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_platform, Priority => 0
   --D Validate that physical devices referenced by device classes exist.
   procedure Class_Physical_Device_References (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_platform, Priority => 0
   --D Validate that subject devices that reference an alias only contain
   --D resources provided by the device alias.
   procedure Subject_Alias_Resource_References
     (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_platform, Priority => 0
   --D Validate that the physical device and resources referenced by the kernel
   --D diagnostics device exists.
   procedure Kernel_Diagnostics_Device_Reference
     (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_platform, Priority => 0
   --D Validate that the kernel diagnostics device resources match the
   --D requirements of the specified diagnostics type.
   procedure Kernel_Diagnostics_Type_Resources
     (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Platform;
