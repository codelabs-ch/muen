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

with Bfd.Files;
with Bfd.Sections;

with Muxml;

package Elfcheck.Bfd_Utils
is

   --  Get section by name from given file descriptor. If the requested section
   --  is not found, an exception is raised.
   function Get_Section
     (Descriptor : Bfd.Files.File_Type;
      Name       : String)
      return Bfd.Sections.Section;

   --  Check that a section matches the properties defined by the memory region
   --  identified by name. An exception is raised if a property does not fit
   --  the requirements. If Mapped is False, the VMA is not validated.
   procedure Check_Section
     (Policy      : Muxml.XML_Data_Type;
      Region_Name : String;
      Section     : Bfd.Sections.Section;
      Mapped      : Boolean := True);

private

   --  Validate that the section size is smaller than the size of the memory
   --  region identified by name.
   procedure Validate_Size
     (Section      : Bfd.Sections.Section;
      Section_Name : String;
      Region_Name  : String;
      Size         : Interfaces.Unsigned_64);

   --  Validate that the section VMA is equal to the virtualAddress of the
   --  memory region identified by name.
   procedure Validate_VMA
     (Section      : Bfd.Sections.Section;
      Section_Name : String;
      Region_Name  : String;
      Address      : Interfaces.Unsigned_64);

   --  Validate that the section LMA plus size lays within the physical memory
   --  region given by address and size.
   procedure Validate_LMA_In_Region
     (Section      : Bfd.Sections.Section;
      Section_Name : String;
      Region_Name  : String;
      Address      : Interfaces.Unsigned_64;
      Size         : Interfaces.Unsigned_64);

   --  Validate that the section flags match the permissions of the memory
   --  region identified by name.
   procedure Validate_Permission
     (Section      : Bfd.Sections.Section;
      Section_Name : String;
      Region_Name  : String;
      Read_Only    : Boolean);

end Elfcheck.Bfd_Utils;
