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

package Pack.Parser
is

   type File_Format_Type is
     (Acpi_Rsdp,
      Acpi_Xsdt,
      Acpi_Fadt,
      Acpi_Dsdt,
      Bin_Raw,
      Bzimage,
      Pt,
      Iobm,
      Msrbm,
      Zp);

   type File_Entry_Type is record
      Mem_Name  : Ada.Strings.Unbounded.Unbounded_String;
      Filename  : Ada.Strings.Unbounded.Unbounded_String;
      Path      : Ada.Strings.Unbounded.Unbounded_String;
      Address   : Interfaces.Unsigned_64;
      Size      : Interfaces.Unsigned_64;
      Offset    : Interfaces.Unsigned_64;
      Format    : File_Format_Type;
   end record;

   type File_Array is array (Natural range <>) of aliased File_Entry_Type;

   --  Parse given policy and return list of files to pack.
   function Parse (Policy : String) return File_Array;

   --  Return size of resulting image in bytes.
   function Get_Image_Size (Files : File_Array) return Interfaces.Unsigned_64;

end Pack.Parser;
