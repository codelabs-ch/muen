--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SK;

--  Generated definitions may be longer than 79 characters. Switch off this
--  check.
pragma Style_Checks ("-m");

package Skp.Packer_Config is

   type File_Kind is
     (Elfbinary,
      Iobitmap,
      Msrbitmap,
      Pagetable,
      Rawbinary,
      Zeropage,
      Acpitable,
      Bzimagebinary);

   type File_Type is record
      Path             : Unbounded_String;
      Physical_Address : SK.Word64;
      Kind             : File_Kind;
   end record;

   type File_Array is array (1 .. __file_count__) of File_Type;

   Files : constant File_Array := (
__files__);

end Skp.Packer_Config;
