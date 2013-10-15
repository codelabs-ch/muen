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

private package Skp.Writers.Packer_Config
is

   --  Supported file types.
   type File_Kind is
     (Elfbinary,
      Iobitmap,
      Msrbitmap,
      Pagetable,
      Rawbinary,
      Zeropage);

   --  Add file to packer configuration.
   procedure Add_File
     (Filename : String;
      Address  : SK.Word64;
      Kind     : File_Kind);

   --  Write packer config to given directory.
   procedure Write (Dir_Name : String);

end Skp.Writers.Packer_Config;
