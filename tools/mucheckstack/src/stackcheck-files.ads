--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

private with GNATCOLL.VFS;

package Stackcheck.Files
is

   type Path_Names is array
     (Natural range <>) of Ada.Strings.Unbounded.Unbounded_String;

   --  Returns all control-flow information files of the specified GNAT project
   --  and all its dependencies.
   function Get_Control_Flow_Info_Files (GPR_File : String) return Path_Names;

   --  Execute the specified process procedure for each of the given files.
   procedure For_Each_File
     (Files   : Path_Names;
      Process : not null access procedure (File : Ada.Text_IO.File_Type));

   IO_Error : exception;

private

   --  Convert given file array to path names.
   function To_Path_Names (Files : GNATCOLL.VFS.File_Array) return Path_Names;

end Stackcheck.Files;
