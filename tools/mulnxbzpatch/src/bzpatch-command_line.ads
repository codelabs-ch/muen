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

with Ada.Strings.Unbounded;

package Bzpatch.Command_Line
is

   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   --  Return source file.
   function Get_File_Src return String;

   --  Retun destination file.
   function Get_File_Dst return String;

private

   File_Src, File_Dst : Ada.Strings.Unbounded.Unbounded_String;

end Bzpatch.Command_Line;
