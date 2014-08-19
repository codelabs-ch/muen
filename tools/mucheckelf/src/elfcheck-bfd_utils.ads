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

with Bfd.Files;
with Bfd.Sections;

package Elfcheck.Bfd_Utils
is

   --  Open file given by filename. If the file could not be opened or the file
   --  is not in the expected format, an exception is raised.
   procedure Open
     (Filename   :     String;
      Descriptor : out Bfd.Files.File_Type);

   --  Get section by name from given file descriptor. If the requested section
   --  is not found, an exception is raised.
   function Get_Section
     (Descriptor : Bfd.Files.File_Type;
      Name       : String)
      return Bfd.Sections.Section;

end Elfcheck.Bfd_Utils;
