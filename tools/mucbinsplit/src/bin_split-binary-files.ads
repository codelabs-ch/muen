--
--  Copyright (C) 2017  secunet Security Networks AG
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

with Bin_Split.Types;

package Bin_Split.Binary.Files is

   type File_Type is new Bfd.Files.File_Type;

   --  Write sections of input binary which correspond to a compound section
   --  (see above) to output binary.  The input binary is given by a File_Type
   --  (Descriptor), and the output binary by a file name (Output_File_Name).
   procedure Write_Section
     (Info             : Types.Section_Info;
      Output_File_Name : String;
      Descriptor       : Binary.Files.File_Type);

   --  Open a binary object file referenced to by Filename.
   --
   --  Raises Bin_Split_Error if file is no binary object, and OPEN_ERROR if
   --  file could not be opened.
   procedure Open
     (Filename   :     String;
      Descriptor : out File_Type);

end Bin_Split.Binary.Files;
