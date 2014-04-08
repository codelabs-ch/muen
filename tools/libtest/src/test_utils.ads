--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Test_Utils
is

   --  Compare two files byte-wise. Returns True if both files are equal.
   --  The two files are closed but not removed after comparison. Raises
   --  Open_File_Error exception if one of the given files cannot be opened.
   function Equal_Files
     (Filename1 : String;
      Filename2 : String)
      return Boolean;

   --  Return content of file given by Filename as string.
   function Read_File (Filename : String) return String;

   Open_File_Error : exception;
   IO_Error        : exception;

end Test_Utils;
