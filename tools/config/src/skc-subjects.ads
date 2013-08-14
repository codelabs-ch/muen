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

with SK;

package Skc.Subjects
is

   --  Information about a subject binary.
   type Binary_Type is record
      Entry_Point   : SK.Word64;
      Stack_Address : SK.Word64;
   end record;

   --  Read and analyze subject binary. Raises Binary_Error if a check fails.
   function Read (Binary : String) return Binary_Type;

   --  Write subject binary specification to given XML file.
   procedure Write
     (XML_File : String;
      Subject  : Binary_Type);

   --  Write subject memory layout specification for a given binary with
   --  specified physical start address to given XML file.
   procedure Write_Memory_Layout
     (XML_File      : String;
      Binary        : String;
      Start_Address : SK.Word64);

   Open_Error   : exception;
   Binary_Error : exception;

end Skc.Subjects;
