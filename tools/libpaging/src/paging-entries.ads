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

package Paging.Entries
is

   --  Table entry type.
   type Table_Entry_Type is tagged private;

   --  Create a pagetable entry with given attributes.
   function Create
     (Dst_Index   : Table_Range;
      Dst_Address : Interfaces.Unsigned_64;
      Present     : Boolean := True;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Maps_Page   : Boolean;
      Global      : Boolean;
      Caching     : Caching_Type)
      return Table_Entry_Type;

   --  Returns the index of the paging structure referenced by this table
   --  entry.
   function Get_Dst_Table_Index (E : Table_Entry_Type) return Table_Range;

   --  Return the address pointed to by this table entry.
   function Get_Dst_Address
     (E : Table_Entry_Type)
      return Interfaces.Unsigned_64;

   --  Set the address pointed to by this table entry.
   procedure Set_Dst_Address
     (E       : in out Table_Entry_Type;
      Address :        Interfaces.Unsigned_64);

   --  Returns True if the table entry allows user access to the mapped memory
   --  region.
   function Is_Readable (E : Table_Entry_Type) return Boolean;

   --  Returns True if the table entry allows write access to the mapped memory
   --  region.
   function Is_Writable (E : Table_Entry_Type) return Boolean;

   --  Returns True if the table entry allows code execution on the mapped
   --  memory region.
   function Is_Executable (E : Table_Entry_Type) return Boolean;

   --  Returns True if the table entry maps a physical page frame.
   function Maps_Page (E : Table_Entry_Type) return Boolean;

   --  Returns True if the table entry is a global memory mapping.
   function Is_Global (E : Table_Entry_Type) return Boolean;

   --  Returns the memory caching type of the mapped memory region.
   function Get_Caching (E : Table_Entry_Type) return Paging.Caching_Type;

private

   type Table_Entry_Type is tagged record
      Dst_Table_Index : Table_Range;
      Dst_Address     : Interfaces.Unsigned_64;
      Present         : Boolean;
      Readable        : Boolean;
      Writable        : Boolean;
      Executable      : Boolean;
      Maps_Page       : Boolean;
      Global          : Boolean;
      Caching         : Caching_Type;
   end record;

end Paging.Entries;
