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

private with Ada.Containers.Ordered_Sets;
private with Ada.Finalization;
private with Ada.Unchecked_Deallocation;

with Paging.Entries;
with Paging.Tables;

package Paging.Maps
is

   --  A page table container.
   type Page_Table_Map is limited private;

   --  Returns True if the map contains a table with specified number, which in
   --  turn contains an entry with given index.
   function Contains
     (Map          : Page_Table_Map;
      Table_Number : Table_Range;
      Entry_Index  : Entry_Range)
      return Boolean;

   --  Add entry with given index to table specified by number. If the entry is
   --  already present an exception is raised.
   procedure Add_Entry
     (Map          : in out Page_Table_Map;
      Table_Number :        Table_Range;
      Entry_Index  :        Entry_Range;
      Table_Entry  :        Entries.Table_Entry_Type);

   --  Return the table entry with given index from the table specified by
   --  number. If the entry is not present an exception is raised.
   function Get_Entry
     (Map          : Page_Table_Map;
      Table_Number : Table_Range;
      Entry_Index  : Entry_Range)
      return Entries.Table_Entry_Type;

   --  Returns the physical address of the table specified by number.
   function Get_Table_Address
     (Map          : Page_Table_Map;
      Table_Number : Table_Range)
      return Interfaces.Unsigned_64;

   --  Returns the number of tables in the map.
   function Length (Map : Page_Table_Map) return Natural;

   --  Iterate over given page table map and call given process procedure for
   --  each entry. The table is modifiable.
   procedure Update
     (Map     : in out Page_Table_Map;
      Process : not null access procedure
        (Table_Number :        Table_Range;
         Table        : in out Tables.Page_Table_Type));

   --  Iterate over given page table map and call given process procedure for
   --  each entry.
   procedure Iterate
     (Map     : Page_Table_Map;
      Process : not null access procedure
        (Table_Number : Table_Range;
         Table        : Tables.Page_Table_Type));

   --  Clear page table map.
   procedure Clear (Map : in out Page_Table_Map);

   Missing_Table : exception;

private

   type Table_Pointer is access Tables.Page_Table_Type;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Tables.Page_Table_Type,
      Name   => Table_Pointer);

   type Table_Map_Type is record
      Index : Table_Range   := 0;
      Data  : Table_Pointer := null;
   end record;

   function "<" (Left, Right : Table_Map_Type) return Boolean
   is (Left.Index < Right.Index);

   function "=" (Left, Right : Table_Map_Type) return Boolean
   is (Left.Index = Right.Index);

   package Tables_Map_Package is new Ada.Containers.Ordered_Sets
     (Element_Type => Table_Map_Type);

   type Page_Table_Map is new Ada.Finalization.Limited_Controlled with record
      Tables : Tables_Map_Package.Set;
   end record;

   overriding
   procedure Finalize (Map : in out Page_Table_Map);

end Paging.Maps;
