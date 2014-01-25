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

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Ordered_Maps;

with Paging.Entries;

--  Page table structure storing a specified type of pagetable entries.
generic

   --  Type of pagetable entries.
   type Entry_Type (<>) is new Paging.Entries.Table_Entry_Type with private;

   --  Range of pagetable entries.
   type Table_Range is range <>;

package Paging.Pagetable
is

   --  Page table storing up to 512 entries.
   type Page_Table_Type is private;

   Null_Table : constant Page_Table_Type;

   --  Add given entry to pagetable.
   procedure Add_Entry
     (Table : in out Page_Table_Type;
      Index :        Table_Range;
      E     :        Entry_Type);

   --  Returns the number of entries present in the table.
   function Count (Table : Page_Table_Type) return Table_Range;

   --  Returns true if an entry with given index exists.
   function Contains
     (Table : Page_Table_Type;
      Index : Table_Range)
      return Boolean;

   --  Returns the physical memory address of the pagetable.
   function Get_Physical_Address
     (Table : Page_Table_Type)
      return Interfaces.Unsigned_64;

   --  Sets the physical memory address of the pagetable.
   procedure Set_Physical_Address
     (Table   : in out Page_Table_Type;
      Address :        Interfaces.Unsigned_64);

   --  Iterate over given page table and call given process procedure for each
   --  entry.
   procedure Iterate
     (Table   : Page_Table_Type;
      Process : not null access procedure
        (Index  : Table_Range;
         TEntry : Entry_Type));

   --  Iterate over specified page table and call given process procedure for
   --  each entry. The table entry is modifiable.
   procedure Update
     (Table   : in out Page_Table_Type;
      Process : not null access procedure
        (Index  :        Table_Range;
         TEntry : in out Entry_Type));

   --  A page table container.
   type Page_Table_Map is private;

   --  Returns True if the map contains a table with specified number, which in
   --  turn contains an entry with given index.
   function Contains
     (Map          : Page_Table_Map;
      Table_Number : Table_Range;
      Entry_Index  : Table_Range)
      return Boolean;

   --  Add entry with given index to table specified by number. If the entry is
   --  already present an exception is raised.
   procedure Add_Entry
     (Map          : in out Page_Table_Map;
      Table_Number :        Table_Range;
      Entry_Index  :        Table_Range;
      Table_Entry  :        Entry_Type);

   --  Returns the number of tables in the map.
   function Length (Map : Page_Table_Map) return Natural;

   --  Iterate over given page table map and call given process procedure for
   --  each entry. The table is modifiable.
   procedure Update
     (Map     : in out Page_Table_Map;
      Process : not null access procedure
        (Table_Number :        Table_Range;
         Table        : in out Page_Table_Type));

   --  Iterate over given page table map and call given process procedure for
   --  each entry.
   procedure Iterate
     (Map     : Page_Table_Map;
      Process : not null access procedure
        (Table_Number : Table_Range;
         Table        : Page_Table_Type));

   Duplicate_Entry : exception;

private

   package Entries_Map_Package is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => Table_Range,
      Element_Type => Entry_Type);

   type Page_Table_Type is record
      Address : Interfaces.Unsigned_64;
      Data    : Entries_Map_Package.Map;
   end record;

   Null_Table : constant Page_Table_Type
     := (Address => 0,
         Data    => Entries_Map_Package.Empty_Map);

   package Tables_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Table_Range,
      Element_Type => Page_Table_Type);

   type Page_Table_Map is record
      Tables : Tables_Map_Package.Map;
   end record;

end Paging.Pagetable;
