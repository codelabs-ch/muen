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

   --  Create new page table with given table number.
   function Create_Table (Number : Table_Range) return Page_Table_Type;

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
      Process : not null access procedure (TEntry : Entry_Type));

private

   package Entries_Map_Package is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => Table_Range,
      Element_Type => Entry_Type);

   type Page_Table_Type is record
      Number  : Table_Range;
      Address : Interfaces.Unsigned_64;
      Data    : Entries_Map_Package.Map;
   end record;

end Paging.Pagetable;
