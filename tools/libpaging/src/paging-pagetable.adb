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

package body Paging.Pagetable
is

   -------------------------------------------------------------------------

   procedure Add_Entry
     (Table : in out Page_Table_Type;
      Index :        Table_Range;
      E     :        Entry_Type)
   is
   begin
      Table.Data.Insert
        (Key      => Index,
         New_Item => E);
   end Add_Entry;

   -------------------------------------------------------------------------

   function Contains
     (Table : Page_Table_Type;
      Index : Table_Range)
      return Boolean
   is
   begin
      return Table.Data.Contains (Key => Index);
   end Contains;

   -------------------------------------------------------------------------

   function Count (Table : Page_Table_Type) return Table_Range
   is
   begin
      return Table_Range (Table.Data.Length);
   end Count;

   -------------------------------------------------------------------------

   function Create_Table (Number : Table_Range) return Page_Table_Type
   is
   begin
      return Page_Table_Type'
        (Number => Number,
         others => <>);
   end Create_Table;

   -------------------------------------------------------------------------

   function Get_Physical_Address
     (Table : Page_Table_Type)
      return Interfaces.Unsigned_64
   is
   begin
      return Table.Address;
   end Get_Physical_Address;

   -------------------------------------------------------------------------

   procedure Iterate
     (Table   : Page_Table_Type;
      Process : not null access procedure
        (Index  : Table_Range;
         TEntry : Entry_Type))
   is
      --  Perform process operation for given element.
      procedure Call_Process (Position : Entries_Map_Package.Cursor);

      procedure Call_Process (Position : Entries_Map_Package.Cursor)
      is
         Index  : constant Table_Range := Entries_Map_Package.Key
           (Position => Position);
         TEntry : constant Entry_Type  := Entries_Map_Package.Element
           (Position => Position);
      begin
         Process (Index  => Index,
                  TEntry => TEntry);
      end Call_Process;
   begin
      Table.Data.Iterate (Process => Call_Process'Access);
   end Iterate;

   -------------------------------------------------------------------------

   procedure Set_Physical_Address
     (Table   : in out Page_Table_Type;
      Address :        Interfaces.Unsigned_64)
   is
   begin
      Table.Address := Address;
   end Set_Physical_Address;

end Paging.Pagetable;
