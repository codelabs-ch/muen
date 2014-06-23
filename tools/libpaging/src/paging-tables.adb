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

package body Paging.Tables
is

   -------------------------------------------------------------------------

   procedure Add_Entry
     (Table : in out Page_Table_Type;
      Index :        Table_Range;
      E     :        Entries.Table_Entry_Type)
   is
   begin
      if Table.Data.Contains (Key => Index) then
         raise Duplicate_Entry with "Table entry with index" & Index'Img
           & " already exists";
      end if;

      Table.Data.Insert
        (Key      => Index,
         New_Item => E);
   end Add_Entry;

   -------------------------------------------------------------------------

   procedure Clear (Table : in out Page_Table_Type)
   is
   begin
      Table.Address := Interfaces.Unsigned_64'First;
      Table.Data.Clear;
   end Clear;

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
         TEntry : Entries.Table_Entry_Type))
   is
      --  Perform process operation for given element.
      procedure Call_Process (Position : Entries_Map_Package.Cursor);

      procedure Call_Process (Position : Entries_Map_Package.Cursor)
      is
         Index  : constant Table_Range := Entries_Map_Package.Key
           (Position => Position);
         TEntry : constant Entries.Table_Entry_Type
           := Entries_Map_Package.Element (Position => Position);
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

   -------------------------------------------------------------------------

   procedure Update
     (Table   : in out Page_Table_Type;
      Process : not null access procedure
        (Index  :        Table_Range;
         TEntry : in out Entries.Table_Entry_Type))
   is
      --  Process the given page table entry.
      procedure Call_Process (Pos : Entries_Map_Package.Cursor);

      procedure Call_Process (Pos : Entries_Map_Package.Cursor)
      is
      begin
         Table.Data.Update_Element (Position => Pos,
                                    Process  => Process);
      end Call_Process;
   begin
      Table.Data.Iterate (Process => Call_Process'Access);
   end Update;

end Paging.Tables;
