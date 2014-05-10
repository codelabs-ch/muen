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
     (Map          : in out Page_Table_Map;
      Table_Number :        Table_Range;
      Entry_Index  :        Table_Range;
      Table_Entry  :        Entry_Type)
   is
      use type Tables_Map_Package.Cursor;

      --  Add entry to given table.
      procedure Add_Entry
        (Number :        Table_Range;
         Table  : in out Page_Table_Type);

      procedure Add_Entry
        (Number :        Table_Range;
         Table  : in out Page_Table_Type)
      is
         pragma Unreferenced (Number);
      begin
         Add_Entry (Table => Table,
                    Index => Entry_Index,
                    E     => Table_Entry);
      end Add_Entry;

      Pos : Tables_Map_Package.Cursor := Map.Tables.Find (Key => Table_Number);
      Ins : Boolean;
   begin

      if Pos = Tables_Map_Package.No_Element then
         Map.Tables.Insert
           (Key      => Table_Number,
            New_Item => Null_Table,
            Position => Pos,
            Inserted => Ins);
      end if;

      Map.Tables.Update_Element
        (Position => Pos,
         Process  => Add_Entry'Access);
   end Add_Entry;

   -------------------------------------------------------------------------

   procedure Add_Entry
     (Table : in out Page_Table_Type;
      Index :        Table_Range;
      E     :        Entry_Type)
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

   function Contains
     (Map          : Page_Table_Map;
      Table_Number : Table_Range;
      Entry_Index  : Table_Range)
      return Boolean
   is
   begin
      return Map.Tables.Contains (Key => Table_Number)
        and then Contains
          (Table => Map.Tables.Element (Key => Table_Number),
           Index => Entry_Index);
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

   function Get_Table_Address
     (Map          : Page_Table_Map;
      Table_Number : Table_Range)
      return Interfaces.Unsigned_64
   is
      use type Tables_Map_Package.Cursor;

      Pos : constant Tables_Map_Package.Cursor := Map.Tables.Find
        (Key => Table_Number);
   begin
      if Pos = Tables_Map_Package.No_Element then
         raise Missing_Table with "Table with number" & Table_Number'Img
           & " not in map";
      end if;

      return Tables_Map_Package.Element (Position => Pos).Address;
   end Get_Table_Address;

   -------------------------------------------------------------------------

   procedure Iterate
     (Map     : Page_Table_Map;
      Process : not null access procedure
        (Table_Number : Table_Range;
         Table        : Page_Table_Type))
   is
      --  Process the given page table.
      procedure Call_Process (Pos : Tables_Map_Package.Cursor);

      procedure Call_Process (Pos : Tables_Map_Package.Cursor)
      is
      begin
         Process
           (Table_Number => Tables_Map_Package.Key (Position => Pos),
            Table        => Tables_Map_Package.Element (Position => Pos));
      end Call_Process;
   begin
      Map.Tables.Iterate (Process => Call_Process'Access);
   end Iterate;

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

   function Length (Map : Page_Table_Map) return Natural
   is
   begin
      return Natural (Map.Tables.Length);
   end Length;

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
         TEntry : in out Entry_Type))
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

   -------------------------------------------------------------------------

   procedure Update
     (Map     : in out Page_Table_Map;
      Process : not null access procedure
        (Table_Number :        Table_Range;
         Table        : in out Page_Table_Type))
   is
      --  Process the given page table.
      procedure Call_Process (Pos : Tables_Map_Package.Cursor);

      procedure Call_Process (Pos : Tables_Map_Package.Cursor)
      is
      begin
         Map.Tables.Update_Element (Position => Pos,
                                    Process  => Process);
      end Call_Process;
   begin
      Map.Tables.Iterate (Process => Call_Process'Access);
   end Update;

end Paging.Pagetable;
