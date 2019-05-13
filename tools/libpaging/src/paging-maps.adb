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

package body Paging.Maps
is

   -------------------------------------------------------------------------

   procedure Add_Entry
     (Map          : in out Page_Table_Map;
      Table_Number :        Table_Range;
      Entry_Index  :        Entry_Range;
      Table_Entry  :        Entries.Table_Entry_Type)
   is
      use type Tables_Map_Package.Cursor;

      --  Add entry to given table.
      procedure Add_Entry
        (Number :        Table_Range;
         Table  : in out Tables.Page_Table_Type);

      procedure Add_Entry
        (Number :        Table_Range;
         Table  : in out Tables.Page_Table_Type)
      is
         pragma Unreferenced (Number);
      begin
         Tables.Add_Entry (Table => Table,
                           Index => Entry_Index,
                           E     => Table_Entry);
      end Add_Entry;

      Pos : Tables_Map_Package.Cursor := Map.Tables.Find (Key => Table_Number);
      Ins : Boolean;
   begin
      if Pos = Tables_Map_Package.No_Element then
         Map.Tables.Insert
           (Key      => Table_Number,
            New_Item => Tables.Null_Table,
            Position => Pos,
            Inserted => Ins);
      end if;

      Map.Tables.Update_Element
        (Position => Pos,
         Process  => Add_Entry'Access);
   end Add_Entry;

   -------------------------------------------------------------------------

   procedure Clear (Map : in out Page_Table_Map)
   is
   begin
      Map.Tables.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Contains
     (Map          : Page_Table_Map;
      Table_Number : Table_Range;
      Entry_Index  : Entry_Range)
      return Boolean
   is
      use type Tables_Map_Package.Cursor;

      Pos    : constant Tables_Map_Package.Cursor
        := Map.Tables.Find (Key => Table_Number);
      Result : Boolean := False;

      --  Check if entry with 'Entry_Index' is present in table.
      procedure Check_Entry_Presence
        (Key     : Table_Range;
         Element : Tables.Page_Table_Type);

      ----------------------------------------------------------------------

      procedure Check_Entry_Presence
        (Key     : Table_Range;
         Element : Tables.Page_Table_Type)
      is
         pragma Unreferenced (Key);
      begin
         Result := Tables.Contains
           (Table => Element,
            Index => Entry_Index);
      end Check_Entry_Presence;
   begin
      if Pos /= Tables_Map_Package.No_Element then
         Tables_Map_Package.Query_Element
           (Position => Pos,
            Process  => Check_Entry_Presence'Access);
      end if;

      return Result;
   end Contains;

   -------------------------------------------------------------------------

   function Get_Entry
     (Map          : Page_Table_Map;
      Table_Number : Table_Range;
      Entry_Index  : Entry_Range)
      return Entries.Table_Entry_Type
   is
      use type Tables_Map_Package.Cursor;

      Pos : constant Tables_Map_Package.Cursor := Map.Tables.Find
        (Key => Table_Number);
   begin
      if Pos = Tables_Map_Package.No_Element then
         raise Missing_Table with "Table with number" & Table_Number'Img
           & " not in map";
      end if;

      return Tables.Get_Entry
        (Table => Tables_Map_Package.Element (Position => Pos),
         Index => Entry_Index);
   end Get_Entry;

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

      return Tables.Get_Physical_Address
        (Table => Tables_Map_Package.Element (Position => Pos));
   end Get_Table_Address;

   -------------------------------------------------------------------------

   procedure Iterate
     (Map     : Page_Table_Map;
      Process : not null access procedure
        (Table_Number : Table_Range;
         Table        : Tables.Page_Table_Type))
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

   function Length (Map : Page_Table_Map) return Natural
   is
   begin
      return Natural (Map.Tables.Length);
   end Length;

   -------------------------------------------------------------------------

   procedure Update
     (Map     : in out Page_Table_Map;
      Process : not null access procedure
        (Table_Number :        Table_Range;
         Table        : in out Tables.Page_Table_Type))
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

end Paging.Maps;
