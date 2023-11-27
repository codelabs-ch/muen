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

      Pos : Tables_Map_Package.Cursor := Map.Tables.Find
        (Item => (Index => Table_Number,
                  Data  => null));
      Ins : Boolean;
   begin
      if Pos = Tables_Map_Package.No_Element then
         Map.Tables.Insert (New_Item =>
                              (Index => Table_Number,
                               Data  => new Tables.Page_Table_Type),
                            Position => Pos,
                            Inserted => Ins);
      end if;

      Tables.Add_Entry
        (Table => Tables_Map_Package.Element (Position => Pos).Data.all,
         Index => Entry_Index,
         E     => Table_Entry);
   end Add_Entry;

   -------------------------------------------------------------------------

   procedure Clear (Map : in out Page_Table_Map)
   is
      use type Tables_Map_Package.Cursor;

      Cur_Pos : Tables_Map_Package.Cursor := Map.Tables.First;
   begin
      while Cur_Pos /= Tables_Map_Package.No_Element loop
         declare
            Element  : Table_Map_Type
              := Tables_Map_Package.Element (Position => Cur_Pos);
            Next_Pos : constant Tables_Map_Package.Cursor
              := Tables_Map_Package.Next (Position => Cur_Pos);
         begin
            Free (Element.Data);
            Map.Tables.Delete (Position => Cur_Pos);
            Cur_Pos := Next_Pos;
         end;
      end loop;
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

      Pos    : constant Tables_Map_Package.Cursor := Map.Tables.Find
        (Item => (Index => Table_Number,
                  Data  => null));
      Result : Boolean := False;
   begin
      if Pos /= Tables_Map_Package.No_Element then
         Result := Tables.Contains
           (Table => Tables_Map_Package.Element (Position => Pos).Data.all,
            Index => Entry_Index);
      end if;

      return Result;
   end Contains;

   -------------------------------------------------------------------------

   procedure Finalize (Map : in out Page_Table_Map)
   is
   begin
      Clear (Map => Map);
   end Finalize;

   -------------------------------------------------------------------------

   function Get_Entry
     (Map          : Page_Table_Map;
      Table_Number : Table_Range;
      Entry_Index  : Entry_Range)
      return Entries.Table_Entry_Type
   is
      use type Tables_Map_Package.Cursor;
      Pos : constant Tables_Map_Package.Cursor := Map.Tables.Find
        (Item => (Index => Table_Number,
                  Data  => null));
   begin
      if Pos = Tables_Map_Package.No_Element then
         raise Missing_Table with "Table with number" & Table_Number'Img
           & " not in map";
      end if;

      return Tables.Get_Entry
        (Table => Tables_Map_Package.Element (Position => Pos).Data.all,
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
        (Item => (Index => Table_Number,
                  Data  => null));
   begin
      if Pos = Tables_Map_Package.No_Element then
         raise Missing_Table with "Table with number" & Table_Number'Img
           & " not in map";
      end if;

      return Tables.Get_Physical_Address
        (Table => Tables_Map_Package.Element (Position => Pos).Data.all);
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
         Elem : constant Table_Map_Type
           := Tables_Map_Package.Element (Position => Pos);
      begin
         Process (Table_Number => Elem.Index,
                  Table        => Elem.Data.all);
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
         Elem : constant Table_Map_Type
           := Tables_Map_Package.Element (Position => Pos);
      begin
         Process (Table_Number => Elem.Index,
                  Table        => Elem.Data.all);
      end Call_Process;
   begin
      Map.Tables.Iterate (Process => Call_Process'Access);
   end Update;

end Paging.Maps;
