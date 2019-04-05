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
      Index :        Entry_Range;
      E     :        Entries.Table_Entry_Type)
   is
   begin
      if Table.Data (Index) /= Entries.Null_Table_Entry then
         raise Duplicate_Entry with "Table entry with index" & Index'Img
           & " already exists";
      end if;

      Table.Data (Index) := E;
      Table.Length := Table.Length + 1;
   end Add_Entry;

   -------------------------------------------------------------------------

   procedure Clear (Table : in out Page_Table_Type)
   is
   begin
      Table.Address := Interfaces.Unsigned_64'First;
      Table.Length  := 0;
      Table.Data    := Null_Entries;
   end Clear;

   -------------------------------------------------------------------------

   function Contains
     (Table : Page_Table_Type;
      Index : Entry_Range)
      return Boolean
   is
   begin
      return Table.Data (Index) /= Entries.Null_Table_Entry;
   end Contains;

   -------------------------------------------------------------------------

   function Count (Table : Page_Table_Type) return Entry_Range
   is
   begin
      return Entry_Range (Table.Length);
   end Count;

   -------------------------------------------------------------------------

   function Get_Entry
     (Table : Page_Table_Type;
      Index : Entry_Range)
      return Entries.Table_Entry_Type
   is
      Result : constant Entries.Table_Entry_Type := Table.Data (Index);
   begin
      if Result = Entries.Null_Table_Entry then
         raise Missing_Entry with "Entry with number" & Index'Img
           & " not in table";
      end if;

      return Result;
   end Get_Entry;

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
        (Index  : Entry_Range;
         TEntry : Entries.Table_Entry_Type))
   is
      Count : Natural := 0;
   begin
      for I in Table.Data'Range loop
         if Table.Data (I) /= Entries.Null_Table_Entry then
            Process (Index  => I,
                     TEntry => Table.Data (I));
            Count := Count + 1;
            if Count = Table.Length then
               return;
            end if;
         end if;
      end loop;
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
        (Index  :        Entry_Range;
         TEntry : in out Entries.Table_Entry_Type))
   is
      Count : Natural := 0;
      Tmp_Entry : Entries.Table_Entry_Type;
   begin
      for I in Table.Data'Range loop
         Tmp_Entry := Table.Data (I);
         if Tmp_Entry /= Entries.Null_Table_Entry then
            Process (Index  => I,
                     TEntry => Tmp_Entry);
            Table.Data (I) := Tmp_Entry;
            Count := Count + 1;
            if Count = Table.Length then
               return;
            end if;
            if Tmp_Entry = Entries.Null_Table_Entry then
               Table.Length := Table.Length - 1;
            end if;
         end if;
      end loop;
   end Update;

end Paging.Tables;
