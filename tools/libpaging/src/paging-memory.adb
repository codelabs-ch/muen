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

with Paging.Entries;

package body Paging.Memory
is

   --  Add entry with given index to page directory specified by number.
   procedure Add_PD_Entry
     (Mem_Layout  : in out Memory_Layout_Type;
      Table_Nr    :        Table_Range;
      Entry_Index :        Table_Range;
      TEntry      :        Entries.PD_Entry_Type);

   --  Add entry with given index to PDP table specified by number.
   procedure Add_PDPT_Entry
     (Mem_Layout  : in out Memory_Layout_Type;
      Table_Nr    :        Table_Range;
      Entry_Index :        Table_Range;
      TEntry      :        Entries.PDPT_Entry_Type);

   --  Add entry with given index to page table specified by table number.
   procedure Add_PT_Entry
     (Mem_Layout  : in out Memory_Layout_Type;
      Table_Nr    :        Table_Range;
      Entry_Index :        Table_Range;
      TEntry      :        Entries.PT_Entry_Type);

   ----------------------------------------------------------------------

   procedure Add_PD_Entry
     (Mem_Layout  : in out Memory_Layout_Type;
      Table_Nr    :        Table_Range;
      Entry_Index :        Table_Range;
      TEntry      :        Entries.PD_Entry_Type)
   is
      use type PD_Map_Package.Cursor;

      --  Add entry to given table.
      procedure Add_Entry
        (Number :        Table_Range;
         Table  : in out Tables.PD.Page_Table_Type);

      ----------------------------------------------------------------------

      procedure Add_Entry
        (Number :        Table_Range;
         Table  : in out Tables.PD.Page_Table_Type)
      is
         pragma Unreferenced (Number);
      begin
         Tables.PD.Add_Entry
           (Table => Table,
            Index => Entry_Index,
            E     => TEntry);
      end Add_Entry;

      Pos : PD_Map_Package.Cursor := Mem_Layout.PDs.Find (Key => Table_Nr);
      Ins : Boolean;
   begin

      if Pos = PD_Map_Package.No_Element then
         Mem_Layout.PDs.Insert
           (Key      => Table_Nr,
            New_Item => Tables.PD.Create_Table (Number => Table_Nr),
            Position => Pos,
            Inserted => Ins);
      end if;

      Mem_Layout.PDs.Update_Element
        (Position => Pos,
         Process  => Add_Entry'Access);
   end Add_PD_Entry;

   -------------------------------------------------------------------------

   procedure Add_PDPT_Entry
     (Mem_Layout  : in out Memory_Layout_Type;
      Table_Nr    :        Table_Range;
      Entry_Index :        Table_Range;
      TEntry      :        Entries.PDPT_Entry_Type)
   is
      use type PDPT_Map_Package.Cursor;

      --  Add entry to given table.
      procedure Add_Entry
        (Number :        Table_Range;
         Table  : in out Tables.PDPT.Page_Table_Type);

      ----------------------------------------------------------------------

      procedure Add_Entry
        (Number :        Table_Range;
         Table  : in out Tables.PDPT.Page_Table_Type)
      is
         pragma Unreferenced (Number);
      begin
         Tables.PDPT.Add_Entry
           (Table => Table,
            Index => Entry_Index,
            E     => TEntry);
      end Add_Entry;

      Pos : PDPT_Map_Package.Cursor := Mem_Layout.PDPTs.Find (Key => Table_Nr);
      Ins : Boolean;
   begin

      if Pos = PDPT_Map_Package.No_Element then
         Mem_Layout.PDPTs.Insert
           (Key      => Table_Nr,
            New_Item => Tables.PDPT.Create_Table (Number => Table_Nr),
            Position => Pos,
            Inserted => Ins);
      end if;

      Mem_Layout.PDPTs.Update_Element
        (Position => Pos,
         Process  => Add_Entry'Access);
   end Add_PDPT_Entry;

   -------------------------------------------------------------------------

   procedure Add_PT_Entry
     (Mem_Layout  : in out Memory_Layout_Type;
      Table_Nr    :        Table_Range;
      Entry_Index :        Table_Range;
      TEntry      :        Entries.PT_Entry_Type)
   is
      use type PT_Map_Package.Cursor;

      --  Add entry to given table.
      procedure Add_Entry
        (Number :        Table_Range;
         Table  : in out Tables.PT.Page_Table_Type);

      ----------------------------------------------------------------------

      procedure Add_Entry
        (Number :        Table_Range;
         Table  : in out Tables.PT.Page_Table_Type)
      is
         pragma Unreferenced (Number);
      begin
         Tables.PT.Add_Entry
           (Table => Table,
            Index => Entry_Index,
            E     => TEntry);
      end Add_Entry;

      Pos : PT_Map_Package.Cursor := Mem_Layout.PTs.Find (Key => Table_Nr);
      Ins : Boolean;
   begin

      if Pos = PT_Map_Package.No_Element then
         Mem_Layout.PTs.Insert
           (Key      => Table_Nr,
            New_Item => Tables.PT.Create_Table (Number => Table_Nr),
            Position => Pos,
            Inserted => Ins);
      end if;

      Mem_Layout.PTs.Update_Element
        (Position => Pos,
         Process  => Add_Entry'Access);
   end Add_PT_Entry;

   -------------------------------------------------------------------------

   function Get_Address
     (Mem_Layout : Memory_Layout_Type)
      return Interfaces.Unsigned_64
   is
   begin
      return Tables.PML4.Get_Physical_Address (Table => Mem_Layout.PML4);
   end Get_Address;

   -------------------------------------------------------------------------

   procedure Get_Table_Count
     (Mem_Layout :     Memory_Layout_Type;
      PML4_Count : out Natural;
      PDPT_Count : out Natural;
      PD_Count   : out Natural;
      PT_Count   : out Natural)
   is
   begin

      --  There is max. one PML4 table.

      if Tables.PML4.Count (Table => Mem_Layout.PML4) > 0 then
         PML4_Count := 1;
      else
         PML4_Count := 0;
      end if;

      PDPT_Count := Natural (Mem_Layout.PDPTs.Length);
      PD_Count   := Natural (Mem_Layout.PDs.Length);
      PT_Count   := Natural (Mem_Layout.PTs.Length);
   end Get_Table_Count;

   -------------------------------------------------------------------------

   procedure Set_Address
     (Mem_Layout : in out Memory_Layout_Type;
      Address    :        Interfaces.Unsigned_64)
   is
   begin
      Tables.PML4.Set_Physical_Address
        (Table   => Mem_Layout.PML4,
         Address => Address);
   end Set_Address;

end Paging.Memory;
