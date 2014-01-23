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

   procedure Add_Memory_Region
     (Mem_Layout       : in out Memory_Layout_Type;
      Physical_Address :        Interfaces.Unsigned_64;
      Virtual_Address  :        Interfaces.Unsigned_64;
      Size             :        Interfaces.Unsigned_64;
      Caching          :        Caching_Type;
      Writable         :        Boolean;
      Executable       :        Boolean)
   is
      use type Interfaces.Unsigned_64;
      use Paging.Tables;

      Is_PDPT_Page : constant Boolean := Size mod PDPT_Page_Size = 0;
      Is_PD_Page   : constant Boolean := Size mod PD_Page_Size   = 0;
      Virt_End     : constant Interfaces.Unsigned_64
        := Virtual_Address + Size - 1;

      PML4_Idx_Start, PML4_Idx_End : Table_Range;
      PDPT_Idx_Start, PDPT_Idx_End : Table_Range;
      PD_Idx_Start, PD_Idx_End     : Table_Range;
      PT_Idx_Start, PT_Idx_End     : Table_Range;

      --  Physical start address of PDPT paging structure(s).
      PDPT_Addr : Interfaces.Unsigned_64;
      --  Physical start address of PD paging structure(s).
      PD_Addr   : Interfaces.Unsigned_64;
      --  Physical start address of PT paging structure(s).
      PT_Addr   : Interfaces.Unsigned_64;

      Physical_Addr : Interfaces.Unsigned_64 := Physical_Address;
   begin
      Get_Indexes (Address    => Virtual_Address,
                   PML4_Index => PML4_Idx_Start,
                   PDPT_Index => PDPT_Idx_Start,
                   PD_Index   => PD_Idx_Start,
                   PT_Index   => PT_Idx_Start);
      Get_Indexes (Address    => Virt_End,
                   PML4_Index => PML4_Idx_End,
                   PDPT_Index => PDPT_Idx_End,
                   PD_Index   => PD_Idx_End,
                   PT_Index   => PT_Idx_End);

      PDPT_Addr := PML4.Get_Physical_Address (Table => Mem_Layout.PML4) +
        (Interfaces.Unsigned_64 (PML4_Idx_End) + 1) * Page_Size;
      PD_Addr   := PDPT_Addr +
        (Interfaces.Unsigned_64 (PDPT_Idx_End) + 1) * Page_Size;
      PT_Addr   := PD_Addr +
        (Interfaces.Unsigned_64 (PD_Idx_End) + 1) * Page_Size;

      for PML4_Idx in Table_Range range PML4_Idx_Start .. PML4_Idx_End loop
         if not Tables.PML4.Contains (Table => Mem_Layout.PML4,
                                      Index => PML4_Idx)
         then
            Tables.PML4.Add_Entry
              (Table => Mem_Layout.PML4,
               Index => PML4_Idx,
               E     => Entries.Create
                 (Dst_Offset  => PML4_Idx,
                  Dst_Address => PDPT_Addr +
                    Interfaces.Unsigned_64 (PML4_Idx) * Page_Size,
                  Readable    => True,
                  Writable    => True,
                  Executable  => True,
                  Maps_Page   => False,
                  Global      => False,
                  Caching     => WC));
         end if;
      end loop;

      for PDPT_Idx in Table_Range range PDPT_Idx_Start .. PDPT_Idx_End loop
         if Is_PDPT_Page then
            PD_Addr := Physical_Addr + Interfaces.Unsigned_64
              (PDPT_Idx - PDPT_Idx_Start) * PDPT_Page_Size;
         else
            PD_Addr := PD_Addr + Interfaces.Unsigned_64
              (PDPT_Idx - PDPT_Idx_Start) * Page_Size;
         end if;

         if not Contains_PDPTE
           (Mem_Layout   => Mem_Layout,
            Table_Number => 0,
            Entry_Index  => PDPT_Idx)
         then
            Add_PDPT_Entry
              (Mem_Layout  => Mem_Layout,
               Table_Nr    => 0,
               Entry_Index => PDPT_Idx,
               TEntry      => Entries.Create
                 (Dst_Offset  => PDPT_Idx,
                  Dst_Address => PD_Addr,
                  Readable    => True,
                  Writable    => not Is_PDPT_Page or Writable,
                  Executable  => not Is_PDPT_Page or Executable,
                  Maps_Page   => Is_PDPT_Page,
                  Global      => False,
                  Caching     => Caching));
         end if;
      end loop;

      if Is_PDPT_Page then
         return;
      end if;

      for PD_Idx in Table_Range range PD_Idx_Start .. PD_Idx_End loop
         if Is_PD_Page then
            PT_Addr := Physical_Addr + Interfaces.Unsigned_64
              (PD_Idx - PD_Idx_Start) * PD_Page_Size;
         else
            PT_Addr := PT_Addr + Interfaces.Unsigned_64
              (PD_Idx - PD_Idx_Start) * Page_Size;
         end if;

         if not Contains_PDE
           (Mem_Layout   => Mem_Layout,
            Table_Number => 0,
            Entry_Index  => PD_Idx)
         then
            Add_PD_Entry
              (Mem_Layout  => Mem_Layout,
               Table_Nr    => 0,
               Entry_Index => PD_Idx,
               TEntry      => Entries.Create
                 (Dst_Offset  => PD_Idx,
                  Dst_Address => PT_Addr,
                  Readable    => True,
                  Writable    => not Is_PD_Page or Writable,
                  Executable  => not Is_PD_Page or Executable,
                  Maps_Page   => Is_PDPT_Page,
                  Global      => False,
                  Caching     => Caching));
         end if;
      end loop;

      if Is_PD_Page then
         return;
      end if;

      for PT_Idx in Table_Range range PT_Idx_Start .. PT_Idx_End loop
         if not Contains_PTE
           (Mem_Layout   => Mem_Layout,
            Table_Number => 0,
            Entry_Index  => PT_Idx)
         then
            Add_PT_Entry
              (Mem_Layout  => Mem_Layout,
               Table_Nr    => 0,
               Entry_Index => PT_Idx,
               TEntry      => Entries.Create
                 (Dst_Offset  => PT_Idx,
                  Dst_Address => Physical_Addr,
                  Readable    => True,
                  Writable    => Writable,
                  Executable  => Executable,
                  Maps_Page   => True,
                  Global      => False,
                  Caching     => Caching));
         end if;

         Physical_Addr := Physical_Addr + Page_Size;
      end loop;
   end Add_Memory_Region;

   -------------------------------------------------------------------------

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

   function Contains_PDE
     (Mem_Layout   : Memory_Layout_Type;
      Table_Number : Table_Range;
      Entry_Index  : Table_Range)
      return Boolean
   is
   begin
      return Mem_Layout.PDs.Contains (Key => Table_Number)
        and then Tables.PD.Contains
          (Table => Mem_Layout.PDs.Element (Key => Table_Number),
           Index => Entry_Index);
   end Contains_PDE;

   -------------------------------------------------------------------------

   function Contains_PDPTE
     (Mem_Layout   : Memory_Layout_Type;
      Table_Number : Table_Range;
      Entry_Index  : Table_Range)
      return Boolean
   is
   begin
      return Mem_Layout.PDPTs.Contains (Key => Table_Number)
        and then Tables.PDPT.Contains
          (Table => Mem_Layout.PDPTs.Element (Key => Table_Number),
           Index => Entry_Index);
   end Contains_PDPTE;

   -------------------------------------------------------------------------

   function Contains_PTE
     (Mem_Layout   : Memory_Layout_Type;
      Table_Number : Table_Range;
      Entry_Index  : Table_Range)
      return Boolean
   is
   begin
      return Mem_Layout.PTs.Contains (Key => Table_Number)
        and then Tables.PT.Contains
          (Table => Mem_Layout.PTs.Element (Key => Table_Number),
           Index => Entry_Index);
   end Contains_PTE;

   -------------------------------------------------------------------------

   function Exists
     (Mem_Layout : Memory_Layout_Type;
      PML4_Index : Table_Range)
      return Boolean
   is
   begin
      return Tables.PML4.Contains
        (Table => Mem_Layout.PML4,
         Index => PML4_Index);
   end Exists;

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
