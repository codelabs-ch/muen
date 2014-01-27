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

   --  Paging structure levels, that can map a page frame.
   type Paging_Level is (PDPT_Page, PD_Page, PT_Page);

   --  Map page at specified paging level by creating all necessary paging
   --  structure entries.
   procedure Map_Page
     (Mem_Layout       : in out Memory_Layout_Type;
      Level            :        Paging_Level;
      Physical_Address :        Interfaces.Unsigned_64;
      Virtual_Address  :        Interfaces.Unsigned_64;
      Caching          :        Caching_Type;
      Writable         :        Boolean;
      Executable       :        Boolean);

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

      Physical_End : constant Interfaces.Unsigned_64
        := Physical_Address + Size;

      Physical_Addr : Interfaces.Unsigned_64 := Physical_Address;
      Virtual_Addr  : Interfaces.Unsigned_64 := Virtual_Address;
      Offset        : Interfaces.Unsigned_64 := 0;
      Page          : Paging_Level;
   begin
      while Physical_Addr /= Physical_End loop
         if Physical_Addr + PDPT_Page_Size <= Physical_End
           and then Physical_Addr mod PDPT_Page_Size = 0
           and then Virtual_Addr mod PDPT_Page_Size = 0
         then
            Page := PDPT_Page;
         elsif Physical_Addr + PD_Page_Size <= Physical_End
           and then Physical_Addr mod PD_Page_Size = 0
           and then Virtual_Addr mod PD_Page_Size = 0
         then
            Page := PD_Page;
         else
            Page := PT_Page;
         end if;

         Map_Page (Mem_Layout       => Mem_Layout,
                   Level            => Page,
                   Physical_Address => Physical_Addr,
                   Virtual_Address  => Virtual_Addr,
                   Caching          => Caching,
                   Writable         => Writable,
                   Executable       => Executable);

         case Page is
            when PDPT_Page => Offset := PDPT_Page_Size;
            when PD_Page   => Offset := PD_Page_Size;
            when PT_Page   => Offset := Page_Size;
         end case;

         Physical_Addr := Physical_Addr + Offset;
         Virtual_Addr  := Virtual_Addr + Offset;
      end loop;
   end Add_Memory_Region;

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

      PDPT_Count := Tables.PDPT.Length (Map => Mem_Layout.PDPTs);
      PD_Count   := Tables.PD.Length (Map => Mem_Layout.PDs);
      PT_Count   := Tables.PT.Length (Map => Mem_Layout.PTs);
   end Get_Table_Count;

   -------------------------------------------------------------------------

   procedure Map_Page
     (Mem_Layout       : in out Memory_Layout_Type;
      Level            :        Paging_Level;
      Physical_Address :        Interfaces.Unsigned_64;
      Virtual_Address  :        Interfaces.Unsigned_64;
      Caching          :        Caching_Type;
      Writable         :        Boolean;
      Executable       :        Boolean)
   is
      use Paging.Tables;

      Is_PDPT_Page : constant Boolean := Level = PDPT_Page;
      Is_PD_Page   : constant Boolean := Level = PD_Page;

      PML4_Idx : Table_Range;
      PDPT_Idx : Table_Range;
      PD_Idx   : Table_Range;
      PT_Idx   : Table_Range;
   begin
      Get_Indexes (Address    => Virtual_Address,
                   PML4_Index => PML4_Idx,
                   PDPT_Index => PDPT_Idx,
                   PD_Index   => PD_Idx,
                   PT_Index   => PT_Idx);

      if not Tables.PML4.Contains
        (Table => Mem_Layout.PML4,
         Index => PML4_Idx)
      then
         Tables.PML4.Add_Entry
           (Table => Mem_Layout.PML4,
            Index => PML4_Idx,
            E     => Entries.Create
              (Dst_Offset  => PML4_Idx,
               Dst_Address => 0,
               Readable    => True,
               Writable    => True,
               Executable  => True,
               Maps_Page   => False,
               Global      => False,
               Caching     => WC));
      end if;

      if not PDPT.Contains
        (Map          => Mem_Layout.PDPTs,
         Table_Number => PML4_Idx,
         Entry_Index  => PDPT_Idx)
      then
         PDPT.Add_Entry
           (Map          => Mem_Layout.PDPTs,
            Table_Number => PML4_Idx,
            Entry_Index  => PDPT_Idx,
            Table_Entry  => Entries.Create
              (Dst_Offset  => PDPT_Idx,
               Dst_Address => (if Is_PDPT_Page then Physical_Address else 0),
               Readable    => True,
               Writable    => not Is_PDPT_Page or Writable,
               Executable  => not Is_PDPT_Page or Executable,
               Maps_Page   => Is_PDPT_Page,
               Global      => False,
               Caching     => Caching));
      end if;

      if Is_PDPT_Page then
         return;
      end if;

      if not PD.Contains
        (Map          => Mem_Layout.PDs,
         Table_Number => PDPT_Idx,
         Entry_Index  => PD_Idx)
      then
         PD.Add_Entry
           (Map          => Mem_Layout.PDs,
            Table_Number => PDPT_Idx,
            Entry_Index  => PD_Idx,
            Table_Entry  => Entries.Create
              (Dst_Offset  => PD_Idx,
               Dst_Address => (if Is_PD_Page then Physical_Address else 0),
               Readable    => True,
               Writable    => not Is_PD_Page or Writable,
               Executable  => not Is_PD_Page or Executable,
               Maps_Page   => Is_PD_Page,
               Global      => False,
               Caching     => Caching));
      end if;

      if Is_PD_Page then
         return;
      end if;

      if not PT.Contains
        (Map          => Mem_Layout.PTs,
         Table_Number => PD_Idx,
         Entry_Index  => PT_Idx)
      then
         PT.Add_Entry
           (Map          => Mem_Layout.PTs,
            Table_Number => PD_Idx,
            Entry_Index  => PT_Idx,
            Table_Entry  => Entries.Create
              (Dst_Offset  => PT_Idx,
               Dst_Address => Physical_Address,
               Readable    => True,
               Writable    => Writable,
               Executable  => Executable,
               Maps_Page   => True,
               Global      => False,
               Caching     => Caching));
      end if;
   end Map_Page;

   -------------------------------------------------------------------------

   procedure Serialize
     (Stream         : not null access Ada.Streams.Root_Stream_Type'Class;
      Mem_Layout     : Memory_Layout_Type;
      Serialize_PML4 : not null access procedure
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         PML4   : Tables.PML4.Page_Table_Type);
      Serialize_PDPT : not null access procedure
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         PDPT   : Tables.PDPT.Page_Table_Type);
      Serialize_PD   : not null access procedure
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         PD     : Tables.PD.Page_Table_Type);
      Serialize_PT   : not null access procedure
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         PT     : Tables.PT.Page_Table_Type))
   is
      --  Call serialize procedure for given PDPT.
      procedure Handle_PDPT
        (Table_Number : Table_Range;
         Table        : Tables.PDPT.Page_Table_Type);

      --  Call serialize procedure for given PD.
      procedure Handle_PD
        (Table_Number : Table_Range;
         Table        : Tables.PD.Page_Table_Type);

      --  Call serialize procedure for given PT.
      procedure Handle_PT
        (Table_Number : Table_Range;
         Table        : Tables.PT.Page_Table_Type);

      ----------------------------------------------------------------------

      procedure Handle_PD
        (Table_Number : Table_Range;
         Table        : Tables.PD.Page_Table_Type)
      is
         pragma Unreferenced (Table_Number);
      begin
         Serialize_PD (Stream => Stream,
                       PD     => Table);
      end Handle_PD;

      ----------------------------------------------------------------------

      procedure Handle_PDPT
        (Table_Number : Table_Range;
         Table        : Tables.PDPT.Page_Table_Type)
      is
         pragma Unreferenced (Table_Number);
      begin
         Serialize_PDPT (Stream => Stream,
                         PDPT   => Table);
      end Handle_PDPT;

      ----------------------------------------------------------------------

      procedure Handle_PT
        (Table_Number : Table_Range;
         Table        : Tables.PT.Page_Table_Type)
      is
         pragma Unreferenced (Table_Number);
      begin
         Serialize_PT (Stream => Stream,
                       PT     => Table);
      end Handle_PT;
   begin
      Serialize_PML4 (Stream => Stream,
                      PML4   => Mem_Layout.PML4);
      Tables.PT.Iterate (Map     => Mem_Layout.PTs,
                         Process => Handle_PT'Access);
      Tables.PD.Iterate (Map     => Mem_Layout.PDs,
                         Process => Handle_PD'Access);
      Tables.PDPT.Iterate (Map     => Mem_Layout.PDPTs,
                           Process => Handle_PDPT'Access);
   end Serialize;

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

   -------------------------------------------------------------------------

   procedure Update_References (Mem_Layout : in out Memory_Layout_Type)
   is
      use type Interfaces.Unsigned_64;

      Phys_Addr : Interfaces.Unsigned_64 := Tables.PML4.Get_Physical_Address
        (Table => Mem_Layout.PML4) + Page_Size;

      --  Adjust destination address of references to PDPTs.
      procedure Adjust_PML4
        (Index  :        Table_Range;
         TEntry : in out Entries.PML4_Entry_Type);

      --  Set physical address of each PDPT and adjust destination address of
      --  each PDPT entry that references a PD.
      procedure Adjust_PDPT
        (Table_Number :        Table_Range;
         Table        : in out Tables.PDPT.Page_Table_Type);

      --  Set physical address of each PD and adjust destination address of
      --  each PD entry that references a PT.
      procedure Adjust_PD
        (Table_Number :        Table_Range;
         Table        : in out Tables.PD.Page_Table_Type);

      --  Set physical address of each PT.
      procedure Adjust_PT
        (Table_Number :        Table_Range;
         Table        : in out Tables.PT.Page_Table_Type);

      ----------------------------------------------------------------------

      procedure Adjust_PD
        (Table_Number :        Table_Range;
         Table        : in out Tables.PD.Page_Table_Type)
      is
         pragma Unreferenced (Table_Number);

         --  Adjust destination address of given PD entry.
         procedure Adjust_PD_Entry
           (Index  :        Table_Range;
            TEntry : in out Entries.PD_Entry_Type);

         -------------------------------------------------------------------

         procedure Adjust_PD_Entry
           (Index  :        Table_Range;
            TEntry : in out Entries.PD_Entry_Type)
         is
            pragma Unreferenced (Index);

            Dst_Idx : Table_Range;
            Address : Interfaces.Unsigned_64;
         begin
            if TEntry.Maps_Page then
               return;
            end if;

            Dst_Idx := TEntry.Get_Dst_Offset;
            Address := Tables.PT.Get_Table_Address
              (Map          => Mem_Layout.PTs,
               Table_Number => Dst_Idx);

            TEntry.Set_Dst_Address (Address => Address);
         end Adjust_PD_Entry;
      begin
         Tables.PD.Update (Table   => Table,
                           Process => Adjust_PD_Entry'Access);
         Tables.PD.Set_Physical_Address (Table   => Table,
                                         Address => Phys_Addr);
         Phys_Addr := Phys_Addr + Page_Size;
      end Adjust_PD;

      ----------------------------------------------------------------------

      procedure Adjust_PDPT
        (Table_Number :        Table_Range;
         Table        : in out Tables.PDPT.Page_Table_Type)
      is
         pragma Unreferenced (Table_Number);

         --  Adjust destination address of given PDPT entry.
         procedure Adjust_PDPT_Entry
           (Index  :        Table_Range;
            TEntry : in out Entries.PDPT_Entry_Type);

         -------------------------------------------------------------------

         procedure Adjust_PDPT_Entry
           (Index  :        Table_Range;
            TEntry : in out Entries.PDPT_Entry_Type)
         is
            pragma Unreferenced (Index);

            Dst_Idx : Table_Range;
            Address : Interfaces.Unsigned_64;
         begin
            if TEntry.Maps_Page then
               return;
            end if;

            Dst_Idx := TEntry.Get_Dst_Offset;
            Address := Tables.PD.Get_Table_Address
              (Map          => Mem_Layout.PDs,
               Table_Number => Dst_Idx);

            TEntry.Set_Dst_Address (Address => Address);
         end Adjust_PDPT_Entry;
      begin
         Tables.PDPT.Update (Table   => Table,
                             Process => Adjust_PDPT_Entry'Access);
         Tables.PDPT.Set_Physical_Address (Table   => Table,
                                           Address => Phys_Addr);
         Phys_Addr := Phys_Addr + Page_Size;
      end Adjust_PDPT;

      ----------------------------------------------------------------------

      procedure Adjust_PML4
        (Index  :        Table_Range;
         TEntry : in out Entries.PML4_Entry_Type)
      is
         pragma Unreferenced (Index);

         Dst_Idx : constant Table_Range := TEntry.Get_Dst_Offset;
         Address : constant Interfaces.Unsigned_64
           := Tables.PDPT.Get_Table_Address
             (Map          => Mem_Layout.PDPTs,
              Table_Number => Dst_Idx);
      begin
         TEntry.Set_Dst_Address (Address => Address);
      end Adjust_PML4;

      ----------------------------------------------------------------------

      procedure Adjust_PT
        (Table_Number :        Table_Range;
         Table        : in out Tables.PT.Page_Table_Type)
      is
         pragma Unreferenced (Table_Number);
      begin
         Tables.PT.Set_Physical_Address (Table   => Table,
                                         Address => Phys_Addr);
         Phys_Addr := Phys_Addr + Page_Size;
      end Adjust_PT;
   begin
      Tables.PT.Update (Map     => Mem_Layout.PTs,
                        Process => Adjust_PT'Access);
      Tables.PD.Update (Map     => Mem_Layout.PDs,
                        Process => Adjust_PD'Access);
      Tables.PDPT.Update (Map     => Mem_Layout.PDPTs,
                          Process => Adjust_PDPT'Access);
      Tables.PML4.Update (Table   => Mem_Layout.PML4,
                          Process => Adjust_PML4'Access);
   end Update_References;

end Paging.Memory;
