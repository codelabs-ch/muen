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

      for PML4_Idx in Table_Range range PML4_Idx_Start .. PML4_Idx_End loop
         if not Tables.PML4.Contains (Table => Mem_Layout.PML4,
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
      end loop;

      for PDPT_Idx in Table_Range range PDPT_Idx_Start .. PDPT_Idx_End loop
         if not PDPT.Contains
           (Map          => Mem_Layout.PDPTs,
            Table_Number => 0,
            Entry_Index  => PDPT_Idx)
         then
            PDPT.Add_Entry
              (Map          => Mem_Layout.PDPTs,
               Table_Number => 0,
               Entry_Index  => PDPT_Idx,
               Table_Entry  => Entries.Create
                 (Dst_Offset  => PDPT_Idx,
                  Dst_Address => (if Is_PDPT_Page then Physical_Addr else 0),
                  Readable    => True,
                  Writable    => not Is_PDPT_Page or Writable,
                  Executable  => not Is_PDPT_Page or Executable,
                  Maps_Page   => Is_PDPT_Page,
                  Global      => False,
                  Caching     => Caching));
         end if;

         if Is_PDPT_Page then
            Physical_Addr := Physical_Addr + PDPT_Page_Size;
         end if;
      end loop;

      if Is_PDPT_Page then
         return;
      end if;

      for PD_Idx in Table_Range range PD_Idx_Start .. PD_Idx_End loop
         if not PD.Contains
           (Map          => Mem_Layout.PDs,
            Table_Number => 0,
            Entry_Index  => PD_Idx)
         then
            PD.Add_Entry
              (Map          => Mem_Layout.PDs,
               Table_Number => 0,
               Entry_Index  => PD_Idx,
               Table_Entry  => Entries.Create
                 (Dst_Offset  => PD_Idx,
                  Dst_Address => (if Is_PD_Page then Physical_Addr else 0),
                  Readable    => True,
                  Writable    => not Is_PD_Page or Writable,
                  Executable  => not Is_PD_Page or Executable,
                  Maps_Page   => Is_PDPT_Page,
                  Global      => False,
                  Caching     => Caching));
         end if;

         if Is_PD_Page then
            Physical_Addr := Physical_Addr + PD_Page_Size;
         else
            for PT_Idx in Table_Range range PT_Idx_Start .. PT_Idx_End loop
               if not PT.Contains
                 (Map          => Mem_Layout.PTs,
                  Table_Number => 0,
                  Entry_Index  => PT_Idx)
               then
                  PT.Add_Entry
                    (Map          => Mem_Layout.PTs,
                     Table_Number => PD_Idx,
                     Entry_Index  => PT_Idx,
                     Table_Entry  => Entries.Create
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
         end if;
      end loop;
   end Add_Memory_Region;

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

      PDPT_Count := Tables.PDPT.Length (Map => Mem_Layout.PDPTs);
      PD_Count   := Tables.PD.Length (Map => Mem_Layout.PDs);
      PT_Count   := Tables.PT.Length (Map => Mem_Layout.PTs);
   end Get_Table_Count;

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

      Tables.PDPT.Iterate (Map     => Mem_Layout.PDPTs,
                           Process => Handle_PDPT'Access);
      Tables.PD.Iterate (Map     => Mem_Layout.PDs,
                         Process => Handle_PD'Access);
      Tables.PT.Iterate (Map     => Mem_Layout.PTs,
                         Process => Handle_PT'Access);
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

   procedure Set_Table_Addresses (Mem_Layout : in out Memory_Layout_Type)
   is
      use type Interfaces.Unsigned_64;

      Phys_Addr : Interfaces.Unsigned_64 := Tables.PML4.Get_Physical_Address
        (Table => Mem_Layout.PML4) + Page_Size;

      --  Set physical address of given PDPT.
      procedure Set_PDPT_Address
        (Table_Nr :         Table_Range;
         Table    : in out Tables.PDPT.Page_Table_Type);

      --  Set physical address of given PD.
      procedure Set_PD_Address
        (Table_Nr :        Table_Range;
         Table    : in out Tables.PD.Page_Table_Type);

      --  Set physical address of given PT.
      procedure Set_PT_Address
        (Table_Nr :        Table_Range;
         Table    : in out Tables.PT.Page_Table_Type);

      ----------------------------------------------------------------------

      procedure Set_PD_Address
        (Table_Nr :        Table_Range;
         Table    : in out Tables.PD.Page_Table_Type)
      is
         pragma Unreferenced (Table_Nr);
      begin
         Tables.PD.Set_Physical_Address (Table   => Table,
                                         Address => Phys_Addr);
         Phys_Addr := Phys_Addr + Page_Size;
      end Set_PD_Address;

      ----------------------------------------------------------------------

      procedure Set_PDPT_Address
        (Table_Nr :         Table_Range;
         Table    : in out Tables.PDPT.Page_Table_Type)
      is
         pragma Unreferenced (Table_Nr);
      begin
         Tables.PDPT.Set_Physical_Address (Table   => Table,
                                           Address => Phys_Addr);
         Phys_Addr := Phys_Addr + Page_Size;
      end Set_PDPT_Address;

      ----------------------------------------------------------------------

      procedure Set_PT_Address
        (Table_Nr :        Table_Range;
         Table    : in out Tables.PT.Page_Table_Type)
      is
         pragma Unreferenced (Table_Nr);
      begin
         Tables.PT.Set_Physical_Address (Table   => Table,
                                         Address => Phys_Addr);
         Phys_Addr := Phys_Addr + Page_Size;
      end Set_PT_Address;
   begin
      Tables.PDPT.Update (Map     => Mem_Layout.PDPTs,
                          Process => Set_PDPT_Address'Access);
      Tables.PD.Update (Map     => Mem_Layout.PDs,
                        Process => Set_PD_Address'Access);
      Tables.PT.Update (Map     => Mem_Layout.PTs,
                        Process => Set_PT_Address'Access);
   end Set_Table_Addresses;

   -------------------------------------------------------------------------

   procedure Update_References (Mem_Layout : in out Memory_Layout_Type)
   is

      --  Adjust destination address of given PML4 entry.
      procedure Adjust_PML4
        (Index  :        Table_Range;
         TEntry : in out Entries.PML4_Entry_Type);

      --  Adjust destination address of each PDPT entry.
      procedure Adjust_PDPT
        (Table_Number :        Table_Range;
         Table        : in out Tables.PDPT.Page_Table_Type);

      --  Adjust destination address of each PD entry.
      procedure Adjust_PD
        (Table_Number :        Table_Range;
         Table        : in out Tables.PD.Page_Table_Type);

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
   begin
      Tables.PML4.Update (Table   => Mem_Layout.PML4,
                          Process => Adjust_PML4'Access);
      Tables.PDPT.Update (Map     => Mem_Layout.PDPTs,
                          Process => Adjust_PDPT'Access);
      Tables.PD.Update (Map     => Mem_Layout.PDs,
                        Process => Adjust_PD'Access);
   end Update_References;

end Paging.Memory;
