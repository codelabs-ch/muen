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

with Mutools.Utils;

with Paging.Entries;

package body Paging.Layouts
is

   --  Paging structure levels that can map a page frame.
   subtype Page_Map_Level is Paging_Level range 2 .. 4;

   --  Map page at specified paging level with given parameters by creating all
   --  necessary/lower-level paging structure entries.
   procedure Map_Page
     (Mem_Layout       : in out Memory_Layout_Type;
      Level            :        Page_Map_Level;
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

      Physical_End  : constant Interfaces.Unsigned_64
        := Physical_Address + Size;

      Physical_Addr : Interfaces.Unsigned_64 := Physical_Address;
      Virtual_Addr  : Interfaces.Unsigned_64 := Virtual_Address;
      Offset        : Interfaces.Unsigned_64 := 0;
      Level         : Page_Map_Level;
   begin
      while Physical_Addr < Physical_End loop
         if Mem_Layout.Use_Large_Pages
           and then Mem_Layout.Levels = Paging_Level'Last
           and then Physical_Addr + PDPT_Page_Size <= Physical_End
           and then Physical_Addr mod PDPT_Page_Size = 0
           and then Virtual_Addr  mod PDPT_Page_Size = 0
         then
            Level  := Mem_Layout.Levels - 2;
            Offset := PDPT_Page_Size;
         elsif Mem_Layout.Use_Large_Pages
           and then Physical_Addr + PD_Page_Size <= Physical_End
           and then Physical_Addr mod PD_Page_Size = 0
           and then Virtual_Addr  mod PD_Page_Size = 0
         then
            Level  := Mem_Layout.Levels - 1;
            Offset := PD_Page_Size;
         else
            Level  := Mem_Layout.Levels;
            Offset := Page_Size;
         end if;

         Map_Page (Mem_Layout       => Mem_Layout,
                   Level            => Level,
                   Physical_Address => Physical_Addr,
                   Virtual_Address  => Virtual_Addr,
                   Caching          => Caching,
                   Writable         => Writable,
                   Executable       => Executable);

         Physical_Addr := Physical_Addr + Offset;
         Virtual_Addr  := Virtual_Addr  + Offset;
      end loop;
   end Add_Memory_Region;

   -------------------------------------------------------------------------

   function Get_Address
     (Mem_Layout : Memory_Layout_Type)
      return Interfaces.Unsigned_64
   is
   begin
      return Tables.Get_Physical_Address
        (Table => Mem_Layout.Level_1_Table);
   end Get_Address;

   -------------------------------------------------------------------------

   function Get_Table_Count
     (Mem_Layout : Memory_Layout_Type)
      return Table_Count_Array
   is
      Table_Counts : Table_Count_Array (1 .. Mem_Layout.Levels);
   begin

      --  There is max. one level 1 table.

      if Tables.Count (Table => Mem_Layout.Level_1_Table) > 0 then
         Table_Counts (1) := 1;
      else
         Table_Counts (1) := 0;
      end if;

      for I in Paging_Level range 2 .. Mem_Layout.Levels loop
         Table_Counts (I) := Maps.Length (Map => Mem_Layout.Structures (I));
      end loop;

      return Table_Counts;
   end Get_Table_Count;

   -------------------------------------------------------------------------

   procedure Map_Page
     (Mem_Layout       : in out Memory_Layout_Type;
      Level            :        Page_Map_Level;
      Physical_Address :        Interfaces.Unsigned_64;
      Virtual_Address  :        Interfaces.Unsigned_64;
      Caching          :        Caching_Type;
      Writable         :        Boolean;
      Executable       :        Boolean)
   is
      Indexes : Table_Index_Array (1 .. Mem_Layout.Levels) := (others => 0);

      Table_Idx : Table_Range := 0;
   begin
      Get_Indexes (Address => Virtual_Address,
                   Indexes => Indexes);

      if not Tables.Contains
        (Table => Mem_Layout.Level_1_Table,
         Index => Indexes (Indexes'First))
      then
         Tables.Add_Entry
           (Table => Mem_Layout.Level_1_Table,
            Index => Indexes (Indexes'First),
            E     => Entries.Create
              (Dst_Index   => Table_Range (Indexes (Indexes'First)),
               Dst_Address => 0,
               Readable    => True,
               Writable    => True,
               Executable  => True,
               Maps_Page   => False,
               Global      => False,
               Caching     => WB));
      end if;

      for I in Paging_Level range 2 .. Level loop
         Table_Idx := Table_Idx * 512 + Table_Range (Indexes (I - 1));

         declare
            use type Entries.Table_Entry_Type;

            New_Entry : constant Entries.Table_Entry_Type
              := Entries.Create
                (Dst_Index   =>
                   (if I /= Level then
                           Table_Idx * 512 + Table_Range (Indexes (I))
                    else 0),
                 Dst_Address => (if Level = I then Physical_Address else 0),
                 Readable    => True,
                 Writable    => Level /= I or Writable,
                 Executable  => Level /= I or Executable,
                 Maps_Page   => Level = I,
                 Global      => False,
                 Caching     => (if Level = I then Caching else WB));
         begin
            if not Maps.Contains (Map          => Mem_Layout.Structures (I),
                                  Table_Number => Table_Idx,
                                  Entry_Index  => Indexes (I))
            then
               Maps.Add_Entry
                 (Map          => Mem_Layout.Structures (I),
                  Table_Number => Table_Idx,
                  Entry_Index  => Indexes (I),
                  Table_Entry  => New_Entry);
            elsif Level = I and then New_Entry /= Maps.Get_Entry
              (Map          => Mem_Layout.Structures (I),
               Table_Number => Table_Idx,
               Entry_Index  => Indexes (I))
            then
               raise Mapping_Present with "Multiple mappings of VMA "
                 & Mutools.Utils.To_Hex (Number => Virtual_Address)
                 & " with different attributes present";
            end if;
         end;
      end loop;
   end Map_Page;

   -------------------------------------------------------------------------

   procedure Serialize
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Mem_Layout  : Memory_Layout_Type;
      Serializers : Serializer_Array)
   is
      Cur_Level : Paging_Level := Serializers'First;

      --  Call serialize procedure for given table.
      procedure Handle_Table
        (Table_Number : Table_Range;
         Table        : Tables.Page_Table_Type);

      ----------------------------------------------------------------------

      procedure Handle_Table
        (Table_Number : Table_Range;
         Table        : Tables.Page_Table_Type)
      is
         pragma Unreferenced (Table_Number);
      begin
         Serializers (Cur_Level)(Stream => Stream,
                                 Table  => Table);
      end Handle_Table;
   begin
      if Tables.Count (Table => Mem_Layout.Level_1_Table) = 0 then
         return;
      end if;

      Serializers (Serializers'First)(Stream => Stream,
                                      Table  => Mem_Layout.Level_1_Table);

      for I in reverse Serializers'First + 1 .. Mem_Layout.Levels loop
         Cur_Level := I;
         Maps.Iterate (Map     => Mem_Layout.Structures (I),
                       Process => Handle_Table'Access);
      end loop;
   end Serialize;

   -------------------------------------------------------------------------

   procedure Set_Address
     (Mem_Layout : in out Memory_Layout_Type;
      Address    :        Interfaces.Unsigned_64)
   is
   begin
      Tables.Set_Physical_Address
        (Table   => Mem_Layout.Level_1_Table,
         Address => Address);
   end Set_Address;

   -------------------------------------------------------------------------

   procedure Set_Large_Page_Support
     (Mem_Layout : in out Memory_Layout_Type;
      State      :        Boolean)
   is
   begin
      Mem_Layout.Use_Large_Pages := State;
   end Set_Large_Page_Support;

   -------------------------------------------------------------------------

   procedure Update_References (Mem_Layout : in out Memory_Layout_Type)
   is
      use type Interfaces.Unsigned_64;

      Phys_Addr : Interfaces.Unsigned_64 := Tables.Get_Physical_Address
        (Table => Mem_Layout.Level_1_Table) + Page_Size;

      Cur_Level : Paging_Level;

      --  Adjust destination address of references to level 2 structures.
      procedure Adjust_Level_1
        (Index  :        Entry_Range;
         TEntry : in out Entries.Table_Entry_Type);

      --  Set physical address of each table and adjust destination address of
      --  each table entry that references a higher level table (e.g. PDE->PT).
      procedure Adjust_Tables
        (Table_Number :        Table_Range;
         Table        : in out Tables.Page_Table_Type);

      ----------------------------------------------------------------------

      procedure Adjust_Level_1
        (Index  :        Entry_Range;
         TEntry : in out Entries.Table_Entry_Type)
      is
         pragma Unreferenced (Index);

         Dst_Idx : constant Table_Range := TEntry.Get_Dst_Table_Index;
         Address : constant Interfaces.Unsigned_64
           := Maps.Get_Table_Address
             (Map          => Mem_Layout.Structures
                (Mem_Layout.Structures'First),
              Table_Number => Dst_Idx);
      begin
         TEntry.Set_Dst_Address (Address => Address);
      end Adjust_Level_1;

      ----------------------------------------------------------------------

      procedure Adjust_Tables
        (Table_Number :        Table_Range;
         Table        : in out Tables.Page_Table_Type)
      is
         pragma Unreferenced (Table_Number);

         --  Adjust destination address of given table entry.
         procedure Adjust_Entry
           (Index  :        Entry_Range;
            TEntry : in out Entries.Table_Entry_Type);

         -------------------------------------------------------------------

         procedure Adjust_Entry
           (Index  :        Entry_Range;
            TEntry : in out Entries.Table_Entry_Type)
         is
            pragma Unreferenced (Index);

            Dst_Idx : Table_Range;
            Address : Interfaces.Unsigned_64;
         begin
            if TEntry.Maps_Page then
               return;
            end if;

            Dst_Idx := TEntry.Get_Dst_Table_Index;
            Address := Maps.Get_Table_Address
              (Map          => Mem_Layout.Structures (Cur_Level + 1),
               Table_Number => Dst_Idx);

            TEntry.Set_Dst_Address (Address => Address);
         end Adjust_Entry;
      begin
         Tables.Update (Table   => Table,
                        Process => Adjust_Entry'Access);
         Tables.Set_Physical_Address (Table   => Table,
                                      Address => Phys_Addr);
         Phys_Addr := Phys_Addr + Page_Size;
      end Adjust_Tables;
   begin
      for I in reverse Paging_Level range 2 .. Mem_Layout.Levels loop
         Cur_Level := I;
         Maps.Update (Map     => Mem_Layout.Structures (I),
                      Process => Adjust_Tables'Access);
      end loop;
      Tables.Update (Table   => Mem_Layout.Level_1_Table,
                     Process => Adjust_Level_1'Access);
   end Update_References;

end Paging.Layouts;
