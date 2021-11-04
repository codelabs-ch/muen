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

      Use_Huge_Pages : constant Boolean
        := Mem_Layout.Use_Large_Pages and Mem_Layout.Levels = Paging_Level'Last;
      Page_Address_Mask : constant Interfaces.Unsigned_64
        := 16#0000fffffffff000#;

      End_Lvl : constant Paging_Level := Mem_Layout.Levels;

      Virtual_End_Addr : constant Interfaces.Unsigned_64
        := (Virtual_Address and Page_Address_Mask) + Size - 1;
      Start_Address : constant Interfaces.Unsigned_64
        := (Virtual_Address and Page_Address_Mask);
      Start_Idx : Interfaces.Unsigned_64
        := Start_Address / Paging.Page_Size;
      End_Idx   : Interfaces.Unsigned_64 := Virtual_End_Addr / Paging.Page_Size;
      Cur_Idx   : Interfaces.Unsigned_64;
      Cur_Phys_Addr : Interfaces.Unsigned_64
        := Physical_Address and Page_Address_Mask;
      Skip : Boolean;
   begin
      if End_Lvl > 3 and then Start_Idx mod (Entries_Per_Table ** 2) /= 0 then

         --  Region starts in the middle of a 1 GB page. Add the necessary
         --  PDPT table entry.

         Maps.Add_Entry
           (Map          => Mem_Layout.Structures (End_Lvl - 2),
            Table_Number => Table_Range (Start_Idx / Entries_Per_Table ** 3),
            Entry_Index  => Entry_Range
              ((Start_Idx / Entries_Per_Table ** 2) mod Entries_Per_Table),
            Table_Entry  => Entries.Create
              (Dst_Index   => Table_Range (Start_Idx / Entries_Per_Table ** 2),
               Dst_Address => 0,
               Readable    => True,
               Writable    => True,
               Executable  => True,
               Maps_Page   => False,
               Global      => False,
               Caching     => WB));
      end if;

      if Start_Idx mod Entries_Per_Table /= 0 then

         --  Region starts in the middle of a 2 MB page. Add the necessary
         --  PD table entry.

         Maps.Add_Entry
           (Map          => Mem_Layout.Structures (End_Lvl - 1),
            Table_Number => Table_Range
              (Start_Idx / Entries_Per_Table ** 2),
            Entry_Index  => Entry_Range
              ((Start_Idx / Entries_Per_Table) mod Entries_Per_Table),
            Table_Entry  => Entries.Create
              (Dst_Index   => Table_Range (Start_Idx / Entries_Per_Table),
               Dst_Address => 0,
               Readable    => True,
               Writable    => True,
               Executable  => True,
               Maps_Page   => False,
               Global      => False,
               Caching     => WB));
      end if;

      Cur_Idx := Start_Idx;
      while Cur_Idx <= End_Idx loop
         Skip := False;
         if Cur_Idx mod Entries_Per_Table = 0 then
            if End_Lvl > 3 and then Cur_Idx mod (Entries_Per_Table ** 2) = 0
            then
               if Use_Huge_Pages
                 and then (End_Idx - Cur_Idx + 1) >= Entries_Per_Table ** 2
               then

                  --  1-GB large page mapping.

                  Maps.Add_Entry
                    (Map          => Mem_Layout.Structures (End_Lvl - 2),
                     Table_Number => Table_Range
                       (Cur_Idx / Entries_Per_Table ** 3),
                     Entry_Index  => Entry_Range
                       ((Cur_Idx / Entries_Per_Table ** 2) mod Entries_Per_Table),
                     Table_Entry  => Entries.Create
                       (Dst_Index   => 0,
                        Dst_Address => Cur_Phys_Addr,
                        Readable    => True,
                        Writable    => Writable,
                        Executable  => Executable,
                        Maps_Page   => True,
                        Global      => False,
                        Caching     => Caching));

                  Cur_Idx := Cur_Idx + Entries_Per_Table ** 2;
                  Cur_Phys_Addr := Cur_Phys_Addr + PDPT_Page_Size;

                  Skip := True;
               else
                  Maps.Add_Entry
                    (Map          => Mem_Layout.Structures (End_Lvl - 2),
                     Table_Number => Table_Range
                       (Cur_Idx / Entries_Per_Table ** 3),
                     Entry_Index  => Entry_Range
                       ((Cur_Idx / Entries_Per_Table ** 2)
                        mod Entries_Per_Table),
                     Table_Entry  => Entries.Create
                       (Dst_Index   => Table_Range
                            (Cur_Idx / Entries_Per_Table ** 2),
                        Dst_Address => 0,
                        Readable    => True,
                        Writable    => True,
                        Executable  => True,
                        Maps_Page   => False,
                        Global      => False,
                        Caching     => WB));
               end if;
            end if;

            if not Skip then
               if Mem_Layout.Use_Large_Pages and then
                 (End_Idx - Cur_Idx + 1) >= Entries_Per_Table
               then

                  --  2 MB large page mapping.

                  Maps.Add_Entry
                    (Map          => Mem_Layout.Structures (End_Lvl - 1),
                     Table_Number => Table_Range
                       (Cur_Idx / Entries_Per_Table ** 2),
                     Entry_Index  => Entry_Range
                       ((Cur_Idx / Entries_Per_Table) mod Entries_Per_Table),
                     Table_Entry  => Entries.Create
                       (Dst_Index   => 0,
                        Dst_Address => Cur_Phys_Addr,
                        Readable    => True,
                        Writable    => Writable,
                        Executable  => Executable,
                        Maps_Page   => True,
                        Global      => False,
                        Caching     => Caching));

                  Cur_Idx := Cur_Idx + Entries_Per_Table;
                  Cur_Phys_Addr := Cur_Phys_Addr + PD_Page_Size;
                  Skip := True;
               else
                  Maps.Add_Entry
                    (Map          => Mem_Layout.Structures (End_Lvl - 1),
                     Table_Number => Table_Range
                       (Cur_Idx / Entries_Per_Table ** 2),
                     Entry_Index  => Entry_Range
                       ((Cur_Idx / Entries_Per_Table) mod Entries_Per_Table),
                     Table_Entry  => Entries.Create
                       (Dst_Index   => Table_Range
                            (Cur_Idx / Entries_Per_Table),
                        Dst_Address => 0,
                        Readable    => True,
                        Writable    => True,
                        Executable  => True,
                        Maps_Page   => False,
                        Global      => False,
                        Caching     => WB));
               end if;
            end if;
         end if;

         if not Skip then
            Maps.Add_Entry
              (Map          => Mem_Layout.Structures (End_Lvl),
               Table_Number => Table_Range (Cur_Idx / Entries_Per_Table),
               Entry_Index  => Entry_Range (Cur_Idx mod Entries_Per_Table),
               Table_Entry  => Entries.Create
                 (Dst_Index   => 0,
                  Dst_Address => Cur_Phys_Addr,
                  Readable    => True,
                  Writable    => Writable,
                  Executable  => Executable,
                  Maps_Page   => True,
                  Global      => False,
                  Caching     => Caching));
            Cur_Idx := Cur_Idx + 1;
            Cur_Phys_Addr := Cur_Phys_Addr + Page_Size;
         end if;
      end loop;

      Start_Idx := Start_Idx / Entries_Per_Table ** 3;
      End_Idx := End_Idx /  Entries_Per_Table ** 3;
      for Cur_Idx in Start_Idx .. End_Idx loop
         Tables.Add_Entry
           (Table => Mem_Layout.Level_1_Table,
            Index => Entry_Range (Cur_Idx mod Entries_Per_Table),
            E     => Entries.Create
              (Dst_Index   => Table_Range (Cur_Idx),
               Dst_Address => 0,
               Readable    => True,
               Writable    => True,
               Executable  => True,
               Maps_Page   => False,
               Global      => False,
               Caching     => WB));
      end loop;

   exception
      when Tables.Duplicate_Entry =>
         raise Mapping_Present with "Multiple mappings of VMA "
           & Mutools.Utils.To_Hex (Number => Virtual_Address)
           & " with different attributes present";
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

   procedure Traverse_Tables
     (Mem_Layout : Memory_Layout_Type;
      Process    : not null access procedure
        (Level       : Paging_Level;
         Table_Index : Table_Range;
         Table       : Tables.Page_Table_Type))
   is
      Cur_Level : Paging_Level := Paging_Level'First;

      --  Call process procedure for given table.
      procedure Handle_Table
        (Table_Number : Table_Range;
         Table        : Tables.Page_Table_Type);

      ----------------------------------------------------------------------

      procedure Handle_Table
        (Table_Number : Table_Range;
         Table        : Tables.Page_Table_Type)
      is
      begin
         Process (Level       => Cur_Level,
                  Table_Index => Table_Number,
                  Table       => Table);
      end Handle_Table;
   begin
      if Tables.Count (Table => Mem_Layout.Level_1_Table) = 0 then
         return;
      end if;

      Process (Level       => Paging_Level'First,
               Table_Index => Table_Range'First,
               Table       => Mem_Layout.Level_1_Table);

      for I in reverse Paging_Level'First + 1 .. Mem_Layout.Levels loop
         Cur_Level := I;
         Maps.Iterate (Map     => Mem_Layout.Structures (I),
                       Process => Handle_Table'Access);
      end loop;
   end Traverse_Tables;

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

         Dst_Idx : constant Table_Range
           := Entries.Get_Dst_Table_Index (TEntry);
         Address : constant Interfaces.Unsigned_64
           := Maps.Get_Table_Address
             (Map          => Mem_Layout.Structures
                (Mem_Layout.Structures'First),
              Table_Number => Dst_Idx);
      begin
         Entries.Set_Dst_Address (E       => TEntry,
                                  Address => Address);
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
            if Entries.Maps_Page (TEntry) then
               return;
            end if;

            Dst_Idx := Entries.Get_Dst_Table_Index (TEntry);
            Address := Maps.Get_Table_Address
              (Map          => Mem_Layout.Structures (Cur_Level + 1),
               Table_Number => Dst_Idx);

            Entries.Set_Dst_Address (E       => TEntry,
                                     Address => Address);
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
