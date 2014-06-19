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

package body Paging.Layouts
is

   --  Paging structure levels that can map a page frame.
   subtype Paging_Level is Positive range 2 .. 4;

   --  Map page at specified paging level with given parameters by creating all
   --  necessary/lower-level paging structure entries.
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

      Physical_End  : constant Interfaces.Unsigned_64
        := Physical_Address + Size;

      Physical_Addr : Interfaces.Unsigned_64 := Physical_Address;
      Virtual_Addr  : Interfaces.Unsigned_64 := Virtual_Address;
      Offset        : Interfaces.Unsigned_64 := 0;
      Level         : Paging_Level;
   begin
      while Physical_Addr < Physical_End loop
         if Mem_Layout.Use_Large_Pages
           and then Physical_Addr + PDPT_Page_Size <= Physical_End
           and then Physical_Addr mod PDPT_Page_Size = 0
           and then Virtual_Addr mod PDPT_Page_Size = 0
         then
            Level  := 2;
            Offset := PDPT_Page_Size;
         elsif Mem_Layout.Use_Large_Pages
           and then Physical_Addr + PD_Page_Size <= Physical_End
           and then Physical_Addr mod PD_Page_Size = 0
           and then Virtual_Addr mod PD_Page_Size = 0
         then
            Level  := 3;
            Offset := PD_Page_Size;
         else
            Level  := 4;
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
      return Pagetables.Get_Physical_Address
        (Table => Mem_Layout.Level_1_Table);
   end Get_Address;

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
      Indexes : array (1 .. 4) of Table_Range := (others => 0);
   begin
      Get_Indexes (Address    => Virtual_Address,
                   PML4_Index => Indexes (1),
                   PDPT_Index => Indexes (2),
                   PD_Index   => Indexes (3),
                   PT_Index   => Indexes (4));

      if not Pagetables.Contains
        (Table => Mem_Layout.Level_1_Table,
         Index => Indexes (Indexes'First))
      then
         Pagetables.Add_Entry
           (Table => Mem_Layout.Level_1_Table,
            Index => Indexes (Indexes'First),
            E     => Entries.Create
              (Dst_Offset  => Indexes (Indexes'First),
               Dst_Address => 0,
               Readable    => True,
               Writable    => True,
               Executable  => True,
               Maps_Page   => False,
               Global      => False,
               Caching     => WB));
      end if;

      for I in Positive range 2 .. Mem_Layout.Levels loop
         if not Maps.Contains (Map          => Mem_Layout.Structures (I),
                               Table_Number => Indexes (I - 1),
                               Entry_Index  => Indexes (I))
         then
            Maps.Add_Entry
              (Map          => Mem_Layout.Structures (I),
               Table_Number => Indexes (I - 1),
               Entry_Index  => Indexes (I),
               Table_Entry  => Entries.Create
                 (Dst_Offset  => Indexes (I),
                  Dst_Address => (if Level = I then Physical_Address else 0),
                  Readable    => True,
                  Writable    => Level /= I or Writable,
                  Executable  => Level /= I or Executable,
                  Maps_Page   => Level = I,
                  Global      => False,
                  Caching     => (if Level = I then Caching else WB)));
         end if;

         if Level = I then
            return;
         end if;
      end loop;
   end Map_Page;

   -------------------------------------------------------------------------

   procedure Set_Address
     (Mem_Layout : in out Memory_Layout_Type;
      Address    :        Interfaces.Unsigned_64)
   is
   begin
      Pagetables.Set_Physical_Address
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

end Paging.Layouts;
