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

with Interfaces;

package Paging
is

   --  Memory caching type, see Intel SDM Vol. 3A, section 11.3.
   type Caching_Type is (UC, WC, WT, WP, WB);

   --  Supported paging modes.
   type Paging_Mode_Type is (IA32e_Mode, EPT_Mode);

   --  All paging structure types (PML4, PDPT, PD, PT) have 512 entries.
   type Entry_Range is range 0 .. 511;

   --  Paging structure index type which defines the range of paging structures
   --  per level.
   type Table_Range is range 0 .. 512 * 512 * 512 - 1;

   subtype Paging_Level is Positive range 1 .. 4;

   subtype Paging_Map_Level is Paging_Level range 2 .. 4;

   type Table_Index_Array is array (Paging_Level range <>) of Entry_Range;

   --  A PDPT entry maps a 1 GB page.
   PDPT_Page_Size : constant := 2 ** 30;

   --  A PD entry maps a 2 MB page.
   PD_Page_Size   : constant := 2 ** 21;

   --  A PT entry maps a 4 KB page.
   Page_Size      : constant := 2 ** 12;

   --  Return the paging structure indexes for a given linear address. see
   --  Intel SDM Vol. 3A, page 4-22:
   --   * PML4 index is formed by bits 39 .. 47
   --   * PDPT index is formed by bits 30 .. 38
   --   * PD   index is formed by bits 21 .. 29
   --   * PT   index is formed by bits 12 .. 20
   --  The returned paging level indexes depend on the range of the provided
   --  array:
   --   * 3-element array -> PDPT, PD and PT are returned
   --   * 4-element array -> PML4, PDPT, PD and PT are returned
   procedure Get_Indexes
     (Address :     Interfaces.Unsigned_64;
      Indexes : out Table_Index_Array);

   --  Returns the index of a paging structure at a specified paging level that
   --  is referenced by a given linear address.
   function Get_Index
     (Address : Interfaces.Unsigned_64;
      Level   : Paging_Level)
      return Entry_Range;

   --  Returns the page frame offset of a mapping at the specified paging level
   --  for the given linear address.
   function Get_Offset
     (Address : Interfaces.Unsigned_64;
      Level   : Paging_Map_Level)
      return Interfaces.Unsigned_64;

end Paging;
