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

package Paging
is

   --  Memory caching type, see Intel SDM Vol. 3A, section 11.3.
   type Caching_Type is (UC, WC, WT, WP, WB);

   --  Supported paging modes.
   type Paging_Mode_Type is (IA32e_Mode, EPT_Mode);

   --  All paging structure types (PML4, PDPT, PD, PT) have 512 entries.
   type Table_Range is range 0 .. 511;

   --  A PDPT entry maps a 1 GB page.
   PDPT_Page_Size : constant := 2 ** 31;

   --  A PD entry maps a 2 MB page.
   PD_Page_Size   : constant := 2 ** 21;

   --  A PT entry maps a 4 KB page.
   Page_Size      : constant := 2 ** 12;

end Paging;
