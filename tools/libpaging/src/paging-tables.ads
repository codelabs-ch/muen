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
with Paging.Pagetable;
pragma Elaborate_All (Paging.Pagetable);

package Paging.Tables
is

   --  A Page Map Level 4 table comprises 512 64-bit entries (PML4Es), see
   --  Intel SDM Vol. 3A, page 4-22.
   package PML4 is new Pagetable
     (Entry_Type  => Entries.PML4_Entry_Type,
      Table_Range => Table_Range);

   --  A page-directory pointer table comprises 512 64-bit entries (PDPTEs),
   --  see Intel SDM Vol. 3A, page 4-22.
   package PDPT is new Pagetable
     (Entry_Type  => Entries.PDPT_Entry_Type,
      Table_Range => Table_Range);

   --  A page directory comprises 512 64-bit entries (PDEs), see Intel SDM
   --  Vol. 3A, 4 - 22.
   package PD is new Pagetable
     (Entry_Type  => Entries.PD_Entry_Type,
      Table_Range => Table_Range);

   --  A page table comprises 512 64-bit entries (PTEs), see Intel SDM Vol.
   --  3A, page 4-22.
   package PT is new Pagetable
     (Entry_Type  => Entries.PT_Entry_Type,
      Table_Range => Table_Range);

end Paging.Tables;
