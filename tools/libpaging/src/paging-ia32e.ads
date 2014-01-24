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

with Ada.Streams;

with Paging.Entries;
with Paging.Tables;

package Paging.IA32e
is

   --  Page Map Level 4 entry, see Intel SDM Vol. 3A, page 4-28.
   function To_Unsigned64
     (E : Entries.PML4_Entry_Type)
      return Interfaces.Unsigned_64;

   --  A Page Map Level 4 table comprises 512 64-bit entries (PML4Es), see
   --  Intel SDM Vol. 3A, page 4-22.
   procedure Serialize
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      PML4   : Tables.PML4.Page_Table_Type);

   --  Page directory pointer table entry, see Intel SDM Vol. 3A, page 4-28.
   function To_Unsigned64
     (E : Entries.PDPT_Entry_Type)
      return Interfaces.Unsigned_64;

   --  A page directory pointer table comprises 512 64-bit entries (PDPTEs),
   --  see Intel SDM Vol. 3A, page 4-22.
   procedure Serialize
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      PDPT   : Tables.PDPT.Page_Table_Type);

   --  Page directory entry, see Intel SDM Vol. 3A, page 4-28.
   function To_Unsigned64
     (E : Entries.PD_Entry_Type)
      return Interfaces.Unsigned_64;

   --  A page directory comprises 512 64-bit entries (PDEs), see Intel SDM Vol.
   --  3A, page 4-22.
   procedure Serialize
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      PD     : Tables.PD.Page_Table_Type);

   --  Page-table entry, see Intel SDM Vol. 3A, page 4-28.
   function To_Unsigned64
     (E : Entries.PT_Entry_Type)
      return Interfaces.Unsigned_64;

   --  A page table comprises 512 64-bit entries (PDEs), see Intel SDM Vol. 3A,
   --  page 4-22.
   procedure Serialize
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      PT     : Tables.PT.Page_Table_Type);

end Paging.IA32e;
