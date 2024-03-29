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

with Paging.Tables;
with Paging.Entries;

package Paging.IA32e
is

   --  See Intel SDM Vol. 3A, "4.5 4-Level Paging", Page 4-22 for more
   --  information about 4-Level Paging (formerly called "IA-32e Paging").

   --  A Page Map Level 4 table comprises 512 64-bit entries (PML4Es).
   procedure Serialize_PML4
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type);

   --  A page directory pointer table comprises 512 64-bit entries (PDPTEs).
   procedure Serialize_PDPT
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type);

   --  A page directory comprises 512 64-bit entries (PDEs).
   procedure Serialize_PD
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type);

   --  A page table comprises 512 64-bit entries (PTEs).
   procedure Serialize_PT
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type);

   --  Create single PML4 entry from given stream data.
   procedure Deserialize_PML4_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type);

   --  Create single PDPT entry from given stream data.
   procedure Deserialize_PDPT_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type);

   --  Create single PD entry from given stream data.
   procedure Deserialize_PD_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type);

   --  Create single PT entry from given stream data.
   procedure Deserialize_PT_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type);

private

   --  Convert given IA-32e memory type numeric value to caching type
   --  representation. Raises constraint error if an invalid value is provided.
   function Cache_Mapping (IA32E_Mem_Type : Natural) return Caching_Type;

end Paging.IA32e;
