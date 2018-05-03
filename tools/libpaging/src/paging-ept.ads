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

package Paging.EPT
is

   --  Implementation of EPT paging structures, as specified by Intel SDM
   --  Vol. 3C, section 28.3.

   procedure Serialize_PML4
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type);

   procedure Serialize_PDPT
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type);

   procedure Serialize_PD
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type);

   procedure Serialize_PT
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type);

   --  Create single PML4 entry from given stream data.
   procedure Deserialze_PML4_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type);

   --  Create single PDPT entry from given stream data.
   procedure Deserialze_PDPT_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type);

   --  Create single PD entry from given stream data.
   procedure Deserialze_PD_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type);

   --  Create single PT entry from given stream data.
   procedure Deserialze_PT_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type);

private

   --  Convert given EPT memory type numeric value to caching type
   --  representation. Raises constraint error if an invalid value is provided.
   function Cache_Mapping (EPT_Memory_Type : Natural) return Caching_Type;

end Paging.EPT;
