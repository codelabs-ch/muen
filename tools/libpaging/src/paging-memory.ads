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

with Interfaces;

with Paging.Tables;

package Paging.Memory
is

   --  A memory layout is a collection of logical to physical memory mappings
   --  managed in several levels of paging structures.
   type Memory_Layout_Type is private;

   Null_Layout : constant Memory_Layout_Type;

   --  Set physical address of memory layout paging structures.
   procedure Set_Address
     (Mem_Layout : in out Memory_Layout_Type;
      Address    :        Interfaces.Unsigned_64);

   --  Return the physical address of the memory layout.
   function Get_Address
     (Mem_Layout : Memory_Layout_Type)
      return Interfaces.Unsigned_64;

   --  Returns the number of pagetables per level.
   procedure Get_Table_Count
     (Mem_Layout :     Memory_Layout_Type;
      PML4_Count : out Natural;
      PDPT_Count : out Natural;
      PD_Count   : out Natural;
      PT_Count   : out Natural);

   --  Add memory region with specified attributes to given memory layout.
   procedure Add_Memory_Region
     (Mem_Layout       : in out Memory_Layout_Type;
      Physical_Address :        Interfaces.Unsigned_64;
      Virtual_Address  :        Interfaces.Unsigned_64;
      Size             :        Interfaces.Unsigned_64;
      Caching          :        Caching_Type;
      Writable         :        Boolean;
      Executable       :        Boolean);

   --  Calculate physical addresses of pagetables.
   procedure Set_Table_Addresses (Mem_Layout : in out Memory_Layout_Type);

   --  Set physical addresses of all paging structures and update destination
   --  addresses of table entries referencing other paging structures.
   procedure Update_References (Mem_Layout : in out Memory_Layout_Type);

   --  Serialze paging structures of given memory layout. Pagetables are
   --  processed in the order PML4 -> PTs -> PDs -> PDPTs using the
   --  specified serialization procedures.
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
         PT     : Tables.PT.Page_Table_Type));

private

   type Memory_Layout_Type is record
      PML4  : Tables.PML4.Page_Table_Type;
      PDPTs : Tables.PDPT.Page_Table_Map;
      PDs   : Tables.PD.Page_Table_Map;
      PTs   : Tables.PT.Page_Table_Map;
   end record;

   Null_Layout : constant Memory_Layout_Type := (others => <>);

end Paging.Memory;
