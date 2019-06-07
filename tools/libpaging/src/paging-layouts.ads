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

with Ada.Streams;

with Paging.Tables;

private with Paging.Maps;

package Paging.Layouts
is

   --  A memory layout is a collection of logical to physical memory mappings
   --  managed in several levels of paging structures.
   type Memory_Layout_Type (Levels : Paging_Level) is private;

   Null_Layout : constant Memory_Layout_Type;

   --  Set physical address of memory layout paging structures.
   procedure Set_Address
     (Mem_Layout : in out Memory_Layout_Type;
      Address    :        Interfaces.Unsigned_64);

   --  Return the physical address of the memory layout.
   function Get_Address
     (Mem_Layout : Memory_Layout_Type)
      return Interfaces.Unsigned_64;

   --  Enable or disable large page mappings for the given memory layout.
   procedure Set_Large_Page_Support
     (Mem_Layout : in out Memory_Layout_Type;
      State      :        Boolean);

   --  Add memory region with specified attributes to given memory layout.
   procedure Add_Memory_Region
     (Mem_Layout       : in out Memory_Layout_Type;
      Physical_Address :        Interfaces.Unsigned_64;
      Virtual_Address  :        Interfaces.Unsigned_64;
      Size             :        Interfaces.Unsigned_64;
      Caching          :        Caching_Type;
      Writable         :        Boolean;
      Executable       :        Boolean);

   --  Set physical addresses of all paging structures and update destination
   --  addresses of table entries referencing other paging structures.
   procedure Update_References (Mem_Layout : in out Memory_Layout_Type);

   type Table_Count_Array is array (Paging_Level range <>) of Natural;

   --  Returns the number of pagetables per level in ascending order (i.e.
   --  first array element is level 1, etc).
   function Get_Table_Count
     (Mem_Layout : Memory_Layout_Type)
      return Table_Count_Array
     with
       Post => Get_Table_Count'Result'Length = Mem_Layout.Levels;

   type Table_Serializer is not null access procedure
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type);

   type Serializer_Array is array (Paging_Level range <>) of Table_Serializer;

   --  Serialize paging structures of given memory layout. Pagetables are
   --  processed in the order PML4 -> PTs -> PDs -> PDPTs using the
   --  specified serialization procedures. The provided serializers are used in
   --  ascending order, i.e. Serializers (1) -> level 1, etc.
   procedure Serialize
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Mem_Layout  : Memory_Layout_Type;
      Serializers : Serializer_Array)
     with
       Pre => Serializers'First = 1 and Serializers'Last = Mem_Layout.Levels;

   Mapping_Present : exception;

private

   type Tables_Array is array (Paging_Level range <>) of Maps.Page_Table_Map;

   type Memory_Layout_Type (Levels : Paging_Level) is record
      Use_Large_Pages : Boolean := True;
      Level_1_Table   : Tables.Page_Table_Type;
      Structures      : Tables_Array (2 .. Levels);
   end record;

   Null_Layout : constant Memory_Layout_Type := (Levels          => 4,
                                                 Use_Large_Pages => True,
                                                 others          => <>);

end Paging.Layouts;
