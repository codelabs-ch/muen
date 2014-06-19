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

private with Paging.Pagetables;
private with Paging.Maps;

package Paging.Layouts
is

   --  A memory layout is a collection of logical to physical memory mappings
   --  managed in several levels of paging structures.
   type Memory_Layout_Type (Levels : Positive) is private;

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

private

   type Tables_Array is array (Positive range <>) of Maps.Page_Table_Map;

   type Memory_Layout_Type (Levels : Positive) is record
      Use_Large_Pages : Boolean := True;
      Level_1_Table   : Pagetables.Page_Table_Type;
      Structures      : Tables_Array (2 .. Levels);
   end record;

   Null_Layout : constant Memory_Layout_Type := (Levels          => 4,
                                                 Use_Large_Pages => True,
                                                 others          => <>);

end Paging.Layouts;
