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

private with Ada.Containers.Ordered_Maps;

private with Paging.Tables;

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

   --  Returns True if a PML4 entry with given index exists.
   function Exists
     (Mem_Layout : Memory_Layout_Type;
      PML4_Index : Table_Range)
      return Boolean;

   --  Returns True if the specified PDPT contains an entry with given index.
   function Contains_PDPTE
     (Mem_Layout   : Memory_Layout_Type;
      Table_Number : Table_Range;
      Entry_Index  : Table_Range)
      return Boolean;

   --  Returns True if the specified PD contains an entry with given index.
   function Contains_PDE
     (Mem_Layout   : Memory_Layout_Type;
      Table_Number : Table_Range;
      Entry_Index  : Table_Range)
      return Boolean;

   --  Returns True if the specified PT contains an entry with given index.
   function Contains_PTE
     (Mem_Layout   : Memory_Layout_Type;
      Table_Number : Table_Range;
      Entry_Index  : Table_Range)
      return Boolean;

private

   use Tables.PDPT;
   use Tables.PD;
   use Tables.PT;

   package PDPT_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Table_Range,
      Element_Type => Tables.PDPT.Page_Table_Type);
   package PD_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Table_Range,
      Element_Type => Tables.PD.Page_Table_Type);
   package PT_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Table_Range,
      Element_Type => Tables.PT.Page_Table_Type);

   type Memory_Layout_Type is record
      PML4  : Tables.PML4.Page_Table_Type;
      PDPTs : PDPT_Map_Package.Map;
      PDs   : PD_Map_Package.Map;
      PTs   : PT_Map_Package.Map;
   end record;

   Null_Layout : constant Memory_Layout_Type := (others => <>);

end Paging.Memory;
