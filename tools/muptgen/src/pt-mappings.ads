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

with Pt.Paging;

package Pt.Mappings
is

   type Paging_Type is (IA32e, EPT);

   --  A memory layout type is a collection of logical to physical memory
   --  mappings.
   type Memory_Layout_Type
     (PT_Type      : Paging_Type;
      PML4_Address : Interfaces.Unsigned_64) is private;

   --  Add memory region with specified attributes to given address space.
   procedure Add_Memory_Region
     (Mem_Layout       : in out Memory_Layout_Type;
      Physical_Address :        Interfaces.Unsigned_64;
      Virtual_Address  :        Interfaces.Unsigned_64;
      Size             :        Interfaces.Unsigned_64;
      Caching          :        Paging.Caching_Type;
      Writable         :        Boolean;
      Executable       :        Boolean);

   --  Write pagetables of the given memory layout to the specified file.
   procedure Write_Pagetables
     (Mem_Layout : Memory_Layout_Type;
      Filename   : String);

private

   type Memory_Layout_Type
     (PT_Type      : Paging_Type;
      PML4_Address : Interfaces.Unsigned_64)
   is record
      PML4 : Paging.PML4_Table_Type := Paging.Null_PML4_Table;
      PDPT : Paging.PDP_Table_Type  := Paging.Null_PDP_Table;
      PD   : Paging.PD_Table_Type   := Paging.Null_PD_Table;
      PT   : Paging.Page_Table_Type := Paging.Null_Page_Table;
   end record;

end Pt.Mappings;
