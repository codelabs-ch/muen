--
--  Copyright (C) 2018  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2018  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

private with Ada.Streams.Stream_IO;

with Interfaces;

with Paging;

package Ptwalk
is

   --  Perform address translation of the specified virtual address with the
   --  given page table.
   procedure Run
     (Table_File      : String;
      Table_Type      : Paging.Paging_Mode_Type;
      Table_Pointer   : Interfaces.Unsigned_64;
      Virtual_Address : Interfaces.Unsigned_64);

private

   --  Recursively perform page table walk for specified address at given
   --  paging level using the page table file with physical address
   --  specified by PT_Pointer. PT_Address points to the page table of the
   --  given level.
   --  Success is set to True if a valid translation is found and the
   --  translated address is returned accordingly.
   procedure Do_Walk
     (Virtual_Address :     Interfaces.Unsigned_64;
      File            :     Ada.Streams.Stream_IO.File_Type;
      PT_Pointer      :     Interfaces.Unsigned_64;
      PT_Type         :     Paging.Paging_Mode_Type;
      Level           :     Paging.Paging_Level;
      PT_Address      :     Interfaces.Unsigned_64;
      Success         : out Boolean;
      Translated_Addr : out Interfaces.Unsigned_64);

end Ptwalk;
