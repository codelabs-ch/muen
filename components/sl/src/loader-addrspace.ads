--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Loader.Addrspace
is

   --  Memory operations are only allowed in limited, high memory. This guards
   --  the loader segments from being overwritten.

   --  Allow sizes from 1 byte to 4 GiB.
   subtype Size_Type is Interfaces.Unsigned_64 range
     1 .. 16#1_0000_0000#;

   --  Allow access to virtual memory from 4 GiB to 128 TiB.
   subtype Addr_Type is Interfaces.Unsigned_64 range
     16#1_0000_0000# .. 16#7ffe_ffff_ffff#;
   subtype Src_Addr_Type is Addr_Type range
     16#7000_0000_0000# .. 16#7ffe_ffff_ffff#;
   subtype Dst_Addr_Type is Addr_Type range
     16#1_0000_0000# .. 16#6ffe_ffff_ffff#;

   --  Set memory region with specified address and size to given pattern.
   procedure Memset
     (Address : Dst_Addr_Type;
      Size    : Size_Type;
      Pattern : Interfaces.Unsigned_8);

   --  Copy memory content with given size from source address to destination
   --  address.
   procedure Memcpy
     (Dst_Address : Dst_Addr_Type;
      Src_Address : Src_Addr_Type;
      Size        : Size_Type);

end Loader.Addrspace;
