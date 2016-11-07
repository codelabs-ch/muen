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

with System;

package body Loader.Addrspace
is

   use type Interfaces.Unsigned_64;

   subtype Address_Space_Range is Interfaces.Unsigned_64 range
     0 .. (Addr_Type'Last + Size_Type'Last) - Addr_Type'First;

   type Addrspace_Type is array (Address_Space_Range) of Interfaces.Unsigned_8;

   Space : Addrspace_Type
   with
      Import,
      Address => System'To_Address (Addr_Type'First);

   -------------------------------------------------------------------------

   procedure Memcpy
     (Dst_Address : Dst_Addr_Type;
      Src_Address : Src_Addr_Type;
      Size        : Size_Type)
   is
      SA : constant Address_Space_Range := Src_Address - Addr_Type'First;
      DA : constant Address_Space_Range := Dst_Address - Addr_Type'First;
   begin
      for I in 0 .. Size - 1 loop
         Space (DA + I) := Space (SA + I);
      end loop;
   end Memcpy;

   -------------------------------------------------------------------------

   procedure Memset
     (Address : Dst_Addr_Type;
      Size    : Size_Type;
      Pattern : Interfaces.Unsigned_8)
   is
      DA : constant Address_Space_Range := Address - Addr_Type'First;
   begin
      for I in 0 .. Size - 1 loop
         Space (DA + I) := Pattern;
      end loop;
   end Memset;

end Loader.Addrspace;
