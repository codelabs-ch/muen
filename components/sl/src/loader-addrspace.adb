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

with Ada.Unchecked_Conversion;

with LSC.SHA256;

package body Loader.Addrspace
is

   subtype Address_Space_Range is Interfaces.Unsigned_64 range
     0 .. (Addr_Type'Last + Size_Type'Last) - Addr_Type'First;

   type Addrspace_Type is array (Address_Space_Range range <>)
     of Interfaces.Unsigned_8;

   Space : Addrspace_Type (Address_Space_Range)
   with
      Import,
      Address => System'To_Address (Addr_Type'First);

   -------------------------------------------------------------------------

   function Calculate_Hash
     (Address : Dst_Addr_Type;
      Size    : Size_Type)
      return Musinfo.Hash_Type
   is
      Bytes_In_Block : constant := LSC.SHA256.Block_Size / 8;

      subtype Source_Index is Address_Space_Range range
        0 .. Bytes_In_Block - 1;

      subtype Source_Array is Addrspace_Type (Source_Index);

      subtype Block_Range is Address_Space_Range range
        0 .. Size / Bytes_In_Block - 1;

      --  Convert 512 bits in source array to SHA256 block.
      function To_SHA256_Block is new Ada.Unchecked_Conversion
        (Source => Source_Array,
         Target => LSC.SHA256.Block_Type);

      --  Convert SHA256 hash to musinfo hash.
      function To_Musinfo_Hash is new Ada.Unchecked_Conversion
        (Source => LSC.SHA256.SHA256_Hash_Type,
         Target => Musinfo.Hash_Type);

      DA  : constant Address_Space_Range := Address - Addr_Type'First;
      Ctx : LSC.SHA256.Context_Type
        := LSC.SHA256.SHA256_Context_Init;
   begin
      for I in Block_Range'Range loop
         declare
            Idx : constant Address_Space_Range := DA + I * Bytes_In_Block;
         begin
            LSC.SHA256.Context_Update
              (Context => Ctx,
               Block   => To_SHA256_Block
                 (Space (Idx .. Idx + Source_Index'Last)));
         end;
      end loop;

      --  Enforced by policy: Input region is always dividable by block size,
      --  therefore we finalize with Null_Block.

      LSC.SHA256.Context_Finalize (Context => Ctx,
                                   Block   => LSC.SHA256.Null_Block,
                                   Length  => 0);

      declare
         LSC_H  : constant LSC.SHA256.SHA256_Hash_Type
           := LSC.SHA256.SHA256_Get_Hash (Context => Ctx);
         Result : constant Musinfo.Hash_Type
           := To_Musinfo_Hash (LSC_H);
      begin
         return Result;
      end;
   end Calculate_Hash;

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
