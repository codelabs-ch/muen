--
--  Copyright (C) 2014  secunet Security Networks AG
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

package Dbg.Byte_Arrays
is

   subtype Byte_Array_Range is Natural range Natural'First .. Natural'Last - 1;

   type Byte_Array is array (Byte_Array_Range range <>)
     of Interfaces.Unsigned_8;

   --  Clear byte array.
   procedure Clear_Byte_Array (Data : out Byte_Array);

   --  Copy contents of source array starting from src index into destination
   --  array at given destination index.
   procedure Copy_Byte_Array
     (Src        :        Byte_Array;
      Src_First  :        Natural;
      Src_Length :        Natural;
      Dest       : in out Byte_Array;
      Index      :        Natural);

   --  Copy contents of source array starting from src index into destination
   --  array at given destination index. The destination index is incremented
   --  with the source length.
   procedure Copy_Byte_Array_Inc
     (Src        :        Byte_Array;
      Src_First  :        Natural;
      Src_Length :        Natural;
      Dest       : in out Byte_Array;
      Index      : in out Natural);

   --  Returns True if the given byte arrays are equal starting from the
   --  specified indexes and length.
   function Equal
     (Data1 : Byte_Array;
      I1    : Natural;
      Len   : Natural;
      Data2 : Byte_Array;
      I2    : Natural)
      return Boolean;

   subtype Single_Byte_Range is Positive range 1 .. 1;
   subtype Single_Byte_Array is Byte_Array (Single_Byte_Range);

end Dbg.Byte_Arrays;
