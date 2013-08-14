--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Skp.MSRs
is

   --  MSR bitmap as specified by Intel SDM Vol. 3C, section 24.6.9. This type
   --  encompasses the whole MSR bitmap range (read & write, low & high).
   type MSR_Bitmap_Type is private;

   Null_MSR_Bitmap : constant MSR_Bitmap_Type;

   --  Sets flag in MSR bitmap to allow access of given mode to address range
   --  specified by start and end address (inclusive).
   procedure Allow_MSRs
     (Bitmap     : in out MSR_Bitmap_Type;
      Start_Addr :        SK.Word32;
      End_Addr   :        SK.Word32;
      Mode       :        MSR_Mode_Type);

   use type Ada.Streams.Stream_Element_Offset;

   subtype MSR_Bitmap_Stream is Ada.Streams.Stream_Element_Array
     (0 .. SK.Page_Size - 1);

   --  Convert MSR bitmap to binary stream.
   function To_Stream (Bitmap : MSR_Bitmap_Type) return MSR_Bitmap_Stream;

private

   type MSR_Flag is mod 2 ** 1;
   for MSR_Flag'Size use 1;

   Allowed : constant MSR_Flag := 0;
   Denied  : constant MSR_Flag := 1;

   type MSR_Bitmap_Type is array (0 .. SK.Page_Size * 8 - 1) of MSR_Flag;
   pragma Pack (MSR_Bitmap_Type);

   for MSR_Bitmap_Type'Size use SK.Page_Size * 8;

   Null_MSR_Bitmap : constant MSR_Bitmap_Type :=
     MSR_Bitmap_Type'(others => Denied);

end Skp.MSRs;
