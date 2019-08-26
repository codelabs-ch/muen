--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with Mutools.Constants;

package Iobm.IO_Ports
is

   --  I/O bitmap as specified by Intel SDM Vol. 3C, "24.6.4 I/O-Bitmap
   --  Addresses". This type encompasses the whole IO port range
   --  (bitmap A & B).
   type IO_Bitmap_Type is private;

   Null_IO_Bitmap : constant IO_Bitmap_Type;

   --  Sets flag in I/O bitmap to allow access to port range specified by start
   --  and end port (inclusive).
   procedure Allow_Ports
     (B          : in out IO_Bitmap_Type;
      Start_Port :        Interfaces.Unsigned_16;
      End_Port   :        Interfaces.Unsigned_16);

   use type Ada.Streams.Stream_Element_Offset;

   subtype IO_Bitmap_Stream is Ada.Streams.Stream_Element_Array
     (1 .. 2 * Mutools.Constants.Page_Size);

   --  Convert I/O bitmap to binary stream.
   function To_Stream
     (B : IO_Bitmap_Type)
      return IO_Bitmap_Stream;

private

   type Port_Flag is mod 2 ** 1;
   for Port_Flag'Size use 1;

   Allowed : constant Port_Flag := 0;
   Denied  : constant Port_Flag := 1;

   type IO_Bitmap_Type is array (Interfaces.Unsigned_16'Range) of Port_Flag;
   pragma Pack (IO_Bitmap_Type);

   for IO_Bitmap_Type'Size use 2 * Mutools.Constants.Page_Size * 8;

   Null_IO_Bitmap : constant IO_Bitmap_Type :=
     IO_Bitmap_Type'(others => Denied);

end Iobm.IO_Ports;
