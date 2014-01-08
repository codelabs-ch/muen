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

with SK;

package Iobm.IO_Ports
is

   --  I/O bitmap as specified by Intel SDM Vol. 3C, section 24.6.4. This type
   --  encompasses the whole IO port range (bitmap A & B).
   type IO_Bitmap_Type is private;

   Null_IO_Bitmap : constant IO_Bitmap_Type;

   --  Sets flag in I/O bitmap to allow access to port range specified by start
   --  and end port (inclusive).
   procedure Allow_Ports
     (B          : in out IO_Bitmap_Type;
      Start_Port :        SK.Word16;
      End_Port   :        SK.Word16);

   use type Ada.Streams.Stream_Element_Offset;

   subtype IO_Bitmap_Stream is Ada.Streams.Stream_Element_Array
     (1 .. 2 * SK.Page_Size);

   --  Convert I/O bitmap to binary stream.
   function To_Stream
     (B : IO_Bitmap_Type)
      return IO_Bitmap_Stream;

private

   type Port_Flag is mod 2 ** 1;
   for Port_Flag'Size use 1;

   Allowed : constant Port_Flag := 0;
   Denied  : constant Port_Flag := 1;

   type IO_Bitmap_Type is array (SK.Word16'Range) of Port_Flag;
   pragma Pack (IO_Bitmap_Type);

   for IO_Bitmap_Type'Size use 2 * SK.Page_Size * 8;

   Null_IO_Bitmap : constant IO_Bitmap_Type :=
     IO_Bitmap_Type'(others => Denied);

end Iobm.IO_Ports;
