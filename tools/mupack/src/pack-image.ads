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

package Pack.Image
is

   --  System image.
   type Image_Type
     (End_Address : Ada.Streams.Stream_Element_Offset) is private;

   --  Add given buffer to system image at specified address.
   procedure Add_Buffer
     (Image   : in out Image_Type;
      Buffer  :        Ada.Streams.Stream_Element_Array;
      Address :        Ada.Streams.Stream_Element_Offset);

   --  Add file with given path, address, size and offset to system image.
   procedure Add_File
     (Image   : in out Image_Type;
      Path    :        String;
      Address :        Interfaces.Unsigned_64;
      Size    :        Interfaces.Unsigned_64;
      Offset  :        Interfaces.Unsigned_64);

   --  Write system image content to file given by filename.
   procedure Write
     (Image    : Image_Type;
      Filename : String);

   Image_Error : exception;
   Write_Error : exception;

private

   use type Ada.Streams.Stream_Element_Offset;

   type Image_Type (End_Address : Ada.Streams.Stream_Element_Offset) is record
      Data : Ada.Streams.Stream_Element_Array (0 .. End_Address)
        := (others => 0);
   end record;

end Pack.Image;
