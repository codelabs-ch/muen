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
with Ada.Finalization;

with Interfaces;

package Pack.Image
is

   --  System image.
   type Image_Type
     (End_Address : Ada.Streams.Stream_Element_Offset) is limited private;

   --  Add file with given path, address, size and offset to system image.
   procedure Add_File
     (Image   : in out Image_Type;
      Path    :        String;
      Address :        Interfaces.Unsigned_64;
      Size    :        Interfaces.Unsigned_64;
      Offset  :        Interfaces.Unsigned_64);

   --  Add pattern with given size to system image at specified address.
   procedure Add_Pattern
     (Image   : in out Image_Type;
      Pattern :        Ada.Streams.Stream_Element;
      Size    :        Interfaces.Unsigned_64;
      Address :        Interfaces.Unsigned_64);

   --  Return image data at address with given size.
   function Get_Buffer
     (Image   : Image_Type;
      Address : Interfaces.Unsigned_64;
      Size    : Interfaces.Unsigned_64)
      return Ada.Streams.Stream_Element_Array;

   --  Write system image content to file given by filename.
   procedure Write
     (Image    : Image_Type;
      Filename : String);

   Image_Error : exception;
   Write_Error : exception;

private

   use type Ada.Streams.Stream_Element_Offset;

   type Stream_Access is access Ada.Streams.Stream_Element_Array;

   type Image_Type (End_Address : Ada.Streams.Stream_Element_Offset) is new
     Ada.Finalization.Limited_Controlled with record
      Data : Stream_Access := new Ada.Streams.Stream_Element_Array
        (0 .. End_Address);
   end record;

   overriding
   procedure Initialize (Image : in out Image_Type);

   overriding
   procedure Finalize (Image : in out Image_Type);

end Pack.Image;
