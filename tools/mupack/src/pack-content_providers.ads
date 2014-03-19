--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Muxml;

with Pack.Image;

package Pack.Content_Providers
is

   type Param_Type (Size : Ada.Streams.Stream_Element_Offset) is record
      XML_Data : Muxml.XML_Data_Type;
      Image    : Pack.Image.Image_Type (End_Address => Size);
   end record;

   --  Add file content to system image.
   procedure Process_Files (Data : in out Param_Type);

end Pack.Content_Providers;
