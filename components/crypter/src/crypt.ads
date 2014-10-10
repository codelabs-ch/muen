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

with SK;

package Crypt
is

   use type SK.Word16;

   subtype Data_Range is SK.Word16 range 1 .. 2048;

   type Data_Array is array (Data_Range) of SK.Byte;
   for Data_Array'Size use Integer (Data_Range'Last * 8);

   Null_Data : constant Data_Array;

   type Message_Type is record
      Size : Data_Range;
      Data : Data_Array;
   end record;
   for Message_Type'Size use (2 + 2048) * 8;

   Null_Message : constant Message_Type;

private

   Null_Data    : constant Data_Array   := Data_Array'(others => 0);
   Null_Message : constant Message_Type := Message_Type'(Size => 1,
                                                         Data => Null_Data);
end Crypt;
