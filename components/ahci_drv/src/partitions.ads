--
--  Copyright (C) 2020 secunet Security Networks AG
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

package Partitions
is
   PARTITION_TYPE_EMPTY : constant := 16#00#;
   PARTITION_TYPE_EBR   : constant := 16#05#;

   type Partition_Element_Type is record
      Start_Lba      : Interfaces.Unsigned_64;
      Sector_Cnt     : Interfaces.Unsigned_64;
      Partition_Type : Interfaces.Unsigned_8;
   end record;

   Null_Partition_Element : Partition_Element_Type :=
      (Start_Lba      => 0,
       Sector_Cnt     => 0,
       Partition_Type => 0);

   type Partition_Array_Type
   is array (Integer range 0 .. 127)
      of Partition_Element_Type;

   Null_Partition_Array : Partition_Array_Type :=
      (others => Null_Partition_Element);
end Partitions;
