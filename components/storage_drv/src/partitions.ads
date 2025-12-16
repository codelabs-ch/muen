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

with Storage_Interface;

package Partitions
is
   PARTITION_TYPE_EMPTY      : constant := 16#00#;
   PARTITION_TYPE_PROTECTIVE : constant := 16#EE#;
   PARTITION_TYPE_EFI        : constant := 16#EF#;

   type EBR_Partition_Types_Type is array (1 .. 3) of Storage_Interface.Unsigned_8;
   EBR_Partition_Types : EBR_Partition_Types_Type := (16#05#, 16#0f#, 16#85#);

   type Partition_Element_Type is record
      Start_Lba      : Storage_Interface.Unsigned_64;
      Sector_Cnt     : Storage_Interface.Unsigned_64;
      Partition_Type : Storage_Interface.Unsigned_8;
   end record;

   Null_Partition_Element : Partition_Element_Type :=
      (Start_Lba      => 0,
       Sector_Cnt     => 0,
       Partition_Type => 0);

   subtype Partition_Array_Length is Natural range 0 .. 128;
   subtype Partition_Array_Range is Partition_Array_Length range 0 .. 127;

   type Partition_Array_Type is array (Partition_Array_Range)
     of Partition_Element_Type;

   Null_Partition_Array : Partition_Array_Type :=
      (others => Null_Partition_Element);

   type Partition_Table_Type is record
      Count   : Partitions.Partition_Array_Length; -- number of valid entries
      Entries : Partitions.Partition_Array_Type;
   end record;

   Null_Partition_Table : constant Partition_Table_Type :=
      (Count   => Partitions.Partition_Array_Length'First,
       Entries => Partitions.Null_Partition_Array);

end Partitions;
