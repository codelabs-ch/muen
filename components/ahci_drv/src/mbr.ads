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

with Ahci;
with Partitions;

package Mbr
is
   type Partition_Table_Type is record
      Count   : Integer;   -- number of valid entries
      Entries : Partitions.Partition_Array_Type;
   end record;

   Null_Partition_Table : Partition_Table_Type :=
      (Count   => Integer'Last,
       Entries => Partitions.Null_Partition_Array);

   --  Parse the MBR (in Sector 0) of the given device
   procedure Parse
      (ID         :     Ahci.Port_Range;
       Part_Table : out Partition_Table_Type);
end Mbr;
