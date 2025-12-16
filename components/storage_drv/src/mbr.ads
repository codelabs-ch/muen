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

with Musinfo.Instance;

with Partitions; use Partitions;
with Storage_Interface; use Storage_Interface;
with Ports_Config;

package Mbr
is
   --  Parse the MBR (in Sector 0) of the given device
   procedure Parse
      (ID         :     Ports_Config.Port_Range;
       Part_Table : out Partition_Table_Type)
   with
      Pre => Musinfo.Instance.Is_Valid and then
             Storage_Interface.Is_Valid;

end Mbr;
