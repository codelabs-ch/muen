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

package Ahci.Device
is
   --  do devices initialisation. Search for attached devices and query device
   --  info.
   procedure Init;

   --  Read / Write 'Count' Sectors to Device starting with Sector 'Start'
   --  Returns number of Bytes written
   procedure RW_Sectors
      (ID      :     Port_Range;
       RW      :     Ahci.RW_Type;
       Start   :     Interfaces.Unsigned_64; --  Start Sector
       Count   :     Interfaces.Unsigned_32; --  Number of Sectors
       Address :     Interfaces.Unsigned_64; --  Buffer Address
       Ret_Val : out Ahci.Status_Type);

   procedure Discard_Sectors
      (ID      :     Port_Range;
       Start   :     Interfaces.Unsigned_64; --  Start Sector
       Count   :     Interfaces.Unsigned_32; --  Number of Sectors
       Ret_Val : out Ahci.Status_Type);

   --  Returns a Bit_Array where 'found' devices are 'True'
   procedure Get_Attached_Devices (Dev : out Bit_Array);

   --  get Size in Bytes
   function Get_Size (ID : Port_Range) return Interfaces.Unsigned_64;

   function Get_Sector_Cnt (ID : Port_Range) return Interfaces.Unsigned_64;

   function Get_Sector_Size (ID : Port_Range) return Interfaces.Unsigned_32;

end Ahci.Device;
