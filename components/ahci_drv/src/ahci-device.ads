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
   --  Do devices initialisation. Search for attached devices and query device.
   procedure Init;

   --  Read / Write 'Count' Sectors to the device starting at sector 'Start'.
   --  Returns number of Bytes written.
   procedure RW_Sectors
      (ID      :     Port_Range;
       RW      :     RW_Type;
       Start   :     Interfaces.Unsigned_64; --  Start Sector
       Count   :     Interfaces.Unsigned_32; --  Number of Sectors
       Address :     Interfaces.Unsigned_64; --  Buffer Address
       Ret_Val : out Status_Type);

   --  Send Discard-Command to the device to discard 'Count' sectors
   --  starting at 'Start' sector.
   procedure Discard_Sectors
      (ID      :     Port_Range;
       Start   :     Interfaces.Unsigned_64; --  Start Sector
       Count   :     Interfaces.Unsigned_32; --  Number of Sectors
       Ret_Val : out Status_Type);

   --  Returns a Bit_Array where 'found' devices are 'True'
   procedure Get_Attached_Devices (Dev : out Bit_Array);

   --  Get maximum number of Sectors per R/W/Discard request.
   function Get_Max_Sector_Count (ID : Port_Range)
      return Interfaces.Unsigned_32;

   --  Get size of the disk at port 'ID' in bytes.
   function Get_Size (ID : Port_Range) return Interfaces.Unsigned_64;

   --  Get number of sectors of the device at port 'ID'.
   function Get_Sector_Cnt (ID : Port_Range) return Interfaces.Unsigned_64;

   --  Get size of a sector in bytes of the device at port 'ID'.
   function Get_Sector_Size (ID : Port_Range) return Interfaces.Unsigned_32;

end Ahci.Device;
