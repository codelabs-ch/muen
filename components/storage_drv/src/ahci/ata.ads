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

with Musinfo.Instance;

with Ahci;
with Ahci.Device;
with Ports_Config;
with Storage_Interface;

package Ata
is
   procedure Discard_Sectors
      (ID      :     Ports_Config.Port_Range;
       Start   :     Interfaces.Unsigned_64;
       Count   :     Interfaces.Unsigned_32;
       Ret_Val : out Storage_Interface.Status_Type)
   with
      Pre => Musinfo.Instance.Is_Valid;

   function Get_Max_Sector_Count (ID : Ports_Config.Port_Range)
      return Interfaces.Unsigned_32;

   procedure RW_Sectors
      (ID      :     Ports_Config.Port_Range;
       RW      :     Ahci.RW_Type;
       Start   :     Interfaces.Unsigned_64; --  Start Sector
       Count   :     Interfaces.Unsigned_32; --  Number of Sectors
       Address :     Interfaces.Unsigned_64; --  DMA Buffer address
       Ret_Val : out Storage_Interface.Status_Type)
   with
      Pre => Musinfo.Instance.Is_Valid;

   procedure Sync
      (ID      :     Ports_Config.Port_Range;
       Ret_Val : out Storage_Interface.Status_Type)
   with
      Pre => Musinfo.Instance.Is_Valid;

   procedure Identify_Device
      (Port_ID : Ports_Config.Port_Range)
   with
      Pre => Musinfo.Instance.Is_Valid;

   procedure Get_SMART
      (ID      :     Ports_Config.Port_Range;
       Address :     Interfaces.Unsigned_64; --  DMA Buffer address
       Status  : out Ahci.Device.SMART_Status_Type;
       Ret_Val : out Storage_Interface.Status_Type)
   with
      Pre => Musinfo.Instance.Is_Valid;

end Ata;
