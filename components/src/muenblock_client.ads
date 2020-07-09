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
with System;

with Muenblock;

generic
   --  Request and response channel addresses
   Req_Channel_Address  : System.Address;
   Resp_Channel_Address : System.Address;

   --  Event Number of the Request Channel
   Event_Number         : Interfaces.Unsigned_8;

   --  Maximum Number of Devices on this channel
   Devices_Cnt_Max      : Interfaces.Unsigned_16;
package Muenblock_Client
is
   use type Interfaces.Unsigned_16;

   type Device_Range_Type is new Interfaces.Unsigned_16
      range 0 .. Devices_Cnt_Max - 1;

   -------------------------------------------------------------------

   --  Initialize the muenblock client.
   --  Give a per block - timeout for read and write operations to avoid
   --  infinite blocking in read / write calls.
   procedure Init (Timeout_MS : Integer := Integer'Last);

   --  Discard a range of sectors
   procedure Discard
      (Device_Id     :     Device_Range_Type;
       Start_Sector  :     Interfaces.Unsigned_64;
       Sector_Cnt    :     Interfaces.Unsigned_64;
       Result        : out Interfaces.Unsigned_64);

   --  Get Number of Sectors and Sector Size of a given Device
   procedure Get_Device_Info
      (Device_Id   :     Device_Range_Type;
       Sector_Cnt  : out Interfaces.Unsigned_64;
       Sector_Size : out Interfaces.Unsigned_64;
       Max_Sectors : out Interfaces.Unsigned_64;
       Valid       : out Boolean);

   --  Read 'Sector_Cnt' Sectors from 'Start_Sector' to
   --  'Buffer_Offset'
   --
   --  Caller must ensure that the Buffer Ranges do no overlap and that
   --  Buffer_Offset + Sector_Cnt is in the Range of the Buffer (no wrap around
   --  handled by muenblock_client)
   --
   --  Read and Write are not threadsave and should not be called concurrently.
   procedure Read
      (Device_Id     :     Device_Range_Type;
       Start_Sector  :     Interfaces.Unsigned_64;
       Buffer_Offset :     Interfaces.Unsigned_64;
       Sector_Cnt    :     Interfaces.Unsigned_64;
       Result        : out Interfaces.Unsigned_64);

   --  Complete any outstanding operations and flush the write cache
   procedure Sync
      (Device_Id     :     Device_Range_Type;
       Result        : out Interfaces.Unsigned_64);

   --  Write 'Sector_Cnt' Sectors from 'Buffer_Offset' to 'Start_Sector'
   --
   --  Caller must ensure that the Buffer Ranges do no overlap and that
   --  Buffer_Offset + Sector_Cnt is in the Range of the Buffer (no wrap around
   --  handled by muenblock_client)
   procedure Write
      (Device_Id     :     Device_Range_Type;
       Start_Sector  :     Interfaces.Unsigned_64;
       Buffer_Offset :     Interfaces.Unsigned_64;
       Sector_Cnt    :     Interfaces.Unsigned_64;
       Result        : out Interfaces.Unsigned_64);

end Muenblock_Client;
