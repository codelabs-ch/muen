--
--  Copyright (C) 2020 secunet Security Networks AG
--  Copyright (C) 2025  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with System;

with Interfaces;

generic
   --  Request and response channel base addresses
   Req_Channel_Base_Address  : System.Address;
   Resp_Channel_Base_Address : System.Address;

   --  Base Event Number of the Request Channels
   Base_Event_Number         : Interfaces.Unsigned_8;

   type Client_Range_Type is range <>;

   --  Maximum Number of Devices per channel
   Devices_Cnt_Max      : Interfaces.Unsigned_16;
package Muenblock_Clients
is
   use type Interfaces.Unsigned_16;

   type Device_Range_Type is new Interfaces.Unsigned_16 range
     0 .. Devices_Cnt_Max - 1;

   -------------------------------------------------------------------

   --  Initialize the muenblock client specified by ID.
   --  Give a per block - timeout for read and write operations to avoid
   --  infinite blocking in read / write calls.
   procedure Init
     (Client     : Client_Range_Type := Client_Range_Type'First;
      Timeout_MS : Integer := Integer'Last);

   --  Discard a range of sectors for the given device of the specified client.
   procedure Discard
     (Client        :     Client_Range_Type := Client_Range_Type'First;
      Device_Id     :     Device_Range_Type;
      Start_Sector  :     Interfaces.Unsigned_64;
      Sector_Cnt    :     Interfaces.Unsigned_64;
      Result        : out Interfaces.Unsigned_64);

   --  Get Number of Sectors and Sector Size of the given Device and specified
   --  client.
   procedure Get_Device_Info
     (Client      :     Client_Range_Type := Client_Range_Type'First;
      Device_Id   :     Device_Range_Type;
      Sector_Cnt  : out Interfaces.Unsigned_64;
      Sector_Size : out Interfaces.Unsigned_64;
      Max_Sectors : out Interfaces.Unsigned_64;
      Valid       : out Boolean);

   --  Get the SMART Attributes the given Device and specified client.
   procedure Get_SMART
     (Client        :     Client_Range_Type := Client_Range_Type'First;
      Device_Id     :     Device_Range_Type;
      Buffer_Offset :     Interfaces.Unsigned_64;
      Result        : out Interfaces.Unsigned_64);

   --  Read 'Sector_Cnt' Sectors from 'Start_Sector' to
   --  'Buffer_Offset'
   --
   --  Caller must ensure that the Buffer Ranges do no overlap and that
   --  Buffer_Offset + Sector_Cnt is in the Range of the Buffer (no wrap around
   --  handled by muenblock_client)
   --
   --  Read and Write are not threadsafe and should not be called concurrently.
   procedure Read
     (Client        :     Client_Range_Type := Client_Range_Type'First;
      Device_Id     :     Device_Range_Type;
      Start_Sector  :     Interfaces.Unsigned_64;
      Buffer_Offset :     Interfaces.Unsigned_64;
      Sector_Cnt    :     Interfaces.Unsigned_64;
      Result        : out Interfaces.Unsigned_64);

   --  Complete any outstanding operations and flush the write cache of the
   --  given Device and specified client.
   procedure Sync
     (Client    :     Client_Range_Type := Client_Range_Type'First;
      Device_Id :     Device_Range_Type;
      Result    : out Interfaces.Unsigned_64);

   --  Write 'Sector_Cnt' Sectors from 'Buffer_Offset' to 'Start_Sector'
   --
   --  Caller must ensure that the Buffer Ranges do no overlap and that
   --  Buffer_Offset + Sector_Cnt is in the Range of the Buffer (no wrap around
   --  handled by muenblock_client)
   procedure Write
     (Client        :     Client_Range_Type := Client_Range_Type'First;
      Device_Id     :     Device_Range_Type;
      Start_Sector  :     Interfaces.Unsigned_64;
      Buffer_Offset :     Interfaces.Unsigned_64;
      Sector_Cnt    :     Interfaces.Unsigned_64;
      Result        : out Interfaces.Unsigned_64);

end Muenblock_Clients;
