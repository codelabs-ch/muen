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

package Muenblock
is

   type Byte_Array is array (Natural range <>) of Interfaces.Unsigned_8
   with
      Pack;

   type Request_Kind_Type is
      (None,               --  Do nothing
       Read,               --  Read count blocks
       Write,              --  Write count blocks
       Discard,            --  Discard/zero count blocks
       Sync,
       Barrier,            --  Enforce media write/barrier etc.
       Media_Blocks,       --  Return number of Blocks of device
       Block_Length,       --  Return block size of device
       Max_Requests,       --  Return maximum number of requests in flight
       Max_Blocks_Count,   --  Return max number of blocks per
                           --   read/write/discard request
       Max_Devices,        --  Return number of devices
       Reset)              --  Assure that no requests are pending upon return
      with Size => 2 * 8;

   for Request_Kind_Type use
      (None               => 16#0000#,
       Read               => 16#0001#,
       Write              => 16#0002#,
       Discard            => 16#0003#,
       Sync               => 16#0004#,
       Barrier            => 16#0005#,
       Media_Blocks       => 16#0010#,
       Block_Length       => 16#0011#,
       Max_Requests       => 16#0019#,
       Max_Blocks_Count   => 16#001a#,
       Max_Devices        => 16#001b#,
       Reset              => 16#001f#);

   Block_Request_Type_Size : constant := 2 + 2 + 4 + 8 + 8 + 8;
   type Block_Request_Type is record
      Request_Kind   : Request_Kind_Type;
      --  Device (0 .. Max_Devices - 1)
      Device_Id      : Interfaces.Unsigned_16;
      --  Chosen by client; client SHOULD avoid duplicate tags in flight
      Request_Tag    : Interfaces.Unsigned_32;
      --  Number of Bytes to be handled (1 .. Max_Length)
      --    Zero for None, must be a multiple of Block_Length
      Request_Length : Interfaces.Unsigned_64;
      --  Operation affects Device Byte Address
      --   (Device_Offset..Device_Offset + Request_Length-1)
      --   Zero for None, must be aligned to Block_Length
      Device_Offset  : Interfaces.Unsigned_64;
      --  Byte offset into DMA Buffer
      --    DMA Buffer used for operation:
      --     (Buffer_Offset .. Buffer_Offset + Request_Length -1)
      --      Zero for None, must be aligned to Block_Length
      Buffer_Offset  : Interfaces.Unsigned_64;
   end record
    with Size => Block_Request_Type_Size * 8;

   for Block_Request_Type use record
      Request_Kind   at  0 range 0 .. 15;
      Device_Id      at  2 range 0 .. 15;
      Request_Tag    at  4 range 0 .. 31;
      Request_Length at  8 range 0 .. 63;
      Device_Offset  at 16 range 0 .. 63;
      Buffer_Offset  at 24 range 0 .. 63;
   end record;

   Block_Response_Size : constant := (2 + 2 + 4 + 8);
   type Block_Response_Type is record
      --  Copied from corresponding request
      Request_Kind : Request_Kind_Type;
      --  Copied from corresponding request
      Device_Id    : Interfaces.Unsigned_16;
      --  Copied from corresponding request
      Request_Tag : Interfaces.Unsigned_32;
      Status_Code : Interfaces.Unsigned_64;
   end record
    with Size => Block_Response_Size * 8;

   for Block_Response_Type use record
     Request_Kind at 0 range 0 .. 15;
     Device_Id    at 2 range 0 .. 15;
     Request_Tag  at 4 range 0 .. 31;
     Status_Code  at 8 range 0 .. 63;
   end record;

   Null_Request  : constant Block_Request_Type :=
      (Request_Kind => None,
       Device_Id    => 0,
       Request_Tag  => 0,
       others       => 0);

   Null_Response : constant Block_Response_Type :=
      (Request_Kind => None,
       Device_Id    => 0,
       Request_Tag  => 0,
       others       => 0);

   --  Muenblock Channel Protocoll ID
   Protocol : constant := 16#5155_6844_5351_6f3d#;

   --  Number of request / responsens in request / response channel
   --  need to honor the muchannel header size (64 byte)
   --  -> 2 elements on request /  4 elements on response channels
   Request_Channel_Elements  : constant := 1022;
   Response_Channel_Elements : constant := 1020;

end Muenblock;
