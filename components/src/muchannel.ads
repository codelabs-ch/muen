--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

--  Muen shared memory channels.
--
--  Muen shared memory channels are an implementation of the SHMStream
--  Version 2 IPC protocol (shmstream) as specified by 'SHMStream Version 2 IPC
--  Interface', Robert Dorn, 2013, unpublished.
generic

   --  Elements transported via channel instance.
   type Element_Type is private;

   --  Capacity of channel in number of elements.
   Elements : Positive;

package Muchannel is

   --  Communication channel used by reader and writer.
   type Channel_Type is limited private;

   --  Type of channel header fields.
   type Header_Field_Type is mod 2**64;

   --  Size of channel header in bytes.
   Header_Size : constant Positive;

   --  Returns True if the channel is currently active.
   procedure Is_Active
     (Channel :     Channel_Type;
      Result  : out Boolean);

private

   --  Returns True if the channel with given epoch is currently active. This
   --  utility function is provided because the public Is_Active procedure
   --  cannot be called from other subprograms due to the volatile Channel
   --  parameter (see also ticket [NA10-010]).
   function Is_Active_Channel (Epoch : Header_Field_Type) return Boolean;

   --  "SHMStream20=", base64-encoded.
   SHMStream_Marker : constant := 16#4873_12b6_b79a_9b6d#;

   for Header_Field_Type'Size use 64;

   --  Channel header as specified by SHMStream v2 protocol.
   type Header_Type is record
      Transport : Header_Field_Type with Atomic;
      Epoch     : Header_Field_Type with Atomic;
      Protocol  : Header_Field_Type;
      Size      : Header_Field_Type;
      Elements  : Header_Field_Type;
      Reserved  : Header_Field_Type;
      WSC       : Header_Field_Type with Atomic;
      WC        : Header_Field_Type with Atomic;
   end record
     with Alignment => 64,
          Size      => 64 * 8;

   for Header_Type use record
      Transport at  0 range 0 .. 63;
      Epoch     at  8 range 0 .. 63;
      Protocol  at 16 range 0 .. 63;
      Size      at 24 range 0 .. 63;
      Elements  at 32 range 0 .. 63;
      Reserved  at 40 range 0 .. 63;
      WSC       at 48 range 0 .. 63;
      WC        at 56 range 0 .. 63;
   end record;

   Header_Size : constant Positive := Header_Type'Size / 8;

   --  Channel data stored as array of elements.
   type Data_Range is new Natural range 0 .. Elements - 1;
   type Data_Type  is array (Data_Range) of Element_Type
     with Pack;

   Data_Size : constant Positive := Data_Type'Size / 8;

   type Channel_Type is record
      Header : Header_Type;
      Data   : Data_Type;
   end record
     with Volatile, Pack;

   --  Null epoch used for inactive/disabled channels.
   Null_Epoch : constant Header_Field_Type := 0;

end Muchannel;
