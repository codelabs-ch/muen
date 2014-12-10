--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--    * Redistributions of source code must retain the above copyright notice,
--      this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--

--  Muen shared memory channels.
--
--  Muen shared memory channels are an implementation of the SHMStream
--  Version 2 IPC protocol (shmstream) as specified by 'SHMStream Version 2 IPC
--  Interface', Robert Dorn, 2013, unpublished.
generic

   --  Elements transported via channel instance.
   type Element_Type is private;

   --  Element size in bytes.
   Element_Size : Positive;

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

   Header_Size : constant Positive := 64;

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
          Size      => 8 * Header_Size;

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

   --  Channel data stored as array of elements.
   type Data_Range is new Natural range 0 .. Elements - 1;
   type Data_Type  is array (Data_Range) of Element_Type
     with Pack;

   Data_Size : constant Positive := Data_Type'Length * Element_Size;

   type Channel_Type is record
      Header : Header_Type;
      Data   : Data_Type;
   end record
     with Volatile, Pack;

   --  Null epoch used for inactive/disabled channels.
   Null_Epoch : constant Header_Field_Type := 0;

end Muchannel;
