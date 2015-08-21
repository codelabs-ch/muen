--
--  Copyright (C) 2014-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

package Musinfo
is

   --  Unique identifier used to designate a subject info memory region.
   --  "muinfo" with highest 2 bytes for counter.
   Muen_Subject_Info_Magic : constant := 16#0100_6f66_6e69_756d#;

   type Bit_Type is range 0 .. 1
     with
       Size => 1;

   type Bit_Array is array (Positive range <>) of Bit_Type
     with
       Pack;

   type Name_Size_Type is range 0 .. 63
     with
       Size => 6;

   subtype Name_Index_Type is Positive range 1 .. 63;

   type Name_Data_Type is new String
     (Name_Index_Type'First .. Name_Index_Type'Last)
     with
       Size => Name_Index_Type'Last * 8;

   --  A name is a string with an explicit length field and maximum size of 63
   --  characters.
   type Name_Type is record
      Length  : Name_Size_Type;
      Padding : Bit_Array (1 .. 2);
      Data    : Name_Data_Type;
   end record
     with
       Size => (1 + Name_Index_Type'Last) * 8;

   for Name_Type use record
      Length  at 0 range 0 .. 5;
      Padding at 0 range 6 .. 7;
      Data    at 1 range 0 .. 503;
   end record;

   Null_Name : constant Name_Type
     := (Length  => 0,
         Padding => (others => 0),
         Data    => (others => ASCII.NUL));

   --  Memory flags specify if memory regions are writable and/or executable,
   --  i.e. if instruction fetches from the memory region are allowed.
   type Memory_Flags_Type is record
      Writable   : Boolean;
      Executable : Boolean;
      Padding    : Bit_Array (1 .. 6);
   end record
     with Size => 8;

   for Memory_Flags_Type use record
      Writable   at 0 range 0 .. 0;
      Executable at 0 range 1 .. 1;
      Padding    at 0 range 2 .. 7;
   end record;

   Null_Memory_Flags : constant Memory_Flags_Type
     := (Writable   => False,
         Executable => False,
         Padding    => (others => 0));

   Memregion_Type_Size : constant := 8 + 8 + 1 + 7;

   --  A memory region is described by its memory address, size and flags.
   type Memregion_Type is record
      Address : Interfaces.Unsigned_64;
      Size    : Interfaces.Unsigned_64;
      Flags   : Memory_Flags_Type;
      Padding : Bit_Array (1 .. 55);
   end record
     with
       Alignment => 8,
       Size      => Memregion_Type_Size * 8;

   for Memregion_Type use record
      Address at  0 range 0 .. 63;
      Size    at  8 range 0 .. 63;
      Flags   at 16 range 0 .. 7;
      Padding at 17 range 0 .. 55;
   end record;

   Null_Memregion : constant Memregion_Type
     := (Address => 0,
         Size    => 0,
         Flags   => Null_Memory_Flags,
         Padding => (others => 0));

   --  Channel flags indicate if a channel has an associated vector and or
   --  event number.
   type Channel_Flags_Type is record
      Has_Event  : Boolean;
      Has_Vector : Boolean;
      Padding    : Bit_Array (1 .. 6);
   end record
     with Size => 8;

   for Channel_Flags_Type use record
      Has_Event  at 0 range 0 .. 0;
      Has_Vector at 0 range 1 .. 1;
      Padding    at 0 range 2 .. 7;
   end record;

   Null_Channel_Flags : constant Channel_Flags_Type
     := (Has_Event  => False,
         Has_Vector => False,
         Padding    => (others => 0));

   type Event_Number_Range is range 0 .. 255
     with
       Size => 8;

   type Vector_Range is range 0 .. 255
     with
       Size => 8;

   Channel_Info_Type_Size : constant := 1 + 1 + 1 + 5;

   --  Channel information consist of an optional vector and event number.
   --  The assignment of an optional vector and/or an event to a channel is
   --  indicated by the Has_Event/Vector flags.
   type Channel_Info_Type is record
      Flags   : Channel_Flags_Type;
      Event   : Event_Number_Range;
      Vector  : Vector_Range;
      Padding : Bit_Array (1 .. 40);
   end record
     with
       Alignment => 8,
       Size      => Channel_Info_Type_Size * 8;

   for Channel_Info_Type use record
      Flags   at 0 range 0 .. 7;
      Event   at 1 range 0 .. 7;
      Vector  at 2 range 0 .. 7;
      Padding at 3 range 0 .. 39;
   end record;

   Null_Channel_Info : constant Channel_Info_Type
     := (Flags   => Null_Channel_Flags,
         Event   => 0,
         Vector  => 0,
         Padding => (others => 0));

   type Resource_Count_Type is range 0 .. 255
     with
       Size => 8;

   No_Resource : constant Resource_Count_Type := Resource_Count_Type'First;

   subtype Resource_Index_Type is Resource_Count_Type range
     1 .. Resource_Count_Type'Last;

   Memregion_Array_Size : constant := Resource_Index_Type'Last
     * Memregion_Type_Size;

   type Memregion_Array is array (Resource_Index_Type) of Memregion_Type
     with
       Pack,
       Alignment => 8;

   Channel_Info_Array_Size : constant := Resource_Index_Type'Last
     * Channel_Info_Type_Size;

   type Channel_Info_Array is array (Resource_Index_Type) of Channel_Info_Type
     with
       Pack,
       Alignment => 8;

   Resource_Type_Size : constant := 64 + 1 + 1 + 6;

   --  A resource associates a name with a memory region and optionally
   --  additional channel information using array indexes as references.
   type Resource_Type is record
      Name             : Name_Type;
      Memregion_Idx    : Resource_Count_Type;
      Channel_Info_Idx : Resource_Count_Type;
      Padding          : Bit_Array (1 .. 48);
   end record
     with
       Alignment => 8,
       Size      => Resource_Type_Size * 8;

   for Resource_Type use record
      Name             at  0 range 0 .. 511;
      Memregion_Idx    at 64 range 0 .. 7;
      Channel_Info_Idx at 65 range 0 .. 7;
      Padding          at 66 range 0 .. 47;
   end record;

   Null_Resource : constant Resource_Type
     := (Name             => Null_Name,
         Memregion_Idx    => No_Resource,
         Channel_Info_Idx => No_Resource,
         Padding          => (others => 0));

   Resource_Array_Size : constant := Resource_Index_Type'Last
     * Resource_Type_Size;

   type Resource_Array is array (Resource_Index_Type) of Resource_Type
     with
       Pack,
       Alignment => 8;

   Subject_Info_Type_Size : constant := 5 * 8 + Resource_Array_Size
     + Memregion_Array_Size + Channel_Info_Array_Size;

   --  Subject info records enable subjects to determine what resources are
   --  provided to them at runtime.
   type Subject_Info_Type is record
      Magic              : Interfaces.Unsigned_64;
      Resource_Count     : Resource_Count_Type;
      Memregion_Count    : Resource_Count_Type;
      Channel_Info_Count : Resource_Count_Type;
      Padding            : Bit_Array (1 .. 40);
      TSC_Khz            : Interfaces.Unsigned_64;
      TSC_Schedule_Start : Interfaces.Unsigned_64;
      TSC_Schedule_End   : Interfaces.Unsigned_64;
      Resources          : Resource_Array;
      Memregions         : Memregion_Array;
      Channels_Info      : Channel_Info_Array;
   end record
     with
       Size      => Subject_Info_Type_Size * 8,
       Alignment => 8;

   Memregions_Offset    : constant := 40 + Resource_Array_Size;
   Channels_Info_Offset : constant := Memregions_Offset + Memregion_Array_Size;

   for Subject_Info_Type use record
      Magic              at 0  range 0 .. 63;
      Resource_Count     at 8  range 0 .. 7;
      Memregion_Count    at 9  range 0 .. 7;
      Channel_Info_Count at 10 range 0 .. 7;
      Padding            at 11 range 0 .. 39;
      TSC_Khz            at 16 range 0 .. 63;
      TSC_Schedule_Start at 24 range 0 .. 63;
      TSC_Schedule_End   at 32 range 0 .. 63;
      Resources          at 40 range 0 .. (Resource_Array_Size * 8) - 1;
      Memregions         at Memregions_Offset range
        0 .. (Memregion_Array_Size * 8) - 1;
      Channels_Info      at Channels_Info_Offset range
        0 .. (Channel_Info_Array_Size * 8) - 1;
   end record;

   Null_Subject_Info : constant Subject_Info_Type
     := (Magic              => Muen_Subject_Info_Magic,
         Resource_Count     => No_Resource,
         Memregion_Count    => No_Resource,
         Channel_Info_Count => No_Resource,
         Padding            => (others => 0),
         TSC_Khz            => 0,
         TSC_Schedule_Start => 0,
         TSC_Schedule_End   => 0,
         Resources          => (others => Null_Resource),
         Memregions         => (others => Null_Memregion),
         Channels_Info      => (others => Null_Channel_Info));

end Musinfo;
