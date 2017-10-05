--
--  Copyright (C) 2014-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

   type Unsigned_2 is mod 2 ** 2
     with
       Size => 2;

   type Unsigned_6 is mod 2 ** 6
     with
       Size => 6;

   type Unsigned_7 is mod 2 ** 7
     with
       Size => 7;

   type Unsigned_40 is mod 2 ** 40
     with
       Size => 40;

   type Unsigned_48 is mod 2 ** 48
     with
       Size => 48;

   type Name_Size_Type is range 0 .. 63
     with
       Size => 6;

   subtype Name_Index_Type is Positive range 1 .. 63;

   type Name_Data_Type is new String (Name_Index_Type'Range)
     with
       Size => Name_Index_Type'Last * 8;

   Name_Type_Size : constant := 1 + Name_Index_Type'Last;

   --  A name is a string with an explicit length field and maximum size of 63
   --  characters.
   type Name_Type is record
      Length  : Name_Size_Type;
      Padding : Unsigned_2;
      Data    : Name_Data_Type;
   end record
     with
       Size => Name_Type_Size * 8;

   for Name_Type use record
      Length  at 0 range 0 .. 5;
      Padding at 0 range 6 .. 7;
      Data    at 1 range 0 .. 503;
   end record;

   Null_Name : constant Name_Type
     := (Length  => 0,
         Padding => 0,
         Data    => (others => ASCII.NUL));

   --  Memory flags specify if memory regions are writable and/or executable,
   --  i.e. if instruction fetches from the memory region are allowed.
   type Memory_Flags_Type is record
      Writable   : Boolean;
      Executable : Boolean;
      Padding    : Unsigned_6;
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
         Padding    => 0);

   Memregion_Type_Size : constant := 4 + 8 + 8 + 1 + 32 + 2 + 1;

   type Content_Type is (Content_Uninitialized, Content_Fill, Content_File)
     with
       Convention => C;

   --  256-bit Hash.
   type Hash_Type is array (1 .. 32) of Interfaces.Unsigned_8
     with
       Convention => C;

   No_Hash : constant Hash_Type := (others => 0);

   No_Pattern : constant := 256;

   type Pattern_Type is range 0 .. No_Pattern
     with
       Size => 16;

   --  A memory region is described by its content, memory address, size, and
   --  flags.
   type Memregion_Type is record
      Content : Content_Type;
      Address : Interfaces.Unsigned_64;
      Size    : Interfaces.Unsigned_64;
      Hash    : Hash_Type;
      Flags   : Memory_Flags_Type;
      Pattern : Pattern_Type;
      Padding : Interfaces.Unsigned_8;
   end record
     with
       Alignment         => 8,
       Size              => Memregion_Type_Size * 8,
       Dynamic_Predicate =>
         (case Memregion_Type.Content is
             when Content_Fill => Memregion_Type.Pattern /= No_Pattern,
             when Content_Uninitialized
               | Content_File => Memregion_Type.Pattern = No_Pattern);

   for Memregion_Type use record
      Content at  0 range 0 .. 31;
      Address at  4 range 0 .. 63;
      Size    at 12 range 0 .. 63;
      Hash    at 20 range 0 .. 32 * 8 - 1;
      Flags   at 52 range 0 .. 7;
      Pattern at 53 range 0 .. 15;
      Padding at 55 range 0 .. 7;
   end record;

   Null_Memregion : constant Memregion_Type
     := (Content => Content_Uninitialized,
         Address => 0,
         Size    => 0,
         Hash    => (others => 0),
         Flags   => Null_Memory_Flags,
         Pattern => No_Pattern,
         Padding => 0);

   --  Channel flags indicate if a channel has an associated vector and or
   --  event number.
   type Channel_Flags_Type is record
      Has_Event  : Boolean;
      Has_Vector : Boolean;
      Padding    : Unsigned_6;
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
         Padding    => 0);

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
      Padding : Unsigned_40;
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
         Padding => 0);

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
      Padding          : Unsigned_48;
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
         Padding          => 0);

   Resource_Array_Size : constant := Resource_Index_Type'Last
     * Resource_Type_Size;

   type Resource_Array is array (Resource_Index_Type) of Resource_Type
     with
       Pack,
       Alignment => 8;

   type Dev_Flags_Type is record
      MSI_Capable : Boolean;
      Padding     : Unsigned_7;
   end record
     with Size => 8;

   for Dev_Flags_Type use record
      MSI_Capable at 0 range 0 .. 0;
      Padding     at 0 range 1 .. 7;
   end record;

   Null_Dev_Flags : constant Dev_Flags_Type
     := (MSI_Capable => False,
         Padding     => 0);

   subtype SID_Type is Interfaces.Unsigned_16;

   Dev_Info_Type_Size : constant := 8;

   --  Device info records enable subjects to query data about assigned PCI
   --  devices.
   type Dev_Info_Type is record
      SID        : SID_Type;
      IRTE_Start : Interfaces.Unsigned_16;
      IRQ_Start  : Interfaces.Unsigned_8;
      IR_Count   : Interfaces.Unsigned_8;
      Flags      : Dev_Flags_Type;
      Padding    : Interfaces.Unsigned_8;
   end record
     with
       Alignment => 8,
       Size      => Dev_Info_Type_Size * 8;

   for Dev_Info_Type use record
      SID        at 0 range 0 .. 15;
      IRTE_Start at 2 range 0 .. 15;
      IRQ_Start  at 4 range 0 .. 7;
      IR_Count   at 5 range 0 .. 7;
      Flags      at 6 range 0 .. 7;
      Padding    at 7 range 0 .. 7;
   end record;

   Null_Dev_Info : constant Dev_Info_Type
     := (SID        => 0,
         IRTE_Start => 0,
         IRQ_Start  => 0,
         IR_Count   => 0,
         Flags      => Null_Dev_Flags,
         Padding    => 0);

   Dev_Info_Array_Size : constant := Resource_Index_Type'Last
     * Dev_Info_Type_Size;

   type Dev_Info_Array is array (Resource_Index_Type) of Dev_Info_Type
     with
       Pack,
       Alignment => 8,
       Size      => Dev_Info_Array_Size * 8;

   Subject_Info_Type_Size : constant := 3 * 8 + Name_Type_Size
     + Resource_Array_Size + Memregion_Array_Size + Channel_Info_Array_Size
     + Dev_Info_Array_Size;

   --  TSC tick rate in Khz (1 Mhz .. 100 Ghz).
   subtype TSC_Tick_Rate_Khz_Type is Interfaces.Unsigned_64 range
     1000 .. 100000000;

   --  Subject info records enable subjects to determine what resources are
   --  provided to them at runtime.
   type Subject_Info_Type is record
      Magic              : Interfaces.Unsigned_64;
      Name               : Name_Type;
      Resource_Count     : Resource_Count_Type;
      Memregion_Count    : Resource_Count_Type;
      Channel_Info_Count : Resource_Count_Type;
      Dev_Info_Count     : Resource_Count_Type;
      Padding            : Interfaces.Integer_32;
      TSC_Khz            : TSC_Tick_Rate_Khz_Type;
      Resources          : Resource_Array;
      Memregions         : Memregion_Array;
      Channels_Info      : Channel_Info_Array;
      Dev_Info           : Dev_Info_Array;
   end record
     with
       Size      => Subject_Info_Type_Size * 8,
       Alignment => 8;

   Memregions_Offset    : constant := 3 * 8 + Name_Type_Size
      + Resource_Array_Size;
   Channels_Info_Offset : constant := Memregions_Offset + Memregion_Array_Size;
   Dev_Info_Offset      : constant := Channels_Info_Offset
      + Channel_Info_Array_Size;
   Name_Offset          : constant := Dev_Info_Offset + Dev_Info_Array_Size;

   for Subject_Info_Type use record
      Magic              at  0 range 0 .. 63;
      Name               at  8 range 0 .. (Name_Type_Size * 8) - 1;
      Resource_Count     at 72 range 0 .. 7;
      Memregion_Count    at 73 range 0 .. 7;
      Channel_Info_Count at 74 range 0 .. 7;
      Dev_Info_Count     at 75 range 0 .. 7;
      Padding            at 76 range 0 .. 31;
      TSC_Khz            at 80 range 0 .. 63;
      Resources          at 88 range 0 .. (Resource_Array_Size * 8) - 1;
      Memregions         at Memregions_Offset range
        0 .. (Memregion_Array_Size * 8) - 1;
      Channels_Info      at Channels_Info_Offset range
        0 .. (Channel_Info_Array_Size * 8) - 1;
      Dev_Info           at Dev_Info_Offset range
        0 .. (Dev_Info_Array_Size * 8) - 1;
   end record;

end Musinfo;
