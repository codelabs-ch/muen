--
--  Copyright (C) 2014-2018  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2018  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

--  Muen subject information (sinfo) data structures.
--
--  An sinfo record is mapped into the address space of each running subject
--  and provides means to retrieve information about the execution environment.
--
--  Subject resources are exported as variant records, which are all explicitly
--  padded in order to guarantee an exact layout and proper initialization of
--  unused space in smaller variants.
--  The padding size of each variant is determined by the size of the largest
--  variant (see the Largest_Variant_Size constant below).
package Musinfo
is

   --  Unique identifier used to designate a subject info memory region.
   --  "muinfo" with highest 2 bytes for counter.
   Muen_Subject_Info_Magic : constant := 16#0200_6f66_6e69_756d#;

   type Unsigned_2 is mod 2 ** 2
     with
       Size => 2;

   type Unsigned_5 is mod 2 ** 5
     with
       Size => 5;

   type Unsigned_7 is mod 2 ** 7
     with
       Size => 7;

   type Name_Size_Type is range 0 .. 63
     with
       Size => 6;

   subtype Name_Index_Type is Positive range 1 .. 63;

   type Name_Data_Type is new String (Name_Index_Type'Range)
     with
       Size => Name_Index_Type'Last * 8;

   Name_Type_Size : constant := 1 + Name_Index_Type'Last + 1;

   --  A name is a string with an explicit length field and maximum size of 63
   --  characters. The data field is null-terminated via Null_Term and can be
   --  safely used in C/C++ string operations as is.
   type Name_Type is record
      Length    : Name_Size_Type;
      Padding   : Unsigned_2;
      Data      : Name_Data_Type;
      Null_Term : Character;
   end record
     with
       Size => Name_Type_Size * 8;

   for Name_Type use record
      Length    at  0 range 0 .. 5;
      Padding   at  0 range 6 .. 7;
      Data      at  1 range 0 .. 503;
      Null_Term at 64 range 0 .. 7;
   end record;

   Null_Name : constant Name_Type
     := (Length    => 0,
         Padding   => 0,
         Data      => (others => ASCII.NUL),
         Null_Term => ASCII.NUL);

   type Memory_Flags_Type is record
      Writable   : Boolean;
      Executable : Boolean;
      Channel    : Boolean;
      Padding    : Unsigned_5;
   end record
     with Size => 8;

   for Memory_Flags_Type use record
      Writable   at 0 range 0 .. 0;
      Executable at 0 range 1 .. 1;
      Channel    at 0 range 2 .. 2;
      Padding    at 0 range 3 .. 7;
   end record;

   Null_Memory_Flags : constant Memory_Flags_Type
     := (Writable   => False,
         Executable => False,
         Channel    => False,
         Padding    => 0);

   Memregion_Type_Size : constant := 4 + 8 + 8 + 32 + 1 + 2 + 1;

   --  Size of the largest variant. All smaller variants must be manually
   --  padded in order to have unused space properly initialized.
   Largest_Variant_Size : constant := Memregion_Type_Size;

   type Content_Type is (Content_Uninitialized, Content_Fill, Content_File)
     with
       Size => 32;

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
   --  flags. Regions of type fill and file may optionally provide a hash of
   --  the content.
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
       Size              => Memregion_Type_Size * 8,
       Alignment         => 8,
       Dynamic_Predicate =>
         (Interfaces."=" (Interfaces."mod" (Memregion_Type.Size, 64), 0)
          and
            (case Memregion_Type.Content is
               when Content_Fill => Memregion_Type.Pattern /= No_Pattern,
               when Content_Uninitialized
                 | Content_File => Memregion_Type.Pattern = No_Pattern));

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

   type Byte_Array is array (Positive range <>) of Interfaces.Unsigned_8;

   subtype Byte_Padding is Byte_Array (1 .. Largest_Variant_Size - 1);

   --  Used to store a 8-bit number in a resource variant.
   type Byte_Type is record
      Value   : Interfaces.Unsigned_8;
      Padding : Byte_Padding;
   end record
     with
       Size      => Largest_Variant_Size * 8,
       Alignment => 8;

   for Byte_Type use record
      Value   at 0 range 0 .. 7;
      Padding at 1 range 0 .. (Byte_Padding'Last * 8) - 1;
   end record;

   type Dev_Flags_Type is record
      MSI_Capable : Boolean;
      Padding     : Unsigned_7;
   end record
     with
       Size => 8;

   for Dev_Flags_Type use record
      MSI_Capable at 0 range 0 .. 0;
      Padding     at 0 range 1 .. 7;
   end record;

   Null_Dev_Flags : constant Dev_Flags_Type
     := (MSI_Capable => False,
         Padding     => 0);

   subtype SID_Type is Interfaces.Unsigned_16;

   Null_SID : constant := SID_Type'Last;

   Device_Type_Size : constant := 7;

   subtype Dev_Padding is Byte_Array
     (1 .. Largest_Variant_Size - Device_Type_Size);

   --  Device records enable subjects to query data about assigned PCI devices.
   type Device_Type is record
      SID        : SID_Type;
      IRTE_Start : Interfaces.Unsigned_16;
      IRQ_Start  : Interfaces.Unsigned_8;
      IR_Count   : Interfaces.Unsigned_8;
      Flags      : Dev_Flags_Type;
      Padding    : Dev_Padding;
   end record
     with
       Size      => Largest_Variant_Size * 8,
       Alignment => 8;

   for Device_Type use record
      SID        at 0 range 0 .. 15;
      IRTE_Start at 2 range 0 .. 15;
      IRQ_Start  at 4 range 0 .. 7;
      IR_Count   at 5 range 0 .. 7;
      Flags      at 6 range 0 .. 7;
      Padding    at 7 range 0 .. (Dev_Padding'Last * 8) - 1;
   end record;

   Null_Device : constant Device_Type
     := (SID        => 0,
         IRTE_Start => 0,
         IRQ_Start  => 0,
         IR_Count   => 0,
         Flags      => Null_Dev_Flags,
         Padding    => (others => 0));

   type Resource_Kind is
     (Res_None,
      Res_Memory,
      Res_Event,
      Res_Vector,
      Res_Device)
     with
       Size => 32;

   --  Must be the size of the largest variant + name + discrimant (4 bytes).
   Resource_Type_Size : constant := 4 + 3 + Name_Type_Size
     + Largest_Variant_Size;

   --  A resource associates a name with given resource data.
   type Resource_Type (Kind : Resource_Kind := Res_None) is record
      Name    : Name_Type;
      Padding : Byte_Array (1 .. 3);

      case Kind is
         when Res_None   => null;
         when Res_Memory =>
            Mem_Data : Memregion_Type;
         when Res_Event  =>
            Evt_Data : Byte_Type;
         when Res_Vector =>
            Vec_Data : Byte_Type;
         when Res_Device =>
            Dev_Data : Device_Type;
      end case;
   end record
     with
       Pack,
       Size      => Resource_Type_Size * 8,
       Alignment => 8;

   Null_Resource : constant Resource_Type
     := (Kind    => Res_None,
         Name    => Null_Name,
         Padding => (others => 0));

   type Resource_Index_Type is range 1 .. 255;

   Resource_Array_Size : constant := Resource_Index_Type'Last
     * Resource_Type_Size;

   pragma Warnings
     (GNATprove, Off, "pragma ""Suppress_Initialization"" ignored*",
      Reason => "Type init code violates No_Implicit_Loops (P916-031)");
   type Resource_Array is array (Resource_Index_Type) of Resource_Type
     with
       Pack,
       Suppress_Initialization, --  Because of No_Implicit_Loops
       Size      => Resource_Array_Size * 8,
       Alignment => 8;
   pragma Warnings
     (GNATprove, On, "pragma ""Suppress_Initialization"" ignored*");

   --  TSC tick rate in Khz (1 Mhz .. 100 Ghz).
   subtype TSC_Tick_Rate_Khz_Type is Interfaces.Unsigned_64 range
     1000 .. 100000000;

   Subject_Info_Type_Size : constant := 8 + 4 + Name_Type_Size
     + Resource_Array_Size + 3;

   --  Subject info records enable subjects to determine what resources are
   --  provided to them at runtime.
   type Subject_Info_Type is record
      Magic          : Interfaces.Unsigned_64;
      TSC_Khz        : TSC_Tick_Rate_Khz_Type;
      Name           : Name_Type;
      Resource_Count : Interfaces.Unsigned_16;
      Padding        : Interfaces.Unsigned_8;
      Resources      : Resource_Array;
   end record
     with
       Size      => Subject_Info_Type_Size * 8,
       Alignment => 8;

   Res_Count_Offset : constant := 8 + 4 + Name_Type_Size;
   Pad_Offset       : constant := Res_Count_Offset + 2;
   Res_Offset       : constant := Pad_Offset + 1;

   for Subject_Info_Type use record
      Magic          at  0 range 0 .. 63;
      TSC_Khz        at  8 range 0 .. 31;
      Name           at 12 range 0 .. (Name_Type_Size * 8) - 1;
      Resource_Count at Res_Count_Offset range 0 .. 15;
      Padding        at Pad_Offset range 0 .. 7;
      Resources      at Res_Offset range 0 .. (Resource_Array_Size * 8) - 1;
   end record;

end Musinfo;
