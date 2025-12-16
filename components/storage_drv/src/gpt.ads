with Partitions;
with Ports_Config;
with Interfaces;
with Storage_Interface; use Storage_Interface;

package Gpt
is
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_128;

   type Partition_Attributes is record
      Platform_Required    : Boolean;
      EFI_Should_Ignore    : Boolean;
      Legacy_Bios_Bootable : Boolean;
      -- rest is reserved / partition specifc (define if needed)
      Reserved             : Bit_Array (3 .. 63);
   end record with
      Size => 64, Object_Size => 64;
   for Partition_Attributes use record
      Platform_Required    at 0 range 0 ..  0;
      EFI_Should_Ignore    at 0 range 1 ..  1;
      Legacy_Bios_Bootable at 0 range 2 ..  2;
      Reserved             at 0 range 3 .. 63;
   end record;

   type Partition_Entry_Type is record
      Partition_Type_GUID   : Unsigned_128;       -- Mixed Endian
      Unique_Partition_GUID : Unsigned_128;       -- Mixed Endian
      Starting_LBA          : Unsigned_64;        -- Little Endian
      Ending_LBA            : Unsigned_64;        -- inclusive, usually odd
      Attributes            : Partition_Attributes;
      Partition_Name        : String (1 .. 72);   -- 36 Chars UTF-16LE (wide_str)--> Decode!
   end record with
      Size => 1024, Object_Size => 1024;
   for Partition_Entry_Type use record
      Partition_Type_GUID   at  0 range 0 .. 127;
      Unique_Partition_GUID at 16 range 0 .. 127;
      Starting_LBA          at 32 range 0 ..  63;
      Ending_LBA            at 40 range 0 ..  63;
      Attributes            at 48 range 0 ..  63;
      Partition_Name        at 56 range 0 .. 575;
   end record;

   type GPT_Header_Type is record
      Signature                   : String (1 .. 8); -- Little Endian
      Revision                    : Unsigned_32;
      Header_Size                 : Unsigned_32;
      Header_CRC32                : Unsigned_32;
      Reserved                    : Byte_Array (0 .. 3); -- reserved (zeroed)
      My_LBA                      : Unsigned_64;
      Alternate_LBA               : Unsigned_64;
      First_Useable_LBA           : Unsigned_64;    -- primary partition table last LBA + 1
      Last_Useable_LBA            : Unsigned_64;    -- secondary partition table first LBA - 1
      Disk_GUID                   : Unsigned_128;   -- Mixed Endian
      Partition_Entry_LBA         : Unsigned_64;    -- LBA of start of partition entry array
      Number_Of_Partition_Entries : Unsigned_32;    -- usually   2
      Size_Of_Partition_Entry     : Unsigned_32;    -- usually 128
      Partition_Entry_Array_CRC32 : Unsigned_32;
   end record with
      Size => 736, Object_Size => 736; -- 92 Byte
   for GPT_Header_Type use record
      Signature                   at  0 range 0 ..  63;
      Revision                    at  8 range 0 ..  31;
      Header_Size                 at 12 range 0 ..  31;
      Header_CRC32                at 16 range 0 ..  31;
      Reserved                    at 20 range 0 ..  31;
      My_LBA                      at 24 range 0 ..  63;
      Alternate_LBA               at 32 range 0 ..  63;
      First_Useable_LBA           at 40 range 0 ..  63;
      Last_Useable_LBA            at 48 range 0 ..  63;
      Disk_GUID                   at 56 range 0 .. 127;
      Partition_Entry_LBA         at 72 range 0 ..  63;
      Number_Of_Partition_Entries at 80 range 0 ..  31;
      Size_Of_Partition_Entry     at 84 range 0 ..  31;
      Partition_Entry_Array_CRC32 at 88 range 0 ..  31;
   end record;

   type Entry_Array_Type is array (Partitions.Partition_Array_Range) of Partition_Entry_Type
   with Pack, Size => 1024 * 128, Object_Size => 1024 * 128;

   -- https://upload.wikimedia.org/wikipedia/commons/0/07/GUID_Partition_Table_Scheme.svg
   type Primary_GPT is record
      Primary_GPT_Header : GPT_Header_Type;
      Reserved           : Byte_Array (0 .. 419);
      Entry_Array        : Entry_Array_Type;
   end record with
      Size => 135168, Object_Size => 135168; -- why 127360
   for Primary_GPT use record
      Primary_GPT_Header at   0 range 0 ..    735;
      Reserved           at  92 range 0 ..   3359;
      Entry_Array        at 512 range 0 .. 131071;
   end record;

   procedure Parse
     (ID         :     Ports_Config.Port_Range;
      Part_Table : out Partitions.Partition_Table_Type)
   with Pre => Storage_Interface.Is_Valid;

end Gpt;
