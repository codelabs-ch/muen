with Interfaces;
with Ports_Config;

with Storage_Drv_Cspecs_Wrapper;

with Muenblock;
with Muenblock.Request_Channel;
with Muenblock.Response_Channel;
with Musinfo.Instance;

package Storage_Interface is
   package CSpecs   renames Storage_Drv_Cspecs_Wrapper.Channel_Arrays;
   package PConf    renames Ports_Config;
   package MB       renames Muenblock;
   package Req_Chn  renames Muenblock.Request_Channel;
   package Resp_Chn renames Muenblock.Response_Channel;

   use type PConf.Port_Range;

   ------------------------------------------------------------------------

   --  Memory Setup Fixme: add individual memory regions to cfg
   Command_Lists_Address : constant := Storage_Drv_Cspecs_Wrapper.Memory.Dma_Region_Address;

   Command_Lists_Size    : constant := (PConf.Port_Range'Last + 1) * 16#400#;

   Command_Table_Address : constant :=
      Command_Lists_Address + Command_Lists_Size;
   Command_Table_Size    : constant := (PConf.Port_Range'Last + 1) * 16#100#;

   Fis_Base_Address      : constant :=
      Command_Table_Address + Command_Table_Size;
   Fis_Table_Size        : constant := (PConf.Port_Range'Last + 1) * 16#100#;

   DMA_Mem_Base_Address : constant :=
      Fis_Base_Address + Fis_Table_Size;
   DMA_Mem_Size         : constant := Storage_Drv_Cspecs_Wrapper.Memory.Dma_Region_Size
           - Fis_Table_Size - Command_Table_Size - Command_Lists_Size;

   ------------------------------------------------------------------------
   -- Common Type Definitions
   ------------------------------------------------------------------------

   type Integer_8 is range -2 ** (8 - 1) .. 2 ** (8 - 1) - 1
   with Size => 8;

   type Integer_12 is range -2 ** (12 - 1) .. 2 ** (12 - 1) - 1
   with Size => 12;

   type Integer_16 is range -2 ** (16 - 1) .. 2 ** (16 - 1) - 1
   with Size => 16;

   type Integer_32 is range -2 ** (32 - 1) .. 2 ** (32 - 1) - 1
   with Size => 32;

   type Integer_64 is range -2 ** (64 - 1) .. 2 ** (64 - 1) - 1
   with Size => 64;

   type Unsigned_2 is mod 2 ** 2
   with Size => 2;

   type Unsigned_3 is mod 2 ** 3
   with Size => 3;

   type Unsigned_4 is mod 2 ** 4
   with Size => 4;

   type Unsigned_5 is mod 2 ** 5
   with Size => 5;

   type Unsigned_6 is mod 2 ** 6
   with Size => 6;

   type Unsigned_7 is mod 2 ** 7
   with Size => 7;

   subtype Unsigned_8 is Interfaces.Unsigned_8;

   type Unsigned_9 is mod 2 ** 9
   with Size => 9;

   type Unsigned_10 is mod 2 ** 10
   with Size => 10;

   type Unsigned_12 is mod 2 ** 12
   with Size => 12;

   subtype Unsigned_16 is Interfaces.Unsigned_16;

   type Unsigned_22 is mod 2 ** 22
   with Size => 22;

   type Unsigned_24 is mod 2 ** 24
   with Size => 24;

   type Unsigned_25 is mod 2 ** 25
   with Size => 25;

   type Unsigned_28 is mod 2 ** 28
   with Size => 28;

   type Unsigned_31 is mod 2 ** 31
   with Size => 31;

   type Unsigned_48 is mod 2 ** 48
   with Size => 48;

   subtype Unsigned_32 is Interfaces.Unsigned_32;
   subtype Unsigned_64 is Interfaces.Unsigned_64;
   subtype Unsigned_128 is Interfaces.Unsigned_128;

   type Bit_Array is array (Natural range <>) of Boolean
   with Pack;

   subtype Bit_Array_7  is Bit_Array (0 .. 6);
   subtype Bit_Array_8  is Bit_Array (0 .. 7);
   subtype Bit_Array_32 is Bit_Array (0 .. 31);

   type Byte_Array is array (Natural range <>) of Unsigned_8
   with Pack;

   type Word_Array is array (Natural range <>) of Interfaces.Unsigned_16
   with Pack;

   --  array for internal storage of detected devices
   type Signature_Type is (Empty_device, Sata_device, NVMe_device, Atapi_device);
   type Device_Info is record
      Signature         : Signature_Type;
      Support_48Bit     : Boolean;
      Support_Discard   : Boolean;
      Support_SMART     : Boolean;
      Sector_Size       : Unsigned_32;
      Sector_Size_Shift : Natural;
      Number_Of_Sectors : Unsigned_64;
   end record;

   Null_Device : Device_Info :=
      (Signature         => Empty_device,
       Support_48Bit     => False,
       Support_Discard   => False,
       Support_SMART     => False,
       Sector_Size       => 0,
       Sector_Size_Shift => 0,
       Number_Of_Sectors => 0);

   type Devices_Array is array (PConf.Port_Range) of Device_Info;
   Devices : Devices_Array := (others => Null_Device);

   --  Error constants (matching linux/blk_types.h)
   type Status_Type is  (OK, ENOTSUP, EIO)
      with Size => 8 * 8;
   for  Status_Type use (OK => 0, ENOTSUP => 1, EIO => 10);

   ------------------------------------------------------------------------

   subtype Devs_Array is Bit_Array (0 .. Integer (PConf.Port_Range'Last));

   --  Maximum number of request we combine to a single request
   Requ_Max : constant := 64;
   subtype Tag_Array_Range is Unsigned_32 range 0 .. Requ_Max;
   type Tag_Array_Type is array (Tag_Array_Range) of Unsigned_32;

   type Current_Request_Type is record
      Request_Kind   : MB.Request_Kind_Type;
      Device_Id      : Unsigned_16;
      Device_Offset  : Unsigned_64;
      Buffer_Offset  : Unsigned_64;
      Request_Length : Unsigned_64;
      Tags           : Tag_Array_Type;
      Tag_Idx        : Tag_Array_Range;
   end record;

   Null_Current : constant Current_Request_Type :=
      (Request_Kind   => MB.None,
       Device_Id      => 0,
       Device_Offset  => 0,
       Buffer_Offset  => 0,
       Request_Length => 0,
       Tag_Idx        => 0,
       Tags           => (others => 0));

   type Internal_Device_Type is record
      Ahci_Port     : PConf.Port_Range;
      Partition     : Integer;
      --  Sector offset used for partition addressing
      Is_Valid      : Boolean;
      Sector_Offset : Unsigned_64;
      Sector_Count  : Unsigned_64;
      Current       : Current_Request_Type;
   end record;

   type Internal_Device_Array_Type is array (PConf.Devices_Range)
      of Internal_Device_Type;

   type Port_Type is record
      Chan_Idx : PConf.Channel_Range;
      Devs     : Internal_Device_Array_Type;
   end record;

   type Ports_Array is array (PConf.Ports_Array_Range) of Port_Type;

   procedure Startup;

   procedure Init
      (Devs    : out Devs_Array;
       Ports   : out Ports_Array;
       Success : out Boolean)
   with
      Pre  => Musinfo.Instance.Is_Valid and NVME_Check_Sector_Size,
      Post => (if Success then Is_Valid);

   function Get_Sector_Size (Dev_Id : PConf.Port_Range) return Unsigned_32;
   function Get_Sector_Cnt (Dev_Id : PConf.Port_Range) return Unsigned_64;
   function Get_Max_Sector_Cnt (Dev_Id : PConf.Port_Range) return Unsigned_64;
   function Get_Size (Dev_Id : PConf.Port_Range) return Unsigned_64;

   procedure Execute_Read_Command
      (Address : Unsigned_64;
       SLBA    : Unsigned_64;
       NLB     : Unsigned_32;
       Dev_Id  : PConf.Port_Range;
       Status  : out Status_Type)
   with
      Pre => Is_Valid;

   procedure Execute_Write_Command
      (Address : Unsigned_64;
       SLBA    : Unsigned_64;
       NLB     : Unsigned_32;
       Dev_Id  : PConf.Port_Range;
       Status  : out Status_Type)
   with
      Pre => Is_Valid;

   procedure Execute_Discard_Command
      (SLBA    : Unsigned_64;
       NLB     : Unsigned_32;
       Dev_Id  : PConf.Port_Range;
       Status  : out Status_Type)
   with
      Pre => Is_Valid;

   procedure Check_SMART_Status (Address :     Unsigned_64;
                                 Dev_Id  :     PConf.Port_Range;
                                 Status  : out Unsigned_64)
   with
      Pre => Is_Valid;

   procedure Sync (Dev_Id :      PConf.Port_Range;
                   Status : out  Unsigned_64)
   with
      Pre => Is_Valid;

   function Is_Valid return Boolean with Ghost;

   function NVME_Check_Sector_Size return Boolean with Ghost;
end Storage_Interface;