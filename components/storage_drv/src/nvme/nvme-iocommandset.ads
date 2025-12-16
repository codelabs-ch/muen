with NVMe.SubmissionQ;
use  NVMe.SubmissionQ;
with NVMe.Host;
with Interfaces;
with Storage_Interface; use Storage_Interface;

package NVMe.IOCommandSet is

   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_64;

   type IOCmd_Type is
      (Flush, Write, Read, Write_Uncorrectable, Compare, Write_Zeroes, Dataset_Mngmt,
       Verify, Reservation_Register, Reservation_Report, Reservation_Acquire, Reservation_Release, Copy);
   for IOCmd_Type use
      (Flush                => 16#00#,
       Write                => 16#01#,
       Read                 => 16#02#,
       Write_Uncorrectable  => 16#04#,
       Compare              => 16#05#,
       Write_Zeroes         => 16#08#,
       Dataset_Mngmt        => 16#09#,
       Verify               => 16#0C#,
       Reservation_Register => 16#0D#,
       Reservation_Report   => 16#0E#,
       Reservation_Acquire  => 16#11#,
       Reservation_Release  => 16#15#,
       Copy                 => 16#19#);

   -- Dataset Management Type
   type DSM_Type is record
      Access_Frequency   : Unsigned_4;
      Access_Latency     : Unsigned_2;
      Sequential_Request : Boolean;
      Incompressible     : Boolean;
   end record with
      Size => 8;
   for DSM_Type use record
      Access_Frequency   at 0 range 0 .. 3;
      Access_Latency     at 0 range 4 .. 5;
      Sequential_Request at 0 range 6 .. 6;
      Incompressible     at 0 range 7 .. 7;
   end record;

   ---------------------------------------------------
   --- 3.2.4 NVM Command Set Specification 1.0c
   --- Read Command
   ---------------------------------------------------

   procedure CreateRead_Command
      (CMD_Identifier  : in out Unsigned_16;          -- Command Identifier
       DPTR            :    SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       SLBA            :    Unsigned_64;              -- Starting Logical Block Address (LBA)
       NLB             :    Unsigned_16;              -- Number of Logical Blocks
       Command         : out IO_Command)
   with Pre => NVMe.Host.Is_Valid;

   ---------------------------------------------------
   --- 3.2.6 NVM Command Set Specification 1.0c
   --- Write Command
   ---------------------------------------------------

   procedure CreateWrite_Command
      (CMD_Identifier  : in out Unsigned_16;          -- Command Identifier
       DPTR            :    SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       SLBA            :    Unsigned_64;              -- Starting Logical Block Address (LBA)
       NLB             :    Unsigned_16;              -- Number of Logical Blocks
       Command         : out IO_Command)
   with Pre => NVMe.Host.Is_Valid;

   ---------------------------------------------------
   --- 3.2.8 NVM Command Set Specification 1.0c
   --- Write Zeroes Command
   ---------------------------------------------------

   procedure CreateWrite_Zeroes_Command
      (CMD_Identifier  : in out Unsigned_16;          -- Command Identifier
       SLBA            :    Unsigned_64;              -- Starting Logical Block Address (LBA)
       NLB             :    Unsigned_16;              -- Number of Logical Blocks
       Command         : out IO_Command);

   ---------------------------------------------------
   --- 7.1 NVMe Base Spec 2.0c
   --- Flush Command
   ---------------------------------------------------

   procedure CreateFlush_Command
      (CMD_Identifier  : in out Unsigned_16;          -- Command Identifier
       Command         : out IO_Command);

private

   -- Read & Write CMD

   type CDW10and11_RW is record
      CDW10 : Unsigned_32;
      CDW11 : Unsigned_32;
   end record with
      Size => 64;
   for CDW10and11_RW use record
      CDW10 at 0 range 0 .. 31;
      CDW11 at 4 range 0 .. 31;
   end record;

   -- Write

   type CDW12_Write is record
      NLB     : Unsigned_16; -- Number of Logical Blocks
      Filler1 : Unsigned_4 := 0;
      DTYPE   : Unsigned_4;  -- Directive Type
      STC     : Boolean;     -- Storage Tag Check
      Filler2 : Boolean := False;
      PRINFO  : Unsigned_4;  -- Protection Information
      FUA     : Boolean;     -- Force Unit Access
      LR      : Boolean;     -- Limited Retry
   end record with
      Size => 32;
   for CDW12_Write use record
      NLB     at 0 range 0 .. 15;
      Filler1 at 2 range 0 ..  3;
      DTYPE   at 2 range 4 ..  7;
      STC     at 3 range 0 ..  0;
      Filler2 at 3 range 1 ..  1;
      PRINFO  at 3 range 2 ..  5;
      FUA     at 3 range 6 ..  6;
      LR      at 3 range 7 ..  7;
   end record;

   type CDW13_Write is record
      DSM    : DSM_Type;
      Filler : Unsigned_8 := 0;
      DSPEC  : Unsigned_16; -- Directive Specific
   end record with
      Size => 32;
   for CDW13_Write use record
      DSM    at 0 range 0 ..  7;
      Filler at 1 range 0 ..  7;
      DSPEC  at 2 range 0 .. 15;
   end record;

   -- Read

   type CDW12_Read is record
      NLB     : Unsigned_16; -- Number of Logical Blocks
      Filler1 : Unsigned_8 := 0;
      STC     : Boolean;     -- Storage Tag Check
      Filler2 : Boolean := False;
      PRINFO  : Unsigned_4;  -- Protection Information
      FUA     : Boolean;     -- Force Unit Access
      LR      : Boolean;     -- Limited Retry
   end record with
      Size => 32;
   for CDW12_Read use record
      NLB     at 0 range 0 .. 15;
      Filler1 at 2 range 0 ..  7;
      STC     at 3 range 0 ..  0;
      Filler2 at 3 range 1 ..  1;
      PRINFO  at 3 range 2 ..  5;
      FUA     at 3 range 6 ..  6;
      LR      at 3 range 7 ..  7;
   end record;

   type CDW13_Read is record
      DSM     : DSM_Type;
      Filler1 : Unsigned_8 := 0;
      Filler2 : Unsigned_16 := 0;
   end record with
      Size => 32;
   for CDW13_Read use record
      DSM     at 0 range 0 ..  7;
      Filler1 at 1 range 0 ..  7;
      Filler2 at 2 range 0 .. 15;
   end record;

   -- Write Zeroes CMD

   type CDW12_Write_Zeroes is record
      NLB    : Unsigned_16; -- Number of Logical Blocks
      Filler : Unsigned_8 := 0;
      STC    : Boolean;     -- Storage Tag Check
      DEAC   : Boolean;     -- Deallocate
      PRINFO : Unsigned_4;  -- Protection Information
      FUA    : Boolean;     -- Force Unit Access
      LR     : Boolean;     -- Limited Retry
   end record with
      Size => 32;
   for CDW12_Write_Zeroes use record
      NLB    at 0 range 0 .. 15;
      Filler at 2 range 0 ..  7;
      STC    at 3 range 0 ..  0;
      DEAC   at 3 range 1 ..  1;
      PRINFO at 3 range 2 ..  5;
      FUA    at 3 range 6 ..  6;
      LR     at 3 range 7 ..  7;
   end record;

end NVMe.IOCommandSet;
