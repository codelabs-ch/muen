with NVMe.SubmissionQ;
use  NVMe.SubmissionQ;

with Storage_Interface; use Storage_Interface;

with Storage_Drv_Cspecs_Wrapper;

with Interfaces;

package NVMe.Host is
   package PCIE_Memory renames Storage_Drv_Cspecs_Wrapper.Devices;
   package DRAM_Memory renames Storage_Drv_Cspecs_Wrapper.Memory;

   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;

   ---------------------------------------------------
   --- 3.1.3.
   --- Controller Properties
   ---------------------------------------------------

   -- Controller Capabilities
   type CAP_Part is record
      MQES      : Unsigned_16; -- Maximum Queue Entries Supported, Zeros Based
      CQR       : Boolean;     -- Contiguous Queues Required
      AMS       : Unsigned_2;  -- Arbitration Mechanism Supported
      Reserved1 : Bit_Array (3 .. 7);
      TO        : Unsigned_8;  -- Timeout
      DSTRD     : Unsigned_4;  -- Doorbell Stride
      NSSRS     : Boolean;     -- NVM Subsystem Reset Supported
      CSS       : Unsigned_8;  -- Command Sets Supported
      BPS       : Boolean;     -- Boot Partition Support
      CPS       : Unsigned_2;  -- Controller Power Scope
      MPSMIN    : Unsigned_4;  -- Memory Page Size Minimum
      MPSMAX    : Unsigned_4;  -- Memory Page Size Maximum
      PMRS      : Boolean;     -- Persistent Memory Region Supported
      CMBS      : Boolean;     -- Controller Memory Buffer Supported
      NSSS      : Boolean;     -- NVM Subsystem Shutdown Supported
      CRMS      : Unsigned_2;  -- Controller Ready Modes Supported
      Reserved2 : Bit_Array (0 .. 2);
   end record with
     Size => 64, Object_Size => 64;

   for CAP_Part use record
      MQES      at 0 range 0 .. 15;
      CQR       at 2 range 0 ..  0;
      AMS       at 2 range 1 ..  2;
      Reserved1 at 2 range 3 ..  7;
      TO        at 3 range 0 ..  7;
      DSTRD     at 4 range 0 ..  3;
      NSSRS     at 4 range 4 ..  4;
      CSS       at 4 range 5 .. 12;
      BPS       at 5 range 5 ..  5;
      CPS       at 5 range 6 ..  7;
      MPSMIN    at 6 range 0 ..  3;
      MPSMAX    at 6 range 4 ..  7;
      PMRS      at 7 range 0 ..  0;
      CMBS      at 7 range 1 ..  1;
      NSSS      at 7 range 2 ..  2;
      CRMS      at 7 range 3 ..  4;
      Reserved2 at 7 range 5 ..  7;
   end record;

   -- Controller Version
   type VS_Part is record
      Tert  : Unsigned_8;
      Minor : Unsigned_8;
      Major : Unsigned_16; -- NVMe Version >= 1.2.1
   end record with
     Size => 32, Object_Size => 32;

   for VS_Part use record
      Tert  at 0 range  0 ..  7;
      Minor at 0 range  8 .. 15;
      Major at 0 range 16 .. 31;
   end record;

   -- Controller Configuration
   type CC_Part is record -- Offset 14h
      EN        : Boolean;    -- Enable
      Reserved1 : Bit_Array (1 .. 3);
      CSS       : Unsigned_3; -- IO Command Set Selected
      MPS       : Unsigned_4; -- Memory Page Size
      AMS       : Unsigned_3; -- Arbitration Mechanism Selected
      SHN       : Unsigned_2; -- Shutdown Notification
      IOSQES    : Unsigned_4; -- IO SubmissionQueue Entry Size
      IOCQES    : Unsigned_4; -- IO CompletionQueue Entry Size
      CRIME     : Boolean;    -- Controller Ready Independent of Media Enable
      Reserved2 : Bit_Array (0 .. 6);
   end record with
     Size => 32, Object_Size => 32;

   for CC_Part use record
      EN        at 0 range 0 ..  0;
      Reserved1 at 0 range 1 .. 3;
      CSS       at 0 range 4 ..  6;
      MPS       at 0 range 7 .. 10;
      AMS       at 1 range 3 ..  5;
      SHN       at 1 range 6 ..  7;
      IOSQES    at 2 range 0 ..  3;
      IOCQES    at 2 range 4 ..  7;
      CRIME     at 3 range 0 ..  0;
      Reserved2 at 3 range 1 .. 7;
   end record;

   -- Controller Status
   type CSTS_Part is record
      RDY      : Boolean;    -- Ready
      CFS      : Boolean;    -- Controller Fatal Status
      SHST     : Unsigned_2; -- Shutdown Status
      NSSRO    : Boolean;    -- NVM Subsystem Reset Occurred
      PP       : Boolean;    -- Processing Paused
      ST       : Boolean;    -- Shutdown Type
      Reserved : Bit_Array (7 .. 31);
   end record with
     Size => 32;

   for CSTS_Part use record
      RDY      at 0 range 0 ..  0;
      CFS      at 0 range 1 ..  1;
      SHST     at 0 range 2 ..  3;
      NSSRO    at 0 range 4 ..  4;
      PP       at 0 range 5 ..  5;
      ST       at 0 range 6 ..  6;
      Reserved at 0 range 7 .. 31;
   end record;

   -- Admin Queue Attributes
   type AQA_Part is record -- Offset 24h
      ASQS      : Unsigned_12; -- Admin Submission Queue Size, Zero based Value
      Reserved1 : Bit_Array (12 .. 15);
      ACQS      : Unsigned_12; -- Admin Completion Queue Size, Zero based Value
      Reserved2 : Bit_Array (28 .. 31);
   end record with
     Size => 32;

   for AQA_Part use record
      ASQS      at 0 range  0 .. 11;
      Reserved1 at 0 range 12 .. 15;
      ACQS      at 2 range  0 .. 11;
      Reserved2 at 2 range 12 .. 15;
   end record;

   -- Controller Ready Timeouts
   type CRTO_PART is record
      CRWMT : Unsigned_16; -- Controller Ready With Media Timeout
      CRIMT : Unsigned_16; -- Controller Ready Independent of Media Timeout
   end record with
     Size => 32;

   for CRTO_PART use record
      CRWMT at 0 range 0 .. 15;
      CRIMT at 2 range 0 .. 15;
   end record;

   -- CONTROLLER PROPERTIES

   type ControllerProperties is record
      CAP        : CAP_Part;    -- Controller Capabilities
      VS         : VS_Part;     -- Version
      INTMS      : Unsigned_32; -- Interrupt Mask Set
      INTMC      : Unsigned_32; -- Interrupt Mask Clear
      CC         : CC_Part;     -- Controller Configuration
      Reserved_1 : Byte_Array (0 .. 3);
      CSTS       : CSTS_Part;   -- Controller Status
      NSSR       : Unsigned_32; -- NVM Subsystem Reset
      AQA        : AQA_Part;    -- Admin Queue Attributes
      ASQ        : Unsigned_64; -- Admin Submission Queue Base Address
      ACQ        : Unsigned_64; -- Admin Completion Queue Base Address
      CMBLOC     : Unsigned_32; -- Controller Memory Buffer Location
      CMBSZ      : Unsigned_32; -- Controller Memory Buffer Size
      BPINFO     : Unsigned_32; -- Boot Partition Information
      BPRSEL     : Unsigned_32; -- Boot Partition Read Select
      BPMBL      : Unsigned_32; -- Boot Partition Memory Buffer Location
      CMBMSC     : Unsigned_64; -- Controller Memory Buffer Memory Space Control
      Reserved_2 : Byte_Array (0 .. 3);
      CMBSTS     : Unsigned_32; -- Controller Memory Buffer Status
      CMBEBS     : Unsigned_32; -- Controller Memory Buffer Elasticity Buffer Size
      CMBSWTP    : Unsigned_32; -- Controller Memory Buffer Sustained Write Throughput
      NSSD       : Unsigned_32; -- NVM Subsystem Shutdown
      CRTO       : CRTO_PART;   -- Controller Ready Timeouts
   --     PMRCAP  : Unsigned_32; -- Persistent Memory Region Capabilities
   --     PMRCTL  : Unsigned_32; -- Persistent Memory Region Control
   --     PMRSTS  : Unsigned_32; -- Persistent Memory Region Status
   --     PMREBS  : Unsigned_32; -- Persistent Memory Region Elasticity Buffer Size
   --     PMRSWTP : Unsigned_32; -- Persistent Memory Region Sustained Write Throughput
   --     PMRMSCL : Unsigned_32; -- Persistent Memory Region Memory Space Control Lower
   --     PMRMSCU : Unsigned_32; -- Persistent Memory Region Memory Space Control Upper
   --  end record with
   --    Size => 3612*8;
   end record with
     Size => 864, Object_Size => 864;

   for ControllerProperties use record
      CAP     at   0 range  0 .. 63;
      VS      at   8 range  0 .. 31;
      INTMS   at  12 range  0 .. 31;
      INTMC   at  16 range  0 .. 31;
      CC      at  20 range  0 .. 31;
      Reserved_1 at 24 range 0 .. 31;
      CSTS    at  28 range  0 .. 31;
      NSSR    at  32 range  0 .. 31;
      AQA     at  36 range  0 .. 31;
      ASQ     at  40 range  0 .. 63;
      ACQ     at  48 range  0 .. 63;
      CMBLOC  at  56 range  0 .. 31;
      CMBSZ   at  60 range  0 .. 31;
      BPINFO  at  64 range  0 .. 31;
      BPRSEL  at  68 range  0 .. 31;
      BPMBL   at  72 range  0 .. 31;
      CMBMSC  at  76 range  0 .. 63;
      Reserved_2 at 84 range 0 .. 31;
      CMBSTS  at  88 range  0 .. 31;
      CMBEBS  at  92 range  0 .. 31;
      CMBSWTP at  96 range  0 .. 31;
      NSSD    at 100 range  0 .. 31;
      CRTO    at 104 range  0 .. 31;
   --     PMRCAP  at  0 range   0 ..  0;
   --     PMRCTL  at  0 range   0 ..  0;
   --     PMRSTS  at  0 range   0 ..  0;
   --     PMREBS  at  0 range   0 ..  0;
   --     PMRSWTP at  0 range   0 ..  0;
   --     PMRMSCL at  0 range   0 ..  0;
   --     PMRMSCU at  0 range   0 ..  0;
   --  end record with
   --    Size => 3640;
   end record;

   ---------------------------------------------------
   --- 5.17.2.1
   --- Identify Controller Data Structure (CNS 01h)
   ---------------------------------------------------

   --  Power State Descriptor
   type PSD is record
      MP          : Unsigned_16; -- Maximum Power [W] * MXPS
      Reserved_1  : Bit_Array (0 .. 7);
      MXPS        : Boolean;     -- Max Power Scale
      NOPS        : Boolean;     -- Non-Operational State
      Reserved_2  : Bit_Array (2 .. 7);
      ENLAT       : Unsigned_32; -- Entry Latency
      EXLAT       : Unsigned_32; -- Exit Latency
      RRT         : Unsigned_5;  -- Relative Read Throughput
      Reserved_3  : Bit_Array (5 .. 7);
      RRL         : Unsigned_5;  -- Relative Read Latency
      Reserved_4  : Bit_Array (5 .. 7);
      RWT         : Unsigned_5;  -- Relative Write Throughput
      Reserved_5  : Bit_Array (5 .. 7);
      RWL         : Unsigned_5;  -- Relative Write Latency
      Reserved_6  : Bit_Array (5 .. 7);
      IDLP        : Unsigned_16; -- Idle Power [W] * IPS
      Reserved_7  : Bit_Array (0 .. 5);
      IPS         : Unsigned_2;  -- Idle Power Scale
      Reserved_8  : Bit_Array (0 .. 7);
      ACTP        : Unsigned_16; -- Active Power [W] * APS
      APW         : Unsigned_3;  -- Active Power Workload
      Reserved_9  : Bit_Array (3 .. 5);
      APS         : Unsigned_2;  -- Active Power Scale
      Reserved_10 : Byte_Array (0 .. 8);
   end record
   with Size => 256, Object_Size => 256;

   for PSD use record
      MP          at  0 range 0 .. 15;
      Reserved_1  at  2 range 0 ..  7;
      MXPS        at  3 range 0 ..  0;
      NOPS        at  3 range 1 ..  1;
      Reserved_2  at  3 range 2 ..  7;
      ENLAT       at  4 range 0 .. 31;
      EXLAT       at  8 range 0 .. 31;
      RRT         at 12 range 0 ..  4;
      Reserved_3  at 12 range 5 ..  7;
      RRL         at 13 range 0 ..  4;
      Reserved_4  at 13 range 5 ..  7;
      RWT         at 14 range 0 ..  4;
      Reserved_5  at 14 range 5 ..  7;
      RWL         at 15 range 0 ..  4;
      Reserved_6  at 15 range 5 ..  7;
      IDLP        at 16 range 0 .. 15;
      Reserved_7  at 18 range 0 ..  5;
      IPS         at 18 range 6 ..  7;
      Reserved_8  at 19 range 0 ..  7;
      ACTP        at 20 range 0 .. 15;
      APW         at 22 range 0 ..  2;
      Reserved_9  at 22 range 3 ..  5;
      APS         at 22 range 6 ..  7;
      Reserved_10 at 23 range 0 .. 71;
   end record;

   type SerialNumberString         is array (1 ..  20) of Character  with Pack, Object_Size =>   8 *  20;
   type ModelNumberString          is array (1 ..  40) of Character  with Pack, Object_Size =>   8 *  40;
   type FirmwareRevisionString     is array (1 ..   8) of Character  with Pack, Object_Size =>   8 *   8;
   type IEEE_OUI_Identifier        is array (0 ..   2) of Unsigned_8 with Pack, Object_Size =>   8 *   3;
   type NVMSS_QualifiedName_String is array (1 .. 256) of Character  with Pack, Object_Size =>   8 * 256;
   type PowerStateDescriptors      is array (0 ..  31) of PSD        with Pack, Object_Size => 256 *  32;

   type IdentifyController is record
      VID        : Unsigned_16;                -- PCI Vendor ID
      SSVID      : Unsigned_16;                -- PCI Subsystem Vendor ID
      SN         : SerialNumberString;         -- Serial Number (ASCII)
      MN         : ModelNumberString;          -- Model Number (ASCII)
      FR         : FirmwareRevisionString;     -- Firmware Revision (ASCII)
      RAB        : Unsigned_8;                 -- Recommended Arbitration Burst (2^N [Command])
      IEEE       : IEEE_OUI_Identifier;        -- IEEE OUI Identifier
      CMIC       : Unsigned_8;                 -- Controller Multi-Path I/O and Namespace Sharing Capabilities
      MDTS       : Unsigned_8;                 -- Maximum Data Transfer Size (2^N [Minimum Page Size])
      CNTLID     : Unsigned_16;                -- Controller ID
      VER        : Unsigned_32;                -- Version
      RTD3R      : Unsigned_32;                -- RTD3 Resume Latency
      RTD3E      : Unsigned_32;                -- RTD3 Entry Latency
      OAES       : Unsigned_32;                -- Optional Asynchronous Events Supported
      CTRATT     : Unsigned_32;                -- Controller Attributes
      RRLS       : Unsigned_16;                -- Read Recovery Levels Supported
      Reserved_1 : Byte_Array (0 .. 8);
      CNTRLTYPE  : Unsigned_8;                 -- Controller Type
      FGUID      : Unsigned_128;                -- FRU Globally Unique Identifier ### Maybe extend type
      CRDT1      : Unsigned_16;                -- Command Retry Delay Time 1 in [100ms]
      CRDT2      : Unsigned_16;                -- Command Retry Delay Time 2 in [100ms]
      CRDT3      : Unsigned_16;                -- Command Retry Delay Time 3 in [100ms]
      -- NVMe V2.0
      --NVMSR     : Unsigned_8;                 -- NVM Subsytem Report
      --VWCI      : Unsigned_8;                 -- VPD Write Cycle Information
      --MEC       : Unsigned_8;                 -- Managment Endpoint Capabilities
      Reserved_2 : Byte_Array (0 .. 121);
      OACS       : Unsigned_16;                -- Optional Admin Command Support, old: OAPSCS
      ACL        : Unsigned_8;                 -- Abort Command Limit
      AERL       : Unsigned_8;                 -- Asynchronous Event Request Limit
      FRMW       : Unsigned_8;                 -- Firmware Updates
      LPA        : Unsigned_8;                 -- Log Page Attributes
      ELPE       : Unsigned_8;                 -- Error Log Page Entries
      NPSS       : Unsigned_8;                 -- Number of Power States Support
      AVSCC      : Unsigned_8;                 -- Admin Vendor Specific Command Configuration
      APSTA      : Unsigned_8;                 -- Autonomous Power State Transition Attributes
      WCTEMP     : Unsigned_16;                -- Warning Composite Temperature Threshold in [K]
      CCTEMP     : Unsigned_16;                -- Critical Composite Temperature Threshold in [K]
      MTFA       : Unsigned_16;                -- Maximum Time for Firmware Activation
      HMPRE      : Unsigned_32;                -- Host Memory Buffer Preferred Size in [4KiB]
      HMMIN      : Unsigned_32;                -- Host Memory Buffer Minimum Size in [4KiB]
      TNVMCAP    : Unsigned_128;               -- Total NVM Capacity in [B]
      UNVMCAP    : Unsigned_128;               -- Unallocated NVM Capacity in [B]
      RPMBS      : Unsigned_32;                -- Replay Protected Memory Block Support
      EDSTT      : Unsigned_16;                -- Extended Device Self-test Time in [min]
      DSTO       : Unsigned_8;                 -- Device Self-test Options
      FWUG       : Unsigned_8;                 -- Firmware Update Granularity
      KAS        : Unsigned_16;                -- Keep Alive Support in [100ms]
      HCTMA      : Unsigned_16;                -- Host Controlled Thermal Management Attributes
      MNTMT      : Unsigned_16;                -- Minimum Thermal Management Temperature in [K]
      HXTMT      : Unsigned_16;                -- Maximum Thermal Management Temperature in [K]
      SANICAP    : Unsigned_32;                -- Sanitize Capabilities
      HMMINDS    : Unsigned_32;                -- Host Memory Buffer Minimum Descriptor Entry Size in [4KiB]
      HMMAXD     : Unsigned_16;                -- Host Memory Maximum Descriptors Entries
      NSETIDMAX  : Unsigned_16;                -- NVM Set Identifier Maximum
      ENDGIDMAX  : Unsigned_16;                -- Endurance Group Identifier Maximum
      ANATT      : Unsigned_8;                 -- ANA Transition Time
      ANACAP     : Unsigned_8;                 -- Asymmetric Namespace Access Capabilities
      ANAGPRMAX  : Unsigned_32;                -- ANA Group Identifier Maximum
      NANAGPRID  : Unsigned_32;                -- Number of ANA Group Identifiers
      PELS       : Unsigned_32;                -- Persistent Event Log Size in [64KiB]
      Reserved_3 : Byte_Array (0 .. 155);
      SQES       : Unsigned_8;                 -- Submission Queue Entry Size (2^N [B])
      CQES       : Unsigned_8;                 -- Completion Queue Entry Size (2^N [B])
      MAXCMD     : Unsigned_16;                -- Maximum Outstanding Commands
      NN         : Unsigned_32;                -- Number of Namespaces
      ONCS       : Unsigned_16;                -- Optional NVM Command Support
      FUSES      : Unsigned_16;                -- Fused Operation Support
      FNA        : Unsigned_8;                 -- Format NVM Attributes
      VWC        : Unsigned_8;                 -- Volatile Write Cache
      AWUN       : Unsigned_16;                -- Atomic Write Unit Normal in 0's Based [LB]
      AWUPF      : Unsigned_16;                -- Atomic Write Unit Power Fail in 0's Based [LB]
      NVMSCC     : Unsigned_8;                 -- NVM Vendor Specific Command Configuration
      NWPC       : Unsigned_8;                 -- Namespace Write Protection Capabilities
      ACWU       : Unsigned_16;                -- Atomic Compare & Write Unit in 0's Based [LB]
      Reserved_4 : Byte_Array (0 .. 1);
      SGLS       : Unsigned_32;                -- SGL Support
      MNAN       : Unsigned_32;                -- Maximum Number of Allowed Namespaces
      Reserved_5 : Byte_Array (0 .. 223);
      SUBNQN     : NVMSS_QualifiedName_String; -- NVM Subsystem NVMe Qualified Name
      Reserved_6 : Byte_Array (0 .. 1023);
      PSD0       : PowerStateDescriptors;      -- Power State 0 Descriptors
   end record
   with
    Size => 3072 * 8, Object_Size => 3072 * 8;

   for IdentifyController use record
      VID        at    0 range   0 ..   15;
      SSVID      at    2 range   0 ..   15;
      SN         at    4 range   0 ..  159;
      MN         at   24 range   0 ..  319;
      FR         at   64 range   0 ..   63;
      RAB        at   72 range   0 ..    7;
      IEEE       at   73 range   0 ..   23;
      CMIC       at   76 range   0 ..    7;
      MDTS       at   77 range   0 ..    7;
      CNTLID     at   78 range   0 ..   15;
      VER        at   80 range   0 ..   31;
      RTD3R      at   84 range   0 ..   31;
      RTD3E      at   88 range   0 ..   31;
      OAES       at   92 range   0 ..   31;
      CTRATT     at   96 range   0 ..   31;
      RRLS       at  100 range   0 ..   15;
      Reserved_1 at  102 range   0 ..   71;
      CNTRLTYPE  at  111 range   0 ..    7;
      FGUID      at  112 range   0 ..  127;
      CRDT1      at  128 range   0 ..   15;
      CRDT2      at  130 range   0 ..   15;
      CRDT3      at  132 range   0 ..   15;
    --NVMSR      at  253 range   0 ..    7;
    --VWCI       at  254 range   0 ..    7;
    --MEC        at  255 range   0 ..    7;
      Reserved_2 at  134 range   0 ..  975;
      OACS       at  256 range   0 ..   15;
      ACL        at  258 range   0 ..    7;
      AERL       at  259 range   0 ..    7;
      FRMW       at  260 range   0 ..    7;
      LPA        at  261 range   0 ..    7;
      ELPE       at  262 range   0 ..    7;
      NPSS       at  263 range   0 ..    7;
      AVSCC      at  264 range   0 ..    7;
      APSTA      at  265 range   0 ..    7;
      WCTEMP     at  266 range   0 ..   15;
      CCTEMP     at  268 range   0 ..   15;
      MTFA       at  270 range   0 ..   15;
      HMPRE      at  272 range   0 ..   31;
      HMMIN      at  276 range   0 ..   31;
      TNVMCAP    at  280 range   0 ..  127;
      UNVMCAP    at  296 range   0 ..  127;
      RPMBS      at  312 range   0 ..   31;
      EDSTT      at  316 range   0 ..   15;
      DSTO       at  318 range   0 ..    7;
      FWUG       at  319 range   0 ..    7;
      KAS        at  320 range   0 ..   15;
      HCTMA      at  322 range   0 ..   15;
      MNTMT      at  324 range   0 ..   15;
      HXTMT      at  326 range   0 ..   15;
      SANICAP    at  328 range   0 ..   31;
      HMMINDS    at  332 range   0 ..   31;
      HMMAXD     at  336 range   0 ..   15;
      NSETIDMAX  at  338 range   0 ..   15;
      ENDGIDMAX  at  340 range   0 ..   15;
      ANATT      at  342 range   0 ..    7;
      ANACAP     at  343 range   0 ..    7;
      ANAGPRMAX  at  344 range   0 ..   31;
      NANAGPRID  at  348 range   0 ..   31;
      PELS       at  352 range   0 ..   31;
      Reserved_3 at  356 range   0 .. 1247;
      -- Insert missing for 2.0
      SQES       at  512 range   0 ..    7;
      CQES       at  513 range   0 ..    7;
      MAXCMD     at  514 range   0 ..   15;
      NN         at  516 range   0 ..   31;
      ONCS       at  520 range   0 ..   15;
      FUSES      at  522 range   0 ..   15;
      FNA        at  524 range   0 ..    7;
      VWC        at  525 range   0 ..    7;
      AWUN       at  526 range   0 ..   15;
      AWUPF      at  528 range   0 ..   15;
      NVMSCC     at  530 range   0 ..    7;
      NWPC       at  531 range   0 ..    7;
      ACWU       at  532 range   0 ..   15;
      -- Insert missing for 2.0
      Reserved_4 at  534 range   0 ..   15;
      SGLS       at  536 range   0 ..   31;
      MNAN       at  540 range   0 ..   31;
      -- Insert missing for 2.0
      Reserved_5 at  544 range   0 .. 1791;
      SUBNQN     at  768 range   0 .. 2047;
      Reserved_6 at 1024 range   0 .. 8191;
      PSD0       at 2048 range   0 .. 8191;
   end record;

   ---------------------------------------------------
   --- 5.17.2.21
   --- Identify I/O Command Set data structure (CNS 1Ch)
   ---------------------------------------------------

   type IO_CMD_Set_Vector is record
      NVM_CMD_Set             : Boolean := False;
      Key_Value_CMD_Set       : Boolean := False;
      Zoned_Namespace_CMD_Set : Boolean := False;
      Reserved                : Bit_Array (3 .. 63);
   end record
   with Size => 64, Object_Size => 64;

   for IO_CMD_Set_Vector use record
      NVM_CMD_Set             at 0 range 0 ..  0;
      Key_Value_CMD_Set       at 0 range 1 ..  1;
      Zoned_Namespace_CMD_Set at 0 range 2 ..  2;
      Reserved                at 0 range 3 .. 63;
   end record;

   -- last item is where the next one has every flag as false
   type IO_CMD_Set_Array_Index_Type is new Natural range 0 .. 511;
   type IO_CMD_Set_Array is array (IO_CMD_Set_Array_Index_Type) of IO_CMD_Set_Vector
   with Pack, Object_Size => 64 * 512;

   ---------------------------------------------------
   --- 5.17.2.10
   --- Identify Namespace data structure for an Allocated Namespace ID (CNS 11h)
   ---------
   --- NVM Command Set Specification 1.0c : 4.1.5.5; Figure 97
   --- Figure 97: Identify – Identify Namespace Data Structure, NVM Command Set
   ---------------------------------------------------

   type Namespace_ID_List_Index_Type is new Natural range 0 .. 1023;
   type Namespace_ID_List is array (Namespace_ID_List_Index_Type) of Unsigned_32
   with Pack, Object_Size => 32 * 1024;

   type LBA_Format is record
      MS    : Unsigned_16; -- Metadata Size
      LBADS : Unsigned_8;  -- LBA Data Size
      RP    : Unsigned_2;  -- Relative Performance
      Reserved : Bit_Array (26 .. 31);
   end record
   with Size => 32, Object_Size => 32;

   for LBA_Format use record
      MS       at 0 range 0 .. 15;
      LBADS    at 2 range 0 ..  7;
      RP       at 3 range 0 ..  1;
      Reserved at 3 range 2 ..  7;
   end record;

   type LBA_Format_List is array (Unsigned_8 range 0 .. 63) of LBA_Format
   with Pack, Object_Size => 32 * 64;

   type IdentifyNamespace is record
      NSZE       : Unsigned_64;  -- Namespace Size
      NCAP       : Unsigned_64;  -- Namespace Capacity
      NUSE       : Unsigned_64;  -- Namespace Utilization
      NSFEAT     : Unsigned_8;   -- Namespace Features -- type
      NLBAF      : Unsigned_8;   -- Number of LBA Formats
      FLABS      : Unsigned_8;   -- Formatted LBA Size
      MC         : Unsigned_8;   -- Metadata Capabilities
      DPC        : Unsigned_8;   -- End-to-end Data Protection Capabilities -- type
      DPS        : Unsigned_8;   -- End-to-end Data Protectoin Settings -- type
      NMIC       : Unsigned_8;   -- Namespace Multi-path I/O and Namespace Sharing Capabilities
      RESCAP     : Unsigned_8;   -- Reservation Capabilities
      FPI        : Unsigned_8;   -- Format Progress Indicator
      DLFEAT     : Unsigned_8;   -- Deallocate Logical Block Features
      NAWUN      : Unsigned_16;  -- Namespace Atomic Write Unit Normal
      NAWUPF     : Unsigned_16;  -- Namespace Atomic Write Unit Power Fail
      NACWU      : Unsigned_16;  -- Namespace Atomic Compare & Write Unit
      NABSN      : Unsigned_16;  -- Namespace Atomic Boundary Size Normal
      NABO       : Unsigned_16;  -- Namespace Atomic Boundary Offset
      NABSPF     : Unsigned_16;  -- Namespace Atomic Boundary Size Power Fail
      NOIOB      : Unsigned_16;  -- Namespace Optimal I/O Boundary
      NVMCAP     : Unsigned_128; -- NVM Capacity
      NPWG       : Unsigned_16;  -- Namespace Preferred Write Granularity
      NPWA       : Unsigned_16;  -- Namespace Preferred Write Alignment
      NPDG       : Unsigned_16;  -- Namespace Preferred Deallocate Granularity
      NPDA       : Unsigned_16;  -- Namespace Preferred Deallocate Alignment
      NOWS       : Unsigned_16;  -- Namespace Optimal Write Size
      MSSRL      : Unsigned_16;  -- Maximum Single Source Range Length
      MCL        : Unsigned_32;  -- Maximum Copy Length
      MSRC       : Unsigned_8;   -- Maximum Source Range Count
      Reserved_1 : Bit_Array (0 .. 87);
      ANAGRPID   : Unsigned_32;  -- ANA Group Identifier
      Reserved_2 : Bit_Array (0 .. 23);
      NSATTR     : Unsigned_8;   -- Namespace Attributes
      NVMSETID   : Unsigned_16;  -- NVM Set Identifier
      ENDGID     : Unsigned_16;  -- Endurance Group Identifier
      NGUID      : Unsigned_128; -- Namespace Globally Unique Identifier
      EUI64      : Unsigned_64;  -- IEEE Extended Unique Identifier
      LBA_List   : LBA_Format_List;
   end record
   with Size => 384 * 8, Object_Size => 384 * 8;

   for IdentifyNamespace use record
      NSZE      at   0 range 0 ..   63;
      NCAP      at   8 range 0 ..   63;
      NUSE      at  16 range 0 ..   63;
      NSFEAT    at  24 range 0 ..    7;
      NLBAF     at  25 range 0 ..    7;
      FLABS     at  26 range 0 ..    7;
      MC        at  27 range 0 ..    7;
      DPC       at  28 range 0 ..    7;
      DPS       at  29 range 0 ..    7;
      NMIC      at  30 range 0 ..    7;
      RESCAP    at  31 range 0 ..    7;
      FPI       at  32 range 0 ..    7;
      DLFEAT    at  33 range 0 ..    7;
      NAWUN     at  34 range 0 ..   15;
      NAWUPF    at  36 range 0 ..   15;
      NACWU     at  38 range 0 ..   15;
      NABSN     at  40 range 0 ..   15;
      NABO      at  42 range 0 ..   15;
      NABSPF    at  44 range 0 ..   15;
      NOIOB     at  46 range 0 ..   15;
      NVMCAP    at  48 range 0 ..  127;
      NPWG      at  64 range 0 ..   15;
      NPWA      at  66 range 0 ..   15;
      NPDG      at  68 range 0 ..   15;
      NPDA      at  70 range 0 ..   15;
      NOWS      at  72 range 0 ..   15;
      MSSRL     at  74 range 0 ..   15;
      MCL       at  76 range 0 ..   31;
      MSRC      at  80 range 0 ..    7;
      Reserved_1 at 81 range 0 ..   87;
      ANAGRPID  at  92 range 0 ..   31;
      Reserved_2 at 96 range 0 ..   23;
      NSATTR    at  99 range 0 ..    7;
      NVMSETID  at 100 range 0 ..   15;
      ENDGID    at 102 range 0 ..   15;
      NGUID     at 104 range 0 ..  127;
      EUI64     at 120 range 0 ..   63;
      LBA_List  at 128 range 0 .. 2047;
   end record;

   ---------------------------------------------------
   --- Setup
   ---------------------------------------------------

   --   type CompletionQs is array (1 .. 65_535) of CompletionQ.CompletionQ with
   --      Predicate =>
   --       CompletionQs (CompletionQs'First) in CompletionQ.Admin_CompletionQ;

   --   type SubmissionQs is array (1 .. 65_535) of SubmissionQ.SubmissionQ with
   --      Predicate =>
   --       SubmissionQs (SubmissionQs'First) in SubmissionQ.Admin_SubmissionQ;

   --   procedure Setup
   --      (IO_SubmissionQs : out SubmissionQs;
   --       IO_CompletionQs : out CompletionQs);

   CMD_Identifier_Admin     : Unsigned_16 := 0;
   CMD_Identifier_IO        : Unsigned_16 := 0;

   -- constants
   -- MPS in Byte
   Memory_Page_Size : Unsigned_8 := 0;

   type BAR_64Bit is record
      RegionType   : Boolean;
      Width        : Unsigned_2;
      Prefetchable : Boolean;
      Address1     : Unsigned_28;
      Address2     : Unsigned_32;
   end record with
     Size => 64, Object_Size => 64;
   for BAR_64Bit use record
      RegionType   at 0 range 0 ..  0;
      Width        at 0 range 1 ..  2;
      Prefetchable at 0 range 3 ..  3;
      Address1     at 0 range 4 .. 31;
      Address2     at 4 range 0 .. 31;
   end record;

   procedure ProcessAdminCommand
     (AdminCMD  :     Admin_Command;
      Status    : out Status_Type);

   procedure ProcessIOCommand
     (IOCmd  :     IO_Command;
      Status : out Status_Type)
   with Pre => Is_Valid;

   type SMART_Status_Type is (OK, Threshold_Exceeded, Undefined);

   procedure GetSMART
     (SMART_Status : out SMART_Status_Type;
      NVMe_Status  : out Status_Type)
   with Pre => Is_Valid;

   function Get_Size return Unsigned_64;

   function Get_Sector_Cnt return Unsigned_64;

   function Get_Max_Sector_Cnt return Unsigned_64;

   function Get_Sector_Size return Unsigned_32;

   --  function Get_Max_Sector_Count return Unsigned_32;

   ---------------------------------------------------
   --- 3.5 Controller Initialization
   ---------------------------------------------------

   procedure ControllerInit
     (Success : out Boolean)
   with Pre => Check_Sector_Size,
        Post => (if Success then Is_Valid);
   ---------------------------------------------------
   --- 3.6 Controller Shutdown
   ---------------------------------------------------

   procedure ControllerShutdown
   with Pre => Is_Valid;

   ---------------------------------------------------
   function Check_Sector_Size return Boolean is
    (Get_Sector_Size in 512 | 4096)
   with Ghost;

   function Is_Valid return Boolean is
    (Memory_Page_Size <= Unsigned_8 (Unsigned_4'Last) and then Check_Sector_Size)
   with Ghost;

end NVMe.Host;
