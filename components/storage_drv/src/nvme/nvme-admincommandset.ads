with NVMe.SubmissionQ;
use  NVMe.SubmissionQ;
with Interfaces;
with Storage_Interface; use Storage_Interface;

package NVMe.AdminCommandSet is

   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;

   ---------------------------------------------------
   --- 5. Admin Command Set
   ---------------------------------------------------

   type AdminCMD_Type is
      (Delete_IO_SQ, Create_IO_SQ, Get_Log_Page, Delete_IO_CQ, Create_IO_CQ,
       Identify, Abort_CMD, Set_Features, Get_Features, Async_Event_Req, Namespace_Mngmt,
       Firmware_Commit, Firmware_Img_Download, Device_Self_Test, Namespace_Attachment,
       Keep_Alive, Directive_Send, Directive_Receive, Virtualization_Mngmt, NVMe_MI_Send,
       NVMe_MI_Receive, Capacity_Mngmt, Lockdown, Doorbell_Buffer_Config, Fabric_Commands,
       Format_NVM, Security_Send, Security_Receive, Sanitize, Get_LBA_Status);
   for AdminCMD_Type use
      (Delete_IO_SQ           => 16#00#,
       Create_IO_SQ           => 16#01#,
       Get_Log_Page           => 16#02#,
       Delete_IO_CQ           => 16#04#,
       Create_IO_CQ           => 16#05#,
       Identify               => 16#06#,
       Abort_CMD              => 16#08#,
       Set_Features           => 16#09#,
       Get_Features           => 16#0A#,
       Async_Event_Req        => 16#0C#,
       Namespace_Mngmt        => 16#0D#,
       Firmware_Commit        => 16#10#,
       Firmware_Img_Download  => 16#11#,
       Device_Self_Test       => 16#14#,
       Namespace_Attachment   => 16#15#,
       Keep_Alive             => 16#18#,
       Directive_Send         => 16#19#,
       Directive_Receive      => 16#1A#,
       Virtualization_Mngmt   => 16#1C#,
       NVMe_MI_Send           => 16#1D#,
       NVMe_MI_Receive        => 16#1E#,
       Capacity_Mngmt         => 16#20#,
       Lockdown               => 16#24#,
       Doorbell_Buffer_Config => 16#7C#,
       Fabric_Commands        => 16#7F#,
       Format_NVM             => 16#80#,
       Security_Send          => 16#81#,
       Security_Receive       => 16#82#,
       Sanitize               => 16#84#,
       Get_LBA_Status         => 16#86#);

   ---------------------------------------------------
   --- 5.27 Set Features command
   ---------------------------------------------------

   procedure CreateSetFeatures_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       FID              :        Unsigned_8;               -- Feature Identifier
       SV               :        Boolean;                  -- Save (persistently)
       UUID_Index       :        Unsigned_7;               -- UUID Index
       CDW11_Cvt        :        Unsigned_32;              -- Already converted FID specific CDW11
       Command          :    out Admin_Command);

    ---------------------------------------------------
    --- 5.15 Get Features command
    ---------------------------------------------------

   procedure CreateGetFeatures_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       FID              :        Unsigned_8;               -- Feature Identifier
       SEL              :        Unsigned_3;               -- Select (Attribute of requested Data)
       UUID_Index       :        Unsigned_7;               -- UUID Index
       Command          :    out Admin_Command);

    ---------------------------------------------------
    --- 5.17 Identify command
    ---------------------------------------------------

   procedure CreateIndentify_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       NSID             :        Unsigned_32;              -- Namespace Identifier
       CNTID            :        Unsigned_16;              -- Controller Identifier
       CNS              :        Unsigned_8;               -- Controller or Namespace Structure
       CSI              :        Unsigned_8;               -- Command Set Identifier
       CNSSpecificIdent :        Unsigned_16;              -- CNS Specific Identifier
       UUID_Index       :        Unsigned_7;               -- UUID Index
       Command          :    out Admin_Command);

   ---------------------------------------------------
   --- 5.4 Create I/O Completion Queue command
   ---------------------------------------------------

   procedure CreateCreateIOCQ_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       QID              :        Unsigned_16;              -- Queue Identifier
       QSIZE            :        Unsigned_16;              -- Queue Size
       PC               :        Boolean;                  -- Physically Contiguous
       IEN              :        Boolean;                  -- Interrupts Enabled (Default: False)
       Command          :    out Admin_Command);

   ---------------------------------------------------
   --- 5.5 Create I/O Submission Queue command
   ---------------------------------------------------

   procedure CreateCreateIOSQ_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       QID              :        Unsigned_16;              -- Queue Identifier
       QSIZE            :        Unsigned_16;              -- Queue Size
       PC               :        Boolean;                  -- Physically Contiguous
       QPRIO            :        Unsigned_2;               -- Queue Priority
       CQID             :        Unsigned_16;              -- Completion Queue Identifier
       Command          :    out Admin_Command);

   ---------------------------------------------------
   --- 5.6 Delete I/O Completion Queue command
   ---------------------------------------------------

   procedure CreateDeleteIOCQ_Command
      (CMD_Identifier   : in out Unsigned_16;      -- Command Identifier
       QID              :        Unsigned_16;     -- Queue Identifier
       Command          :    out Admin_Command);

    ---------------------------------------------------
    --- 5.7 Delete I/O Submission Queue command
    ---------------------------------------------------

   procedure CreateDeleteIOSQ_Command
      (CMD_Identifier   : in out Unsigned_16;     -- Command Identifier
       QID              :        Unsigned_16;     -- Queue Identifier
       Command          :    out Admin_Command);

   ---------------------------------------------------
   --- 5.16 Get Log Page command
   ---------------------------------------------------

   procedure CreateGetLogPage_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       LID              :        Unsigned_8;               -- Log Page Identifier
       LSP              :        Unsigned_7;               -- Log Specific Parameter
       RAE              :        Boolean;                  -- Retain Async Event
       NUMDL            :        Unsigned_16;              -- Number of DWORDS Lower
       NUMDU            :        Unsigned_16;              -- Number of DWORDS (16 most significant bits)
       LogSpecificID    :        Unsigned_16;              -- Log Specific Identifier
       Command          :    out Admin_Command);

   procedure CreateSMART_Health_LogPage_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       Command          :    out Admin_Command);

   -------------------------------------------
   -- Figure 207: SMART / Health Info Log Page
   -------------------------------------------

   type CriticalWarning_Type is record
      AvailableSpaceBelowThresh   : Boolean;
      TemperatureWarning          : Boolean; -- see Section 5.27.1.3
      ReliabilityDegraded         : Boolean;
      ReadOnlyModeActive          : Boolean; -- see Section 8.12.1
      BackupDeviceFailure         : Boolean;
      PersistMemoryRegionReadOnly : Boolean; -- see Section 8.14
      Reserved                    : Bit_Array (6 .. 7);
   end record with
      Size => 8;
   for CriticalWarning_Type use record
      AvailableSpaceBelowThresh   at 0 range 0 .. 0;
      TemperatureWarning          at 0 range 1 .. 1;
      ReliabilityDegraded         at 0 range 2 .. 2;
      ReadOnlyModeActive          at 0 range 3 .. 3;
      BackupDeviceFailure         at 0 range 4 .. 4;
      PersistMemoryRegionReadOnly at 0 range 5 .. 5;
      Reserved                    at 0 range 6 .. 7;
   end record;

   type TempSensorArray is array (1 .. 8) of Unsigned_16
   with Pack, Object_Size => 16 * 8;                           -- Consists of Temperature Readings [K]

   type SMART_LogPage is record
      CriticalWarning                  : CriticalWarning_Type; -- Indicated Type of Critical Warning
      CompositeTemperature             : Unsigned_16;          -- Current Composite Temp [Kelvin]
      AvailableSpare                   : Unsigned_8;           -- Current Available Space [%]
      AvailableSpareThreshold          : Unsigned_8;           -- Threshold for 'full' [%]
      PercentageUsed                   : Unsigned_8;           -- Estimate of NVM life used [%]
      -- Unused: EnduranceGroupCriticalWarning
      Reserved_1                       : Byte_Array (6 .. 31);
      DataUnitsRead                    : Unsigned_128;         -- Number of 512 Byte Units the Host has read
      DataUnitsWritten                 : Unsigned_128;         -- Number of 512 Byte Units the Host has written
      HostReadCommands                 : Unsigned_128;         -- Number of Host Read CMDs completed by the controller
      HostWriteommands                 : Unsigned_128;         -- Number of Host Write CMDs completed by the controller
      ControllerBusyTime               : Unsigned_128;         -- Amount of time the controller was busy with I/O CMDs [min]
      PowerCycles                      : Unsigned_128;         -- Number of Power Cycles
      PowerOnHours                     : Unsigned_128;         -- Number of (operational) Power-on hours [h]
      UnsafeShutdowns                  : Unsigned_128;         -- Number of unsafe shutdowns
      MediaAndIntegrityErrors          : Unsigned_128;         -- Number of detected unrecoverdd data integrity errors
      NumberOfErrorLogInfoEntries      : Unsigned_128;         -- Number of Error information log Entries
      WarningCompositeTempTime         : Unsigned_32;          -- Amount of time the Composite Temperarature was greater then allowed [min]
      CriticalCompositeTempTime        : Unsigned_32;          -- Amount of time the Composite Temperarature was critical [min]
      TempSensors                      : TempSensorArray;      -- Array for current temperarature reports by sensors 1 .. 8
      ThermalMngmtTemp1TransitionCount : Unsigned_32;          -- Number of times the controller thermal throttled lightly
      ThermalMngmtTemp2TransitionCount : Unsigned_32;          -- Number of times the controller thermal throttled heavily
      TotalThermalMngmtTempTime1       : Unsigned_32;          -- Amount of time the controller thermal throttled lightly [s]
      TotalThermalMngmtTempTime2       : Unsigned_32;          -- Amount of time the controller thermal throttled heavily [s]
      Reserved_2                       : Byte_Array (232 .. 511);
   end record with
      Size => 512 * 8, Object_Size => 512 * 8;
   for SMART_LogPage use record
      CriticalWarning                  at   0 range 0 ..    7;
      CompositeTemperature             at   1 range 0 ..   15;
      AvailableSpare                   at   3 range 0 ..    7;
      AvailableSpareThreshold          at   4 range 0 ..    7;
      PercentageUsed                   at   5 range 0 ..    7;
      Reserved_1                       at   6 range 0 ..  207;
      DataUnitsRead                    at  32 range 0 ..  127;
      DataUnitsWritten                 at  48 range 0 ..  127;
      HostReadCommands                 at  64 range 0 ..  127;
      HostWriteommands                 at  80 range 0 ..  127;
      ControllerBusyTime               at  96 range 0 ..  127;
      PowerCycles                      at 112 range 0 ..  127;
      PowerOnHours                     at 128 range 0 ..  127;
      UnsafeShutdowns                  at 144 range 0 ..  127;
      MediaAndIntegrityErrors          at 160 range 0 ..  127;
      NumberOfErrorLogInfoEntries      at 176 range 0 ..  127;
      WarningCompositeTempTime         at 192 range 0 ..   31;
      CriticalCompositeTempTime        at 196 range 0 ..   31;
      TempSensors                      at 200 range 0 ..  127;
      ThermalMngmtTemp1TransitionCount at 216 range 0 ..   31;
      ThermalMngmtTemp2TransitionCount at 220 range 0 ..   31;
      TotalThermalMngmtTempTime1       at 224 range 0 ..   31;
      TotalThermalMngmtTempTime2       at 228 range 0 ..   31;
      Reserved_2                       at 232 range 0 .. 2239;
   end record;

   ---------------------------------------------------
   --- 5.1 Abort Command
   ---------------------------------------------------

   procedure CreateAbort_Command
      (CMD_Identifier   : in out Unsigned_16;     -- Command Identifier
       CMD_ID2Abort     :        Unsigned_16;     -- Command Identifier of the Command to be aborted
       SQID             :        Unsigned_16;     -- Submission Queue Identifier
       Command          :    out Admin_Command);

   ---------------------------------------------------
   --- 5.2 Async Event Request Command
   ---------------------------------------------------

   procedure CreateAsyncEventReq_Command
      (CMD_Identifier   : in out Unsigned_16;     -- Command Identifier
       Command          :    out Admin_Command);

private

   type CDW14 is record
      UUID_Index : Unsigned_7;
      Filler1    : Boolean := False;
      Filler2    : Unsigned_8 := 0;
      Filler3    : Unsigned_16 := 0;
   end record with
      Size => 32;

   for CDW14 use record
      UUID_Index at 0 range 0 ..  6;
      Filler1    at 0 range 7 ..  7;
      Filler2    at 1 range 0 ..  7;
      Filler3    at 2 range 0 .. 15;
   end record;

   -- SET FEATURE CMD

   type CDW10_SET is record
      FID     : Unsigned_8; -- Feature Identifier
      Filler1 : Unsigned_16 := 0;
      Filler2 : Unsigned_7 := 0;
      SV      : Boolean;    -- Save
   end record with
      Size => 32;

   for CDW10_SET use record
      FID     at 0 range 0 ..  7;
      Filler1 at 1 range 0 .. 15;
      Filler2 at 3 range 0 ..  6;
      SV      at 3 range 7 ..  7;
   end record;

   -- GET FEATURE CMD

   type CDW10_GET is record
      FID     : Unsigned_8; -- Feature Identifier
      SEL     : Unsigned_3; -- Select
      Filler1 : Unsigned_5 := 0;
      Filler2 : Unsigned_16 := 0;
   end record with
      Size => 32;

   for CDW10_GET use record
      FID     at 0 range 0 ..  7;
      SEL     at 1 range 0 ..  2;
      Filler1 at 1 range 3 ..  7;
      Filler2 at 2 range 0 .. 15;
   end record;

   -- IDENTIFY CMD

   type CDW10_Ident is record
      CNS    : Unsigned_8;  -- Controller or Namespace Structure
      Filler : Unsigned_8 := 0;
      CNTID  : Unsigned_16; -- Controller Identifier
   end record with
      Size => 32;

   for CDW10_Ident use record
      CNS    at 0 range 0 ..  7;
      Filler at 1 range 0 ..  7;
      CNTID  at 2 range 0 .. 15;
   end record;

   type CDW11_Ident is record
      CNS_SI : Unsigned_16; -- CNS Specific Identifier
      Filler : Unsigned_8 := 0;
      CSI    : Unsigned_8;  -- Command Set Identifier
   end record with
      Size => 32;

   for CDW11_Ident use record
      CNS_SI at 0 range 0 .. 15;
      Filler at 2 range 0 ..  7;
      CSI    at 3 range 0 ..  7;
   end record;

   -- Create IO Queues CMD

   type CDW10_CreateIOQ is record
      QID   : Unsigned_16; -- Queue ID
      QSIZE : Unsigned_16; -- Queue Size
   end record with
      Size => 32;
   for CDW10_CreateIOQ use record
      QID   at 0 range 0 .. 15;
      QSIZE at 2 range 0 .. 15;
   end record;

   type CDW11_CreateIOCQ is record
      PC      : Boolean;     -- Physically Contiguous
      IEN     : Boolean;     -- Interrupts Enabled
      Filler1 : Unsigned_6 := 0;
      Filler2 : Unsigned_8 := 0;
      IV      : Unsigned_16; -- Interrupt Vector
   end record with
      Size => 32;
   for CDW11_CreateIOCQ use record
      PC      at 0 range 0 ..  0;
      IEN     at 0 range 1 ..  1;
      Filler1 at 0 range 2 ..  7;
      Filler2 at 1 range 0 ..  7;
      IV      at 2 range 0 .. 15;
   end record;

   type CDW11_CreateIOSQ is record
      PC      : Boolean;     -- Physically Contiguous
      QPRIO   : Unsigned_2;  -- Queue Priority
      Filler1 : Unsigned_5 := 0;
      Filler2 : Unsigned_8 := 0;
      CQID    : Unsigned_16; -- Completion Queue Identifier
   end record with
      Size => 32;
   for CDW11_CreateIOSQ use record
      PC      at 0 range 0 ..  0;
      QPRIO   at 0 range 1 ..  2;
      Filler1 at 0 range 3 ..  7;
      Filler2 at 1 range 0 ..  7;
      CQID    at 2 range 0 .. 15;
   end record;

   -- Delete IO Queues CMD
   type CDW10_DeleteIOQ is record
      QID    : Unsigned_16; -- Queue Identifier
      Filler : Unsigned_16 := 0;
   end record with
      Size => 32;
   for CDW10_DeleteIOQ use record
      QID    at 0 range 0 .. 15;
      Filler at 2 range 0 .. 15;
   end record;

   -- Get Log Page CMD
   type CDW10_GetLogPage is record
      LID   : Unsigned_8;  -- Log Page Identifier
      LSP   : Unsigned_7;  -- Log Specific Parameter
      RAE   : Boolean;     -- Retain Async Event
      NUMDL : Unsigned_16; -- Number of DWORDS Lower
   end record with
      Size => 32;
   for CDW10_GetLogPage use record
      LID   at 0 range 0 ..  7;
      LSP   at 1 range 0 ..  6;
      RAE   at 1 range 7 ..  7;
      NUMDL at 2 range 0 .. 15;
   end record;

   type CDW11_GetLogPage is record
      NUMDU         : Unsigned_16; -- Number of DWORDS (16 most significant bits)
      LogSpecificID : Unsigned_16; -- Log Specific Identifier
   end record with
      Size => 32;
   for CDW11_GetLogPage use record
      NUMDU         at 0 range 0 .. 15;
      LogSpecificID at 2 range 0 .. 15;
   end record;

   -- Abort CMD

   type CDW10_Abort is record
      SQID  : Unsigned_16; -- Submission Queue Identifier
      CID2A : Unsigned_16; -- Command Identifier of the Command to be aborted
   end record with
      Size => 32;
   for CDW10_Abort use record
      SQID  at 0 range 0 .. 15;
      CID2A at 2 range 0 .. 15;
   end record;

end NVMe.AdminCommandSet;
