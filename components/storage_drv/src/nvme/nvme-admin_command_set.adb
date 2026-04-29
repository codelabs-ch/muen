with Ada.Unchecked_Conversion;

package body NVMe.Admin_Command_Set
is

   -- CDW11_Cvt shall already be converted from specific Field Type to Unsigned32
   procedure Create_Set_Features_Command
      (CMD_Identifier : in out Interfaces.Unsigned_16;       -- Command Identifier
       DPTR           :        SubmissionQ.PRP_Data_Ptr;     -- PRP Data Pointer
       FID            :        Interfaces.Unsigned_8;        -- Feature Identifier
       SV             :        Boolean;                      -- Save (persistently)
       UUID_Index     :        Storage_Interface.Unsigned_7; -- UUID Index
       CDW11_Cvt      :        Interfaces.Unsigned_32;       -- Already converted FID specific CDW11
       Command        :    out SubmissionQ.Admin_Command)
   is
      CDW10_Temp : constant CDW10_SET := (FID => FID, SV => SV, others => <>);
      CDW14_Temp : constant CDW14     := (UUID_Index, others => <>);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_SET, Interfaces.Unsigned_32);
      function Cvt_CDW14 is new Ada.Unchecked_Conversion (CDW14, Interfaces.Unsigned_32);

   begin
      Command :=
         (PSDT     => 0,
          OPC      => 9,
          FUSE     => 0,
          Reserved => (others => False),
          CID      => CMD_Identifier,
          NSID     => 16#FFFFFFFF#, -- Scope: Controller
          CDW2     => 0,
          CDW3     => 0,
          MPTR     => 0,
          CDW10    => Cvt_CDW10 (CDW10_Temp),
          CDW11    => CDW11_Cvt,
          CDW12    => 0,
          CDW13    => 0,
          CDW14    => Cvt_CDW14 (CDW14_Temp),
          CDW15    => 0,
          DPRP     => DPTR);

      CMD_Identifier := CMD_Identifier + 1;
   end Create_Set_Features_Command;

   -------------------------------------------------------------------------

   procedure Create_Get_Features_Command
      (CMD_Identifier : in out Interfaces.Unsigned_16;       -- Command Identifier
       DPTR           :        SubmissionQ.PRP_Data_Ptr;     -- PRP Data Pointer
       FID            :        Interfaces.Unsigned_8;        -- Feature Identifier
       SEL            :        Storage_Interface.Unsigned_3; -- Select (Attribute of requested Data)
       UUID_Index     :        Storage_Interface.Unsigned_7; -- UUID Index
       Command        :    out SubmissionQ.Admin_Command)
   is
      CDW10_Temp : constant CDW10_GET := (FID, SEL, others => <>);
      CDW14_Temp : constant CDW14     := (UUID_Index, others => <>);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_GET, Interfaces.Unsigned_32);
      function Cvt_CDW14 is new Ada.Unchecked_Conversion (CDW14, Interfaces.Unsigned_32);

   begin
      Command :=
         (PSDT     => 0,
          OPC      => 10,
          FUSE     => 0,
          Reserved => (others => False),
          CID      => CMD_Identifier,
          NSID     => 16#FFFFFFFF#, -- Scope: Controller
          CDW2     => 0,
          CDW3     => 0,
          MPTR     => 0,
          CDW10    => Cvt_CDW10 (CDW10_Temp),
          CDW11    => 0,
          CDW12    => 0,
          CDW13    => 0,
          CDW14    => Cvt_CDW14 (CDW14_Temp),
          CDW15    => 0,
          DPRP     => DPTR);

      CMD_Identifier := CMD_Identifier + 1;
   end Create_Get_Features_Command;

   -------------------------------------------------------------------------

   procedure Create_Indentify_Command
      (CMD_Identifier   : in out Interfaces.Unsigned_16;       -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr;     -- PRP Data Pointer
       NSID             :        Interfaces.Unsigned_32;       -- Namespace Identifier
       CNTID            :        Interfaces.Unsigned_16;       -- Controller Identifier
       CNS              :        Interfaces.Unsigned_8;        -- Controller or Namespace Structure
       CSI              :        Interfaces.Unsigned_8;        -- Command Set Identifier
       CNSSpecificIdent :        Interfaces.Unsigned_16;       -- CNS Specific Identifier
       UUID_Index       :        Storage_Interface.Unsigned_7; -- UUID Index
       Command          :    out SubmissionQ.Admin_Command)
   is
      CDW10_Temp  : constant CDW10_Ident := (CNS => CNS, Filler => 0, CNTID => CNTID);
      CDW11_Temp  : constant CDW11_Ident := (CNS_SI => CNSSpecificIdent, Filler => 0, CSI => CSI);
      CDW14_Temp  : constant CDW14       := (UUID_Index, False, 0, 0);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_Ident, Interfaces.Unsigned_32);
      function Cvt_CDW11 is new Ada.Unchecked_Conversion (CDW11_Ident, Interfaces.Unsigned_32);
      function Cvt_CDW14 is new Ada.Unchecked_Conversion (CDW14, Interfaces.Unsigned_32);

   begin
      Command :=
         (PSDT     => 0,
          OPC      => 6,
          FUSE     => 0,
          Reserved => (others => False),
          CID      => CMD_Identifier,
          NSID     => NSID,
          CDW2     => 0,
          CDW3     => 0,
          MPTR     => 0,
          CDW10    => Cvt_CDW10 (CDW10_Temp),
          CDW11    => Cvt_CDW11 (CDW11_Temp),
          CDW12    => 0,
          CDW13    => 0,
          CDW14    => Cvt_CDW14 (CDW14_Temp),
          CDW15    => 0,
          DPRP     => DPTR);
      -- Increment unique ID
      CMD_Identifier := CMD_Identifier + 1;
   end Create_Indentify_Command;

   -------------------------------------------------------------------------

   procedure Create_Create_IOCQ_Command
      (CMD_Identifier : in out Interfaces.Unsigned_16;   -- Command Identifier
       DPTR           :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       QID            :        Interfaces.Unsigned_16;   -- Queue Identifier
       QSIZE          :        Interfaces.Unsigned_16;   -- Queue Size
       PC             :        Boolean;                  -- Physically Contiguous
       IEN            :        Boolean;                  -- Interrupts Enabled (Default: False)
       Command        :    out SubmissionQ.Admin_Command)
   is
      CDW10_Temp : constant CDW10_CreateIOQ  := (QID => QID, QSIZE => QSIZE);
      CDW11_Temp : constant CDW11_CreateIOCQ := (PC, IEN, 0, 0, 0);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_CreateIOQ, Interfaces.Unsigned_32);
      function Cvt_CDW11 is new Ada.Unchecked_Conversion (CDW11_CreateIOCQ, Interfaces.Unsigned_32);

   begin
      Command :=
         (PSDT     => 0,
          OPC      => 5,
          FUSE     => 0,
          Reserved => (others => False),
          CID      => CMD_Identifier,
          NSID     => 0,
          CDW2     => 0,
          CDW3     => 0,
          MPTR     => 0,
          CDW10    => Cvt_CDW10 (CDW10_Temp),
          CDW11    => Cvt_CDW11 (CDW11_Temp),
          CDW12    => 0,
          CDW13    => 0,
          CDW14    => 0,
          CDW15    => 0,
          DPRP     => DPTR);
      -- Increment unique ID
      CMD_Identifier := CMD_Identifier + 1;
   end Create_Create_IOCQ_Command;

   -------------------------------------------------------------------------

   procedure Create_Create_IOSQ_Command
      (CMD_Identifier : in out Interfaces.Unsigned_16;       -- Command Identifier
       DPTR           :        SubmissionQ.PRP_Data_Ptr;     -- PRP Data Pointer
       QID            :        Interfaces.Unsigned_16;       -- Queue Identifier
       QSIZE          :        Interfaces.Unsigned_16;       -- Queue Size
       PC             :        Boolean;                      -- Physically Contiguous
       QPRIO          :        Storage_Interface.Unsigned_2; -- Queue Priority
       CQID           :        Interfaces.Unsigned_16;       -- Completion Queue Identifier
       Command        :    out SubmissionQ.Admin_Command)
   is
      CDW10_Temp : constant CDW10_CreateIOQ  := (QID => QID, QSIZE => QSIZE);
      CDW11_Temp : constant CDW11_CreateIOSQ := (PC => PC, QPRIO => QPRIO, CQID => CQID, others => <>);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_CreateIOQ, Interfaces.Unsigned_32);
      function Cvt_CDW11 is new Ada.Unchecked_Conversion (CDW11_CreateIOSQ, Interfaces.Unsigned_32);

   begin
      Command :=
         (PSDT     => 0,
          OPC      => 1,
          FUSE     => 0,
          Reserved => (others => False),
          CID      => CMD_Identifier,
          NSID     => 0,
          CDW2     => 0,
          CDW3     => 0,
          MPTR     => 0,
          CDW10    => Cvt_CDW10 (CDW10_Temp),
          CDW11    => Cvt_CDW11 (CDW11_Temp),
          CDW12    => 0,
          CDW13    => 0,
          CDW14    => 0,
          CDW15    => 0,
          DPRP     => DPTR);
      -- Increment unique ID
      CMD_Identifier := CMD_Identifier + 1;
   end Create_Create_IOSQ_Command;

   -------------------------------------------------------------------------

   procedure Create_Delete_IOCQ_Command
      (CMD_Identifier : in out Interfaces.Unsigned_16;     -- Command Identifier
       QID            :        Interfaces.Unsigned_16;     -- Queue Identifier
       Command        :    out SubmissionQ.Admin_Command)
   is
      CDW10_Temp : constant CDW10_DeleteIOQ  := (QID, others => <>);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_DeleteIOQ, Interfaces.Unsigned_32);

   begin
      Command :=
         (PSDT     => 0,
          OPC      => 4,
          FUSE     => 0,
          Reserved => (others => False),
          CID      => CMD_Identifier,
          NSID     => 0,
          CDW2     => 0,
          CDW3     => 0,
          MPTR     => 0,
          CDW10    => Cvt_CDW10 (CDW10_Temp),
          CDW11    => 0,
          CDW12    => 0,
          CDW13    => 0,
          CDW14    => 0,
          CDW15    => 0,
          DPRP     => (0, 0));
      -- Increment unique ID
      CMD_Identifier := CMD_Identifier + 1;
   end Create_Delete_IOCQ_Command;

   -------------------------------------------------------------------------

   procedure Create_Delete_IOSQ_Command
      (CMD_Identifier : in out Interfaces.Unsigned_16;     -- Command Identifier
       QID            :        Interfaces.Unsigned_16;     -- Queue Identifier
       Command        :    out SubmissionQ.Admin_Command)
   is
      CDW10_Temp : constant CDW10_DeleteIOQ  := (QID, others => <>);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_DeleteIOQ, Interfaces.Unsigned_32);

   begin
      Command :=
         (PSDT     => 0,
          OPC      => 0,
          FUSE     => 0,
          Reserved => (others => False),
          CID      => CMD_Identifier,
          NSID     => 0,
          CDW2     => 0,
          CDW3     => 0,
          MPTR     => 0,
          CDW10    => Cvt_CDW10 (CDW10_Temp),
          CDW11    => 0,
          CDW12    => 0,
          CDW13    => 0,
          CDW14    => 0,
          CDW15    => 0,
          DPRP     => (0, 0));
      -- Increment unique ID
      CMD_Identifier := CMD_Identifier + 1;
   end Create_Delete_IOSQ_Command;

   -------------------------------------------------------------------------

   procedure Create_Get_Log_Page_Command
      (CMD_Identifier : in out Interfaces.Unsigned_16;       -- Command Identifier
       DPTR           :        SubmissionQ.PRP_Data_Ptr;     -- PRP Data Pointer
       LID            :        Interfaces.Unsigned_8;        -- Log Page Identifier
       LSP            :        Storage_Interface.Unsigned_7; -- Log Specific Parameter
       RAE            :        Boolean;                      -- Retain Async Event
       NUMDL          :        Interfaces.Unsigned_16;       -- Number of DWORDS Lower
       NUMDU          :        Interfaces.Unsigned_16;       -- Number of DWORDS (16 most significant bits)
       LogSpecificID  :        Interfaces.Unsigned_16;       -- Log Specific Identifier
       Command        :    out SubmissionQ.Admin_Command)
   is
      CDW10_Temp : constant CDW10_GetLogPage := (LID => LID, LSP => LSP, RAE => RAE, NUMDL => NUMDL);
      CDW11_Temp : constant CDW11_GetLogPage  := (NUMDU => NUMDU, LogSpecificID => LogSpecificID);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_GetLogPage, Interfaces.Unsigned_32);
      function Cvt_CDW11 is new Ada.Unchecked_Conversion (CDW11_GetLogPage, Interfaces.Unsigned_32);

   begin
      Command :=
         (PSDT     => 0,
          OPC      => 2,
          FUSE     => 0,
          Reserved => (others => False),
          CID      => CMD_Identifier,
          NSID     => 16#FFFFFFFF#, -- Scope: Controller
          CDW2     => 0,
          CDW3     => 0,
          MPTR     => 0,
          CDW10    => Cvt_CDW10 (CDW10_Temp),
          CDW11    => Cvt_CDW11 (CDW11_Temp),
          CDW12    => 0, -- ?? TODO Log Page Offset Lower
          CDW13    => 0,
          CDW14    => 0,
          CDW15    => 0,
          DPRP     => DPTR);
      -- Increment unique ID
      CMD_Identifier := CMD_Identifier + 1;
   end Create_Get_Log_Page_Command;

   -------------------------------------------------------------------------

   procedure Create_SMART_Health_Log_Page_Command
      (CMD_Identifier : in out Interfaces.Unsigned_16;   -- Command Identifier
       DPTR           :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       Command        :    out SubmissionQ.Admin_Command)
   is
      SMART_Log_Page_DWords : constant := 127;
   begin

      Create_Get_Log_Page_Command
         (CMD_Identifier => CMD_Identifier,
          DPTR           => DPTR,
          LID            => 2,
          LSP            => 0,
          RAE            => False,
          NUMDL          => SMART_Log_Page_DWords,
          NUMDU          => 0,
          LogSpecificID  => 0,
          Command        => Command);
      -- no CMD ID increment due to existing increment in Create_Get_Log_Page_Command
   end Create_SMART_Health_Log_Page_Command;

   -------------------------------------------------------------------------

   procedure Create_Abort_Command
      (CMD_Identifier : in out Interfaces.Unsigned_16;     -- Command Identifier
       CMD_ID2Abort   :        Interfaces.Unsigned_16;     -- Command Identifier of the Command to be aborted
       SQID           :        Interfaces.Unsigned_16;     -- Submission Queue Identifier
       Command        :    out SubmissionQ.Admin_Command)
   is
      CDW10_Temp : constant CDW10_Abort  := (SQID => SQID, CID2A => CMD_ID2Abort);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_Abort, Interfaces.Unsigned_32);

   begin
      Command :=
         (PSDT     => 0,
          OPC      => 8,
          FUSE     => 0,
          Reserved => (others => False),
          CID      => CMD_Identifier,
          NSID     => 0,
          CDW2     => 0,
          CDW3     => 0,
          MPTR     => 0,
          CDW10    => Cvt_CDW10 (CDW10_Temp),
          CDW11    => 0,
          CDW12    => 0,
          CDW13    => 0,
          CDW14    => 0,
          CDW15    => 0,
          DPRP     => (0, 0));
      -- Increment unique ID
      CMD_Identifier := CMD_Identifier + 1;
   end Create_Abort_Command;

   -------------------------------------------------------------------------

   procedure Create_Async_Event_Req_Command
      (CMD_Identifier : in out Interfaces.Unsigned_16; -- Command Identifier
       Command        :    out SubmissionQ.Admin_Command)
   is
   begin
      Command :=
         (PSDT     => 0,
          OPC      => 16#0C#,
          FUSE     => 0,
          Reserved => (others => False),
          CID      => CMD_Identifier,
          NSID     => 0,
          CDW2     => 0,
          CDW3     => 0,
          MPTR     => 0,
          CDW10    => 0,
          CDW11    => 0,
          CDW12    => 0,
          CDW13    => 0,
          CDW14    => 0,
          CDW15    => 0,
          DPRP     => (0, 0));
      -- Increment unique ID
      CMD_Identifier := CMD_Identifier + 1;
   end Create_Async_Event_Req_Command;

end NVMe.Admin_Command_Set;
