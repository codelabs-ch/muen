with Ada.Unchecked_Conversion;

package body NVMe.AdminCommandSet is

   -- CDW11_Cvt shall already be converted from specific Field Type to Unsigned32
   procedure CreateSetFeatures_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       FID              :        Unsigned_8;               -- Feature Identifier
       SV               :        Boolean;                  -- Save (persistently)
       UUID_Index       :        Unsigned_7;               -- UUID Index
       CDW11_Cvt        :        Unsigned_32;              -- Already converted FID specific CDW11
       Command          :    out Admin_Command)
   is
      CDW10_Temp : constant CDW10_SET := (FID => FID, SV => SV, others => <>);
      CDW14_Temp : constant CDW14     := (UUID_Index, others => <>);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_SET, Unsigned_32);
      function Cvt_CDW14 is new Ada.Unchecked_Conversion (CDW14, Unsigned_32);

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

   end CreateSetFeatures_Command;

   procedure CreateGetFeatures_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       FID              :        Unsigned_8;               -- Feature Identifier
       SEL              :        Unsigned_3;               -- Select (Attribute of requested Data)
       UUID_Index       :        Unsigned_7;               -- UUID Index
       Command          :    out Admin_Command)
   is
      CDW10_Temp : constant CDW10_GET := (FID, SEL, others => <>);
      CDW14_Temp : constant CDW14     := (UUID_Index, others => <>);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_GET, Unsigned_32);
      function Cvt_CDW14 is new Ada.Unchecked_Conversion (CDW14, Unsigned_32);

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

   end CreateGetFeatures_Command;

   procedure CreateIndentify_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       NSID             :        Unsigned_32;              -- Namespace Identifier
       CNTID            :        Unsigned_16;              -- Controller Identifier
       CNS              :        Unsigned_8;               -- Controller or Namespace Structure
       CSI              :        Unsigned_8;               -- Command Set Identifier
       CNSSpecificIdent :        Unsigned_16;              -- CNS Specific Identifier
       UUID_Index       :        Unsigned_7;               -- UUID Index
       Command          :    out Admin_Command)
   is
      CDW10_Temp  : constant CDW10_Ident := (CNS => CNS, Filler => 0, CNTID => CNTID);
      CDW11_Temp  : constant CDW11_Ident := (CNS_SI => CNSSpecificIdent, Filler => 0, CSI => CSI);
      CDW14_Temp  : constant CDW14       := (UUID_Index, False, 0, 0);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_Ident, Unsigned_32);
      function Cvt_CDW11 is new Ada.Unchecked_Conversion (CDW11_Ident, Unsigned_32);
      function Cvt_CDW14 is new Ada.Unchecked_Conversion (CDW14, Unsigned_32);

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

   end CreateIndentify_Command;

   procedure CreateCreateIOCQ_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       QID              :        Unsigned_16;              -- Queue Identifier
       QSIZE            :        Unsigned_16;              -- Queue Size
       PC               :        Boolean;                  -- Physically Contiguous
       IEN              :        Boolean;                  -- Interrupts Enabled (Default: False)
       Command          :    out Admin_Command)
   is
      CDW10_Temp : constant CDW10_CreateIOQ  := (QID => QID, QSIZE => QSIZE);
      CDW11_Temp : constant CDW11_CreateIOCQ := (PC, IEN, 0, 0, 0);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_CreateIOQ, Unsigned_32);
      function Cvt_CDW11 is new Ada.Unchecked_Conversion (CDW11_CreateIOCQ, Unsigned_32);

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

   end CreateCreateIOCQ_Command;

   procedure CreateCreateIOSQ_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       QID              :        Unsigned_16;              -- Queue Identifier
       QSIZE            :        Unsigned_16;              -- Queue Size
       PC               :        Boolean;                  -- Physically Contiguous
       QPRIO            :        Unsigned_2;               -- Queue Priority
       CQID             :        Unsigned_16;              -- Completion Queue Identifier
       Command          :    out Admin_Command)
   is
      CDW10_Temp : constant CDW10_CreateIOQ  := (QID => QID, QSIZE => QSIZE);
      CDW11_Temp : constant CDW11_CreateIOSQ := (PC => PC, QPRIO => QPRIO, CQID => CQID, others => <>);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_CreateIOQ, Unsigned_32);
      function Cvt_CDW11 is new Ada.Unchecked_Conversion (CDW11_CreateIOSQ, Unsigned_32);

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

   end CreateCreateIOSQ_Command;

   procedure CreateDeleteIOCQ_Command
      (CMD_Identifier   : in out Unsigned_16;     -- Command Identifier
       QID              :        Unsigned_16;     -- Queue Identifier
       Command          :    out Admin_Command)
   is
      CDW10_Temp : constant CDW10_DeleteIOQ  := (QID, others => <>);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_DeleteIOQ, Unsigned_32);

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

   end CreateDeleteIOCQ_Command;

   procedure CreateDeleteIOSQ_Command
      (CMD_Identifier   : in out Unsigned_16;     -- Command Identifier
       QID              :        Unsigned_16;     -- Queue Identifier
       Command          :    out Admin_Command)
   is
      CDW10_Temp : constant CDW10_DeleteIOQ  := (QID, others => <>);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_DeleteIOQ, Unsigned_32);

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

   end CreateDeleteIOSQ_Command;

   procedure CreateGetLogPage_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       LID              :        Unsigned_8;               -- Log Page Identifier
       LSP              :        Unsigned_7;               -- Log Specific Parameter
       RAE              :        Boolean;                  -- Retain Async Event
       NUMDL            :        Unsigned_16;              -- Number of DWORDS Lower
       NUMDU            :        Unsigned_16;              -- Number of DWORDS (16 most significant bits)
       LogSpecificID    :        Unsigned_16;              -- Log Specific Identifier
       Command          :    out Admin_Command)
   is
      CDW10_Temp : constant CDW10_GetLogPage := (LID => LID, LSP => LSP, RAE => RAE, NUMDL => NUMDL);
      CDW11_Temp : constant CDW11_GetLogPage  := (NUMDU => NUMDU, LogSpecificID => LogSpecificID);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_GetLogPage, Unsigned_32);
      function Cvt_CDW11 is new Ada.Unchecked_Conversion (CDW11_GetLogPage, Unsigned_32);

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

   end CreateGetLogPage_Command;

   procedure CreateSMART_Health_LogPage_Command
      (CMD_Identifier   : in out Unsigned_16;              -- Command Identifier
       DPTR             :        SubmissionQ.PRP_Data_Ptr; -- PRP Data Pointer
       Command          :    out Admin_Command)
   is
   begin

      CreateGetLogPage_Command
         (CMD_Identifier => CMD_Identifier,
          DPTR           => DPTR,
          LID            => 2,
          LSP            => 0,
          RAE            => False,
          NUMDL          => 128, --?
          NUMDU          => 0, --?
          LogSpecificID  => 0,
          Command        => Command);

   end CreateSMART_Health_LogPage_Command;

   procedure CreateAbort_Command
      (CMD_Identifier   : in out Unsigned_16;     -- Command Identifier
       CMD_ID2Abort     :        Unsigned_16;     -- Command Identifier of the Command to be aborted
       SQID             :        Unsigned_16;     -- Submission Queue Identifier
       Command          :    out Admin_Command)
   is
      CDW10_Temp : constant CDW10_Abort  := (SQID => SQID, CID2A => CMD_ID2Abort);

      function Cvt_CDW10 is new Ada.Unchecked_Conversion (CDW10_Abort, Unsigned_32);

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

   end CreateAbort_Command;

   procedure CreateAsyncEventReq_Command
      (CMD_Identifier   : in out Unsigned_16; -- Command Identifier
       Command          :    out Admin_Command)
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

   end CreateAsyncEventReq_Command;

end NVMe.AdminCommandSet;
