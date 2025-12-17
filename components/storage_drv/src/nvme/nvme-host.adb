with Ada.Unchecked_Conversion;

with System;

with SK.Strings;

with Log;

with NVMe.AdminCommandSet;
with NVMe.CompletionQ;

package body NVMe.Host
is

   -------------------------------------------------------------------------
   -- Named Address Numbers
   -------------------------------------------------------------------------

   -- PCIE mmconf
   Bar0_Offset     : constant := 16#10#;
   Bar0_Address    : constant := PCIE_Memory.Controller_Mmconf_Address + Bar0_Offset;

   -- Controller Memory
   CProp_Offset    : constant := 0;
   CProp_Address   : constant := PCIE_Memory.Controller_Mem1_Address + CProp_Offset;

   -- Queue Memory
   ASQ_Offset      : constant := 0;
   ASQ_Address     : constant := DRAM_Memory.Queue_Memory_Address + ASQ_Offset;
   ACQ_Offset      : constant := 16#0001_0000#;
   ACQ_Address     : constant := DRAM_Memory.Queue_Memory_Address + ACQ_Offset;
   IOSQ_Offset     : constant := 16#0002_0000#;
   IOSQ_Address    : constant := DRAM_Memory.Queue_Memory_Address + IOSQ_Offset;
   IOCQ_Offset     : constant := 16#0003_0000#;
   IOCQ_Address    : constant := DRAM_Memory.Queue_Memory_Address + IOCQ_Offset;

   -- Controller Memory
   ASQ_TD_Offset   : constant := 16#1000#;
   ASQ_TD_Address  : constant := PCIE_Memory.Controller_Mem1_Address + ASQ_TD_Offset;
   ACQ_HD_Offset   : constant := 16#1004#;
   ACQ_HD_Address  : constant := PCIE_Memory.Controller_Mem1_Address + ACQ_HD_Offset;
   IOSQ_TD_Offset  : constant := 16#1008#;
   IOSQ_TD_Address : constant := PCIE_Memory.Controller_Mem1_Address + IOSQ_TD_Offset;
   IOCQ_HD_Offset  : constant := 16#100C#;
   IOCQ_HD_Address : constant := PCIE_Memory.Controller_Mem1_Address + IOCQ_HD_Offset;

   -- DRAM Memory
   Ident_Controller_Offset         : constant := 16#0004_0000#;
   Ident_Controller_Address        : constant := DRAM_Memory.Queue_Memory_Address + Ident_Controller_Offset;
   IO_CMD_Sets_Offset              : constant := 16#0005_0000#;
   IO_CMD_Sets_Address             : constant := DRAM_Memory.Queue_Memory_Address + IO_CMD_Sets_Offset;
   Namespace_List_Offset           : constant := 16#0006_0000#;
   Namespace_List_Address          : constant := DRAM_Memory.Queue_Memory_Address + Namespace_List_Offset;
   Ident_Namespace_Offset          : constant := 16#0007_0000#;
   Ident_Namespace_Address         : constant := DRAM_Memory.Queue_Memory_Address + Ident_Namespace_Offset;
   SMART_Health_LogPage_Offset     : constant := 16#0009_0000#;
   SMART_Health_LogPage_Address    : constant := DRAM_Memory.Queue_Memory_Address + SMART_Health_LogPage_Offset;

   Test_IO_Write_Offset    : constant := 16#0008_0000#;
   Test_IO_Write_Address   : constant := DRAM_Memory.Queue_Memory_Address + Test_IO_Write_Offset;
   Test_IO_Read_Offset     : constant := 16#0008_0020#;
   Test_IO_Read_Address    : constant := DRAM_Memory.Queue_Memory_Address + Test_IO_Read_Offset;

   -------------------------------------------------------------------------
   -- Controller Data Structures
   -------------------------------------------------------------------------

   pragma Warnings
     (GNATprove, Off,
      "indirect writes to * through a potential alias are ignored",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   BAR0 : BAR_64Bit
   with
      Volatile, Async_Readers, Async_Writers,
      Address =>
       System'To_Address (Bar0_Address);

   CProp : ControllerProperties
   with
      Volatile, Async_Readers, Async_Writers,
      Address =>
       System'To_Address (CProp_Address);

   -------------------------------------------------------------------------
   -- Admin Submission Queue
   -------------------------------------------------------------------------

   ASQ : SubmissionQ.Entry_Queue := (others => SubmissionQ.Null_SQE)
   with
      Volatile, Async_Readers, Effective_Writes,
      Address =>
       System'To_Address (ASQ_Address);

   ASQ_TailDoorbell : Unsigned_32 := 0
   with
      Volatile, Async_Readers, Effective_Writes,
      Address =>
       System'To_Address (ASQ_TD_Address);

   ASQ_Index : SubmissionQ.Entry_Queue_Range := 0;

   -------------------------------------------------------------------------
   -- Admin Completion Queue
   -------------------------------------------------------------------------

   ACQ : CompletionQ.Entry_Queue := (others => CompletionQ.Null_CQE)
   with
     Volatile, Async_Readers, Async_Writers,
     Address =>
      System'To_Address (ACQ_Address);

   ACQ_HeadDoorbell : Unsigned_32 := 0
   with
      Volatile, Async_Readers, Effective_Writes,
      Address =>
       System'To_Address (ACQ_HD_Address);

   ACQ_Index     : CompletionQ.Entry_Queue_Range := 0;
   ACQ_PhaseTag  : Boolean    := False;

   -------------------------------------------------------------------------
   -- IO Submission Queue
   -------------------------------------------------------------------------

   IOSQ : SubmissionQ.Entry_Queue := (others => SubmissionQ.Null_SQE)
   with
      Volatile, Async_Readers,
      Address =>
       System'To_Address (IOSQ_Address);

   IOSQ_TailDoorbell : Unsigned_32 := 0
   with
      Volatile, Async_Readers, Effective_Writes,
      Address =>
       System'To_Address (IOSQ_TD_Address);

   IOSQ_Index : SubmissionQ.Entry_Queue_Range := 0;

   -------------------------------------------------------------------------
   -- IO Completion Queue
   -------------------------------------------------------------------------

   IOCQ : CompletionQ.Entry_Queue := (others => CompletionQ.Null_CQE)
   with
      Volatile, Async_Readers, Async_Writers,
      Address =>
       System'To_Address (IOCQ_Address);

   IOCQ_HeadDoorbell : Unsigned_32 := 0
   with
      Volatile, Async_Readers, Effective_Writes,
      Address =>
       System'To_Address (IOCQ_HD_Address);

   IOCQ_Index    : CompletionQ.Entry_Queue_Range := 0;
   IOCQ_PhaseTag : Boolean    := False;

   -------------------------------------------------------------------------
   -- Other Declarations
   -------------------------------------------------------------------------

   IdentController : IdentifyController
   with
      Volatile, Async_Writers,
       Address => System'To_Address (Ident_Controller_Address);

   -- last item is where the next one has every flag as false
   IO_CMD_Sets : IO_CMD_Set_Array
   with
      Volatile, Async_Writers, Import,
       Address => System'To_Address (IO_CMD_Sets_Address);

   IO_CMD_Set_Index : IO_CMD_Set_Array_Index_Type := 0;

   Namespace_List : Namespace_ID_List := (others => 0)
   with
      Volatile, Async_Writers,
       Address => System'To_Address (Namespace_List_Address);

   IdentNamespace : IdentifyNamespace
   with
      Volatile, Async_Writers,
       Address => System'To_Address (Ident_Namespace_Address);

   IO_Write_TestNum : Unsigned_32 := 69_420
   with
      Address => System'To_Address (Test_IO_Write_Address);

   IO_Read_TestNum : Unsigned_32 := 0
   with
      Volatile, Async_Writers,
      Address => System'To_Address (Test_IO_Read_Address);

   SMART_Health_LogPage : AdminCommandSet.SMART_LogPage
   with
      Volatile, Async_Writers,
      Address => System'To_Address (SMART_Health_LogPage_Address);

   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");
   pragma Warnings
     (GNATprove, On,
      "indirect writes to * through a potential alias are ignored");

   -- MUENBLOCK Constants
   MB_Size             : Unsigned_64 := 0;
   MB_Sector_Count     : Unsigned_64 := 0;
   MB_Max_Sector_Count : Unsigned_64 := 0;
   MB_Sector_Size      : Unsigned_32 := 512;

   -------------------------------------------------------------------------

   pragma Warnings (GNATprove, Off, "subprogram ""Wait_For_Ready"" has no effect");
   procedure Wait_For_Ready (Should_Be : Boolean)
   is
   begin
      declare
         Ready : Boolean := not Should_Be;
      begin
         while Ready /= Should_Be
         loop
            Ready := CProp.CSTS.RDY;
         end loop;
      end;

   end Wait_For_Ready;
   pragma Warnings (GNATprove, On, "subprogram ""Wait_For_Ready"" has no effect");

   -------------------------------------------------------------------------

   procedure Set_Muenblock_Constants (NS : IdentifyNamespace)
   with Pre => NS.LBA_List (0).LBADS = 9 or else NS.LBA_List (0).LBADS = 12,
        Post => MB_Sector_Size = 512 or else MB_Sector_Size = 4096
   is
   begin
      MB_Sector_Count := NS.NSZE;
      MB_Max_Sector_Count := NS.NCAP;
      MB_Sector_Size := Unsigned_32 (2 ** Natural (NS.LBA_List (0).LBADS));
      MB_Size := MB_Sector_Count * Unsigned_64 (MB_Sector_Size);
   end Set_Muenblock_Constants;

   -------------------------------------------------------------------------

   procedure ProcessAdminCommand
      (AdminCMD :     SubmissionQ.Admin_Command;
       Status   : out Status_Type)
   is
      use type CompletionQ.Entry_Queue_Range;
      use type SubmissionQ.Entry_Queue_Range;

      Temp_CQE       : CompletionQ.CQE;
   begin

      ASQ (ASQ_Index) := AdminCMD;
      if ASQ_Index = ASQ'Last then
         -- Queue Wrap
         ASQ_Index := 0;
      else
         ASQ_Index := ASQ_Index + 1;
      end if;
      ASQ_TailDoorbell := Unsigned_32 (ASQ_Index);
      Status := Unknown;

      loop
         Temp_CQE := ACQ (ACQ_Index);
         exit when Temp_CQE.P /= ACQ_PhaseTag;
         -- Todo Timeout?
      end loop;
      -- TODO Clenaup if statement if no timeout
      if Temp_CQE.CID = AdminCMD.CID and Temp_CQE.Status.SC = 0 then
         Log.Put_String ("Successfully executed Admin Command ");
         Log.Put_NVMe_AdminCMD_Image (AdminCMD.OPC);
         Log.New_Line;
         Status := OK;
      elsif Temp_CQE.CID = AdminCMD.CID and Temp_CQE.Status.SC > 0 then
         Log.Put_String ("Error: Admin CMD failed: ");
         Log.Put_NVMe_AdminCMD_Image (AdminCMD.OPC);
         Log.New_Line;
         Log.Put_Line ("CID"  & SK.Strings.Img_Dec (Unsigned_64 (Temp_CQE.CID)));
         Log.Put_Line ("SQHD" & SK.Strings.Img_Dec (Unsigned_64 (Temp_CQE.SQHD)));
         Log.Put_Line ("SQID" & SK.Strings.Img_Dec (Unsigned_64 (Temp_CQE.SQID)));
         Log.Put_Line ("P "   & Log.Boolean_Image (Temp_CQE.P));
         Log.Put_Line ("SC"   & SK.Strings.Img_Dec (Unsigned_64 (Temp_CQE.Status.SC)) &
                       " SCT" & SK.Strings.Img_Dec (Unsigned_64 (Temp_CQE.Status.SCT)));
         Log.Print_Status_Code (SC => Temp_CQE.Status.SC, SCT => Temp_CQE.Status.SCT);
         Status := Fail;
      end if;

      if ACQ_Index = ACQ'Last then
         -- Queue Wrap
         ACQ_Index := 0;
         -- Inverting Phase Tag
         ACQ_PhaseTag := not ACQ_PhaseTag;
      else
         ACQ_Index := ACQ_Index + 1;
      end if;
      ACQ_HeadDoorbell := Unsigned_32 (ACQ_Index);

   end ProcessAdminCommand;

   -------------------------------------------------------------------------

   procedure GetSMART
     (Address      :     Unsigned_64;
      SMART_Status : out SMART_Status_Type;
      NVMe_Status  : out Status_Type)
   is
      Admin_CMD    : SubmissionQ.Admin_Command;
      PRP_D_Ptr    : SubmissionQ.PRP_Data_Ptr := (0, 0);
      No_Warning   : constant AdminCommandSet.CriticalWarning_Type :=
        (AvailableSpaceBelowThresh => False,
         TemperatureWarning => False,
         ReliabilityDegraded => False,
         ReadOnlyModeActive => False,
         BackupDeviceFailure => False,
         PersistMemoryRegionReadOnly => False,
         Reserved => (False, False));

   begin

      PRP_D_Ptr.E1 := Address;

      AdminCommandSet.CreateSMART_Health_LogPage_Command
                     (CMD_Identifier => CMD_Identifier_Admin,
                      DPTR           => PRP_D_Ptr,
                      Command        => Admin_CMD);
      ProcessAdminCommand (Admin_CMD, NVMe_Status);

      if NVMe_Status /= OK then
         SMART_Status := Undefined;
         return;
      end if;

      -- execute again so we get the logpage also on our specified location to check flags
      PRP_D_Ptr.E1 := SMART_Health_LogPage_Address;

      AdminCommandSet.CreateSMART_Health_LogPage_Command
                     (CMD_Identifier => CMD_Identifier_Admin,
                      DPTR           => PRP_D_Ptr,
                      Command        => Admin_CMD);
      ProcessAdminCommand (Admin_CMD, NVMe_Status);

      if NVMe_Status = OK
      then
         declare
            use type AdminCommandSet.CriticalWarning_Type;
            LogPage : constant AdminCommandSet.SMART_LogPage := SMART_Health_LogPage;
         begin
            if LogPage.CriticalWarning = No_Warning then
               SMART_Status := OK;
            elsif LogPage.CriticalWarning.AvailableSpaceBelowThresh then
               SMART_Status := Threshold_Exceeded;
            else
               SMART_Status := Undefined;
            end if;
         end;
      else
         SMART_Status := Undefined;
      end if;
   end GetSMART;

   -------------------------------------------------------------------------

   procedure ProcessIOCommand
      (IOCmd  :     SubmissionQ.IO_Command;
       Status : out Status_Type)
   is
      use type CompletionQ.Entry_Queue_Range;
      use type SubmissionQ.Entry_Queue_Range;

      Temp_CQE : CompletionQ.CQE;
   begin

      IOSQ (IOSQ_Index)  := IOCmd;
      if IOSQ_Index = IOSQ'Last then
         -- Queue Wrap
         IOSQ_Index := 0;
      else
         IOSQ_Index := IOSQ_Index + 1;
      end if;
      IOSQ_TailDoorbell  := Unsigned_32 (IOSQ_Index);
      Status := Unknown;

      loop
         Temp_CQE := IOCQ (IOCQ_Index);
         exit when Temp_CQE.P /= IOCQ_PhaseTag and Temp_CQE.CID = IOCmd.CID;
      end loop;

      if Temp_CQE.CID = IOCmd.CID and Temp_CQE.Status.SC = 0 then
         Status := OK;
      elsif Temp_CQE.CID = IOCmd.CID and Temp_CQE.Status.SC > 0 then
         Log.Put_String ("Error: IO CMD failed: ");
         Log.Put_NVMe_IOCMD_Image (IOCmd.OPC);
         Log.New_Line;
         Log.Put_Line ("CID  " & SK.Strings.Img_Dec (Unsigned_64 (Temp_CQE.CID)));
         Log.Put_Line ("SQHD " & SK.Strings.Img_Dec (Unsigned_64 (Temp_CQE.SQHD)));
         Log.Put_Line ("SQID " & SK.Strings.Img_Dec (Unsigned_64 (Temp_CQE.SQID)));
         Log.Put_Line ("P    " & Log.Boolean_Image (Temp_CQE.P));
         Log.Put_Line ("SC   " & SK.Strings.Img_Dec (Unsigned_64 (Temp_CQE.Status.SC)) &
                       " SCT " & SK.Strings.Img_Dec (Unsigned_64 (Temp_CQE.Status.SCT)));
         Log.Print_Status_Code (SC => Temp_CQE.Status.SC, SCT => Temp_CQE.Status.SCT);
         Status := Fail;
      end if;

      if IOCQ_Index = IOCQ'Last then
         -- Queue Wrap
         IOCQ_Index := 0;
         -- Inverting Phase Tag
         IOCQ_PhaseTag := not IOCQ_PhaseTag;
      else
         IOCQ_Index := IOCQ_Index + 1;
      end if;
      IOCQ_HeadDoorbell := Unsigned_32 (IOCQ_Index);

   end ProcessIOCommand;

   -------------------------------------------------------------------------
   --- 3.5 Controller Initialization
   -------------------------------------------------------------------------

   procedure ControllerInit
     (Success : out Boolean)
   is
      use type CompletionQ.Entry_Queue_Range;

      type CDW11_SetIOSetsType is record
         IOCSCI   : IO_CMD_Set_Array_Index_Type;
         Filler1  : Unsigned_7 := 0;
         Filler2  : Unsigned_16 := 0;
      end record with Size => 32;
      for CDW11_SetIOSetsType use record
         IOCSCI   at 0 range 0 ..  8;
         Filler1  at 1 range 1 ..  7;
         Filler2  at 2 range 0 .. 15;
      end record;

      type CDW11_NumOfQsType is record
         NSQR : Unsigned_16;
         NCQR : Unsigned_16;
      end record with Size => 32;
      for CDW11_NumOfQsType use record
         NSQR at 0 range 0 .. 15;
         NCQR at 2 range 0 .. 15;
      end record;

      function uInt8ToBitArray   is new Ada.Unchecked_Conversion (Unsigned_8, Bit_Array_8);
      function IOCMDIndexToCDW11 is new Ada.Unchecked_Conversion (CDW11_SetIOSetsType, Unsigned_32);
      function NumOfQsToCDW11    is new Ada.Unchecked_Conversion (CDW11_NumOfQsType, Unsigned_32);

      CSS_BitArray        : Bit_Array_8;
      Admin_CMD           : SubmissionQ.Admin_Command;
      PRP_D_Ptr           : SubmissionQ.PRP_Data_Ptr := (0, 0);
      Test_Bool           : Boolean;
      CDW11_Temp          : Unsigned_32;
      Temp_CQE            : CompletionQ.CQE;
      Temp_NSID           : Unsigned_32;
      Is_NVM_CMD_Set      : Boolean := False;
      Is_Old_NVME_Version : Boolean := False;

      NVMe_Status         : Status_Type;

   begin
      Success := False;

      Log.Put_Line ("Storage_Drv: NVMe Controller Init Start");

      ---------------------------------------------------
      --- 0. Resetting the Controller
      ---------------------------------------------------
      declare
         Temp_CC : CC_Part := CProp.CC;
      begin
         Temp_CC.EN := True;
         CProp.CC   := Temp_CC;
      end;

      declare
         Temp_CC : CC_Part := CProp.CC;
      begin
         Temp_CC.EN := False;
         CProp.CC   := Temp_CC;
      end;

      ---------------------------------------------------
      -- 1. Waiting for Controller to indicate that it's ready.
      ---------------------------------------------------

      declare
         Temp_V : constant VS_Part := CProp.VS;
      begin
         Log.Put_Line ("NVMe Controller Version is " &
            SK.Strings.Img (Temp_V.Major) & "." &
            SK.Strings.Img (Temp_V.Minor) & "." &
            SK.Strings.Img (Temp_V.Tert));

            if Temp_V.Major < 2 then
               Is_Old_NVME_Version := True;
               Log.Put_Line ("NVMe 1.x detected - using legacy initialization path");
            else
               Log.Put_Line ("");
               Log.Put_Line ("---------------------------------------------------");
               Log.Put_Line ("WARNING: NVMe version >= 2.0");
               Log.Put_Line ("         Untested Controller Init");
               Log.Put_Line ("---------------------------------------------------");
               Log.Put_Line ("");

            end if;
      end;

      Wait_For_Ready (Should_Be => False);

      ---------------------------------------------------
      --- 2. Configuring Admin Queue Attributes, if needed
      ---------------------------------------------------
         -- Default Values of Qemu are 255; 63 (Zeros based => 256, 64)
      declare
         Temp_AQA : AQA_Part := CProp.AQA;
      begin
         Temp_AQA.ACQS := 255;
         Temp_AQA.ASQS := 63;
         CProp.AQA := Temp_AQA;
      end;
        -- Configure Admin Queue Base Addresses
        -- divided the given memory in half - change if needed
      CProp.ASQ := ASQ_Address;
      CProp.ACQ := ACQ_Address;

      ---------------------------------------------------
      --- 3. Checking supported I/O Cmd Sets
      ---------------------------------------------------
      declare
         Temp : constant Unsigned_8 := CProp.CAP.CSS;
      begin
         CSS_BitArray := uInt8ToBitArray (Temp);
      end;
      declare
         Temp_CC : CC_Part := CProp.CC;
         Temp_CAP_AMS : constant Unsigned_2 := CProp.CAP.AMS;
      begin
         if Is_Old_NVME_Version then
            -- NVMe 1.4: Only NVM Command Set (CSS bit 0)
            if CSS_BitArray (0) then
               Temp_CC.CSS := 0;
            else
               Log.Put_Line ("NVME: Error - NVM Command Set not supported");
               return;
            end if;
         else
            -- NVMe 2.0: Support for multiple command sets
            if CSS_BitArray (7) then
               Temp_CC.CSS := 7;
            elsif CSS_BitArray (6) then
               Temp_CC.CSS := 6;
            elsif CSS_BitArray (0) then
               Temp_CC.CSS := 0;
            end if;
         end if;

      ---------------------------------------------------
      --- 4. Configuring Controller Settings
      ---------------------------------------------------
      -- 4.1. Arbitration Mechanism
      ------------------------------------
         -- Round Robin is standard AM
         Temp_CC.AMS := 0;
         -- Check if Weighted Round Robin is supported
         if Temp_CAP_AMS = 1
         then
            Temp_CC.AMS := 1;
         end if;
         -- Check if Vendor Specific AM is available
         if Temp_CAP_AMS = 2
         then
            Log.Put_Line ("Vendor Specific Arbitration Mechanism available!");
            Temp_CC.AMS := 7;
         end if;

      ---------------------------------------------------
      -- 4.2 Memory Page Size
      ---------------------------------------------------

      ---------------------------------------------------
      -- MPS is 2^(12+MPS) -> with 2 -> 16KB
      -- limits on qemu: min=0, max=4
      -- uInt 4
      -- change if needed:
      -- bigger  : for fewer, heavier loads
      -- smaller : for more, random and small loads

      -- Muenblock max MPS need should be 64k so MPS=4

      ---------------------------------------------------
         declare
            Temp_Max : constant Unsigned_4 := CProp.CAP.MPSMAX;
            Temp_Min : constant Unsigned_4 := CProp.CAP.MPSMIN;
         begin
            if Temp_Max < 4 then
               Temp_CC.MPS := Temp_Max;
            else
               if Temp_Min > 4 then
                  Temp_CC.MPS := Temp_Min;
               else
                  Temp_CC.MPS := 4;
               end if;
            end if;
         end;

         Memory_Page_Size := Unsigned_8 (Temp_CC.MPS);
         pragma Assert (Memory_Page_Size <= Unsigned_8 (Unsigned_4'Last));

         --this should come later but testing showed is required here already
         Temp_CC.IOCQES := 4; -- 16 Byte
         Temp_CC.IOSQES := 6; -- 64 Byte

      ---------------------------------------------------
      --- 5. Enabling the Controller
      ---------------------------------------------------

         Temp_CC.EN := True;

         CProp.CC := Temp_CC;
      end;

      ---------------------------------------------------
      --- 6. Waiting for the Controller
      ---------------------------------------------------

      Wait_For_Ready (Should_Be => True);

      Log.Put_Line ("Controller is now ready to process commands");

      ---------------------------------------------------
      --- 7. Identify Controller
      ---------------------------------------------------

      PRP_D_Ptr.E1 := Ident_Controller_Address;

      AdminCommandSet.CreateIndentify_Command
        (CMD_Identifier   => CMD_Identifier_Admin,
         DPTR             => PRP_D_Ptr,
         NSID             => 0,
         CNTID            => 0,
         CNS              => 1,
         CSI              => 0,
         CNSSpecificIdent => 0,
         UUID_Index       => 0,
         Command          => Admin_CMD);

      ProcessAdminCommand (Admin_CMD, NVMe_Status);

      if NVMe_Status /= OK then
         Log.Put_Line ("NVME: Error During NVMe Controller Init Step 7");
         return;
      end if;

      declare
         VID : constant Unsigned_16 := IdentController.VID;
      begin
         Log.Put_Line ("VID :" & SK.Strings.Img (VID));
      end;

      ---------------------------------------------------
      --- 8. Determining IO CMD Set
      ---------------------------------------------------

      if Is_Old_NVME_Version then
         ---------------------------------------------------
         -- NVMe 1.4 Path: Assume NVM Command Set
         ---------------------------------------------------
         Log.Put_Line ("NVMe 1.4: Skipping I/O Command Set configuration");

         IO_CMD_Set_Index := 0;

         ------------------------------------------------------------
         -- 8.b.i (1.4) Identify active Namespace ID List (CNS=02h)
         ------------------------------------------------------------
         PRP_D_Ptr.E1 := Namespace_List_Address;

         AdminCommandSet.CreateIndentify_Command
           (CMD_Identifier   => CMD_Identifier_Admin,
            DPTR             => PRP_D_Ptr,
            NSID             => 0,
            CNTID            => 0,
            CNS              => 16#02#,  -- Active NSID list in NVMe 1.4
            CSI              => 0,
            CNSSpecificIdent => 0,
            UUID_Index       => 0,
            Command          => Admin_CMD);

         ProcessAdminCommand (Admin_CMD, NVMe_Status);
         if NVMe_Status /= OK then
            Log.Put_Line ("NVME: Error During NVMe 1.4 Controller Init Step 8.b.i");
            return;
         end if;

         ------------------------------------
         -- 8.b.ii (1.4) Process Namespaces
         ------------------------------------
         for Namespace_ID_List_Index in Namespace_ID_List_Index_Type'Range loop
            pragma Loop_Invariant (MB_Sector_Size in 512 | 4096);

            Temp_NSID := Namespace_List (Namespace_ID_List_Index);
            if Temp_NSID = 0 then
               if Namespace_ID_List_Index = 0 then
                  Log.Put_Line ("NVME: No active namespaces found");
                  return;
               end if;
               exit;
            end if;

            Log.Put_Line ("NSID " & SK.Strings.Img_Dec (Unsigned_64 (Namespace_ID_List_Index)) & " is " & SK.Strings.Img_Dec (Unsigned_64 (Temp_NSID)));

            PRP_D_Ptr.E1 := Ident_Namespace_Address;

            AdminCommandSet.CreateIndentify_Command
              (CMD_Identifier   => CMD_Identifier_Admin,
               DPTR             => PRP_D_Ptr,
               NSID             => Temp_NSID,
               CNTID            => 0,
               CNS              => 0,
               CSI              => 0,
               CNSSpecificIdent => 0,
               UUID_Index       => 0,
               Command          => Admin_CMD);

            ProcessAdminCommand (Admin_CMD, NVMe_Status);
            if NVMe_Status /= OK then
               Log.Put_Line ("NVME: Error During NVMe 1.4 Controller Init Step 8.b.ii");
               return;
            end if;

            declare
               Temp_NS   : constant IdentifyNamespace := IdentNamespace;
               Temp_NSZE : Unsigned_64;
            begin
               Temp_NSZE := Temp_NS.NSZE;
               Log.Put_Line ("Namespace Size is " & SK.Strings.Img_Dec (Temp_NSZE) & " logical blocks.");
               if Temp_NS.LBA_List (0).LBADS not in 9 | 12 then
                  Log.Put_Line ("NVME: Invalid Namespace LBA Size.");
                  return;
               end if;
               Set_Muenblock_Constants (Temp_NS);
            end;
         end loop;

      else
         ---------------------------------------------------
         -- NVMe 2.0 Path: Full I/O Command Set Support
         ---------------------------------------------------

         if CSS_BitArray (7) then
            Log.Put_Line ("NVME: Error During NVMe Controller Init Step 8 - Unexpected CSS Bit");
            return;
         end if;

         ------------------------------------
         -- 8.a.i Identify IO CMD Set CNS 1Ch
         ------------------------------------
         PRP_D_Ptr.E1 := IO_CMD_Sets_Address;
         AdminCommandSet.CreateIndentify_Command
           (CMD_Identifier   => CMD_Identifier_Admin,
            DPTR             => PRP_D_Ptr,
            NSID             => 0,
            CNTID            => 16#FFFF#, -- Controller which processes this
            CNS              => 16#1C#,
            CSI              => 0,
            CNSSpecificIdent => 0,
            UUID_Index       => 0,
            Command          => Admin_CMD);

         ProcessAdminCommand (Admin_CMD, NVMe_Status);
         if NVMe_Status /= OK then
            Log.Put_Line ("NVME: Error During NVMe Controller Init Step 8.a.i");
            return;
         end if;

         for IO_CMD_Set_Counter in IO_CMD_Set_Array'Range loop
            Test_Bool := IO_CMD_Sets (IO_CMD_Set_Counter).NVM_CMD_Set;
            if not Test_Bool then
               Test_Bool := IO_CMD_Sets (IO_CMD_Set_Counter).Key_Value_CMD_Set;
               if not Test_Bool then
                  Test_Bool := IO_CMD_Sets (IO_CMD_Set_Counter).Zoned_Namespace_CMD_Set;
                     if not Test_Bool then
                        if IO_CMD_Set_Counter = 0 then
                           Log.Put_Line ("NVME: Warning: No CMD Set found");
                        else
                           IO_CMD_Set_Index := IO_CMD_Set_Counter - 1;
                        end if;
                        exit;
                     end if;
               end if;
            end if;
         end loop;

         Log.Put_Line ("Found IO CMD Set Index" & SK.Strings.Img_Dec (Unsigned_64 (IO_CMD_Set_Index)));

         -----------------------------------------------
         -- 8.a.ii Set Features FID 19h for cmdset index
         -----------------------------------------------
         CDW11_Temp := IOCMDIndexToCDW11 ((IOCSCI => IO_CMD_Set_Index, others => <>));
         PRP_D_Ptr.E1 := 0;
         AdminCommandSet.CreateSetFeatures_Command
           (CMD_Identifier => CMD_Identifier_Admin,
            DPTR           => PRP_D_Ptr,
            FID            => 16#19#,
            SV             => False,
            UUID_Index     => 0,
            CDW11_Cvt      => CDW11_Temp,
            Command        => Admin_CMD);

         ProcessAdminCommand (Admin_CMD, NVMe_Status);
         if NVMe_Status /= OK then
            Log.Put_Line ("NVME: Error During NVMe Controller Init Step 8.a.ii");
            return;
         end if;
         Temp_CQE := ACQ (ACQ_Index - 1);
         Log.Put_Line ("Index of IO CMD Set Combination is now: " & SK.Strings.Img_Dec (Unsigned_64 (Temp_CQE.DWORD0)));

         ------------------------------------------------------------
         -- 8.b.i Identify active Namespace ID List for every CMD Set
         ------------------------------------------------------------
         PRP_D_Ptr.E1 := Namespace_List_Address;

         for IO_CMD_Set_Iterator in  0 .. IO_CMD_Set_Index loop
            pragma Loop_Invariant (MB_Sector_Size in 512 | 4096);

            Test_Bool := IO_CMD_Sets (IO_CMD_Set_Iterator).NVM_CMD_Set;
            if Test_Bool then
               -- CSI 0 for NVM-CMD-Set
               AdminCommandSet.CreateIndentify_Command
                 (CMD_Identifier   => CMD_Identifier_Admin,
                  DPTR             => PRP_D_Ptr,
                  NSID             => 0,
                  CNTID            => 0,
                  CNS              => 7,
                  CSI              => 0,
                  CNSSpecificIdent => 0,
                  UUID_Index       => 0,
                  Command          => Admin_CMD);
               Is_NVM_CMD_Set := True;

            else
               Test_Bool := IO_CMD_Sets (IO_CMD_Set_Iterator).Key_Value_CMD_Set;
               if Test_Bool then
                  -- CSI 1 for Key-Value-CMD-Set
                  AdminCommandSet.CreateIndentify_Command
                    (CMD_Identifier   => CMD_Identifier_Admin,
                     DPTR             => PRP_D_Ptr,
                     NSID             => 0,
                     CNTID            => 0,
                     CNS              => 7,
                     CSI              => 1,
                     CNSSpecificIdent => 0,
                     UUID_Index       => 0,
                     Command          => Admin_CMD);

               else
                  Test_Bool := IO_CMD_Sets (IO_CMD_Set_Iterator).Zoned_Namespace_CMD_Set;
                  if Test_Bool then
                     -- CSI 2 for Zoned-Namespace-CMD-Set
                     AdminCommandSet.CreateIndentify_Command
                       (CMD_Identifier   => CMD_Identifier_Admin,
                        DPTR             => PRP_D_Ptr,
                        NSID             => 0,
                        CNTID            => 0,
                        CNS              => 7,
                        CSI              => 2,
                        CNSSpecificIdent => 0,
                        UUID_Index       => 0,
                        Command          => Admin_CMD);
                     Is_NVM_CMD_Set := True;

                  else
                     Log.Put_Line ("Undefined behavior while trying to identify CMD Set type");
                     return;
                  end if;
               end if;
            end if;
            Log.Put_Line ("Identifying CMD Set at Index " & SK.Strings.Img_Dec (Unsigned_64 (IO_CMD_Set_Iterator)) & " of " & SK.Strings.Img_Dec (Unsigned_64 (IO_CMD_Set_Index)));
            ProcessAdminCommand (Admin_CMD, NVMe_Status);
            if NVMe_Status /= OK then
               Log.Put_Line ("NVME: Error During NVMe Controller Init Step 8.b.i");
               return;
            end if;

            -- currently only 1 NSID: "1"
            ------------------------------------
            -- 8.b.ii
            ------------------------------------
            for Namespace_ID_List_Index in Namespace_ID_List_Index_Type'Range loop
               pragma Loop_Invariant (MB_Sector_Size in 512 | 4096);

               Temp_NSID := Namespace_List (Namespace_ID_List_Index);
               if Temp_NSID = 0 then
                  if Namespace_ID_List_Index = 0 then
                     return;
                  end if;
                  exit;
               end if;

               Log.Put_Line ("NSID " & SK.Strings.Img_Dec (Unsigned_64 (Namespace_ID_List_Index)) & " is " & SK.Strings.Img_Dec (Unsigned_64 (Temp_NSID)));
               ---------------------------------------------
               -- 8.b.ii.1 only for NVM or Zoned NS CMD Sets
               ---------------------------------------------
               if Is_NVM_CMD_Set then
                  PRP_D_Ptr.E1 := Ident_Namespace_Address;
                  AdminCommandSet.CreateIndentify_Command
                    (CMD_Identifier   => CMD_Identifier_Admin,
                     DPTR             => PRP_D_Ptr,
                     NSID             => Temp_NSID,
                     CNTID            => 0,
                     CNS              => 0,
                     CSI              => 0,
                     CNSSpecificIdent => 0,
                     UUID_Index       => 0,
                     Command          => Admin_CMD);
                  ProcessAdminCommand (Admin_CMD, NVMe_Status);
                  if NVMe_Status /= OK then
                     Log.Put_Line ("NVME: Error During NVMe Controller Init Step 8.b.ii");
                     return;
                  end if;

                  declare
                     Temp_NS   : constant IdentifyNamespace := IdentNamespace;
                     Temp_NSZE : Unsigned_64;
                  begin
                     Temp_NSZE := Temp_NS.NSZE;
                     Log.Put_Line ("Namespace Size is " & SK.Strings.Img_Dec (Temp_NSZE) & " logical blocks.");
                     if Temp_NS.LBA_List (0).LBADS not in 9 | 12 then
                        Log.Put_Line ("NVME: Invalid Namespace LBA Size.");
                        return;
                     end if;
                     Set_Muenblock_Constants (Temp_NS);
                  end;
               else
                  -- Temporary until more needed
                  return;
               end if;
               ------------------------------------
               -- 8.b.ii.2 Identify 05h, 06h, 08h
               ------------------------------------
                  --is this really necessary???

            end loop;
         end loop;
      end if;

      pragma Assert (Check_Sector_Size);

      ---------------------------------------------------
      --- 9. Set Feature Number of I/O Queues
      ---------------------------------------------------

      PRP_D_Ptr.E1 := 0;
      CDW11_Temp := NumOfQsToCDW11 ((NSQR => 1, NCQR => 1));
      AdminCommandSet.CreateSetFeatures_Command
        (CMD_Identifier => CMD_Identifier_Admin,
         DPTR           => PRP_D_Ptr,
         FID            => 16#07#,
         SV             => False,
         UUID_Index     => 0,
         CDW11_Cvt      => CDW11_Temp,
         Command        => Admin_CMD);
      ProcessAdminCommand (Admin_CMD, NVMe_Status);
      if NVMe_Status /= OK then
         Log.Put_Line ("NVME: Error During NVMe Controller Init Step 9");
         return;
      end if;

      ---------------------------------------------------
      --- 10. Initialize I/O Completion Queues
      ---------------------------------------------------

         -- value in bytes, 2^n
      declare
         Temp_CC : CC_Part := CProp.CC;
      begin
         Temp_CC.IOCQES := 4; -- 16 Byte
         Temp_CC.IOSQES := 6; -- 64 Byte
         CProp.CC := Temp_CC;
      end;

      PRP_D_Ptr.E1 := IOCQ_Address;
      AdminCommandSet.CreateCreateIOCQ_Command
        (CMD_Identifier   => CMD_Identifier_Admin,
         DPTR             => PRP_D_Ptr,
         QID              => 1,
         QSIZE            => 255,
         PC               => True,
         IEN              => False,
         Command          => Admin_CMD);
      ProcessAdminCommand (Admin_CMD, NVMe_Status);
      if NVMe_Status /= OK then
         Log.Put_Line ("NVME: Error During NVMe Controller Init Step 10");
         return;
      end if;

      ---------------------------------------------------
      --- 11. Initialize I/O Submission Queues
      ---------------------------------------------------

      PRP_D_Ptr.E1 := IOSQ_Address;
      AdminCommandSet.CreateCreateIOSQ_Command
        (CMD_Identifier   => CMD_Identifier_Admin,
         DPTR             => PRP_D_Ptr,
         QID              => 1,
         QSIZE            => 63,
         PC               => True,
         QPRIO            => 0,
         CQID             => 1,
         Command          => Admin_CMD);
      ProcessAdminCommand (Admin_CMD, NVMe_Status);
      if NVMe_Status /= OK then
         Log.Put_Line ("NVME: Error During NVMe Controller Init Step 11");
         return;
      end if;

      Success := True;
      Log.Put_Line ("NVME: Finished Controller Init");

   end ControllerInit;

   ---------------------------------------------------
   --- 3.6 Controller Shutdown
   ---------------------------------------------------

   procedure ControllerShutdown
   is
      Admin_CMD   : SubmissionQ.Admin_Command;
      NVMe_Status : Status_Type;
   begin

   ----------------------------
   --- 1. If Controller enabled
   ----------------------------
      declare
         Enabled : constant Boolean := CProp.CC.EN;

      begin
         if Enabled then
            AdminCommandSet.CreateDeleteIOSQ_Command
               (CMD_Identifier => CMD_Identifier_Admin,
                QID            => 1,
                Command        => Admin_CMD);
            ProcessAdminCommand (Admin_CMD, NVMe_Status);
            if NVMe_Status /= OK then
               Log.Put_Line ("NVME: Error During NVMe Controller Shutdown Step 1-1");
            end if;

            AdminCommandSet.CreateDeleteIOCQ_Command
               (CMD_Identifier => CMD_Identifier_Admin,
                QID            => 1,
                Command        => Admin_CMD);
            ProcessAdminCommand (Admin_CMD, NVMe_Status);
            if NVMe_Status /= OK then
               Log.Put_Line ("NVME: Error During NVMe Controller Shutdown Step 1-2");
            end if;
         end if;
      end;

      ----------------------------
      --- 2. Shutdown Notification
      ----------------------------

      declare
         Temp_CC : CC_Part := CProp.CC;
      begin
         Temp_CC.SHN := 1;
         CProp.CC    := Temp_CC;
      end;

      loop
         declare
            Temp_SHST : constant Unsigned_2 := CProp.CSTS.SHST;
            Temp_ST   : constant Boolean    := CProp.CSTS.ST;
         begin
            pragma Warnings (GNATprove, Off, "statement has no effect");
            exit when Temp_SHST = 2 and not Temp_ST;
            pragma Warnings (GNATprove, On, "statement has no effect");
         end;
      end loop;

      Log.Put_Line ("Controller Shutdown Complete.");

   end ControllerShutdown;

   --------------------------------------------------------------------------

   -- Number of logical blocks * sector size
   function Get_Size return Unsigned_64
      -- maybe use NVMCAP (check the documentation note for it first)
   is (MB_Size);

   -- Sector = Logical Block
   function Get_Sector_Cnt return Unsigned_64
   is (MB_Sector_Count);

   function Get_Max_Sector_Cnt return Unsigned_64
   is (MB_Max_Sector_Count);

   function Get_Sector_Size return Unsigned_32
   is (MB_Sector_Size);

end NVMe.Host;
