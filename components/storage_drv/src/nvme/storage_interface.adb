with SK.CPU;

with NVMe.SubmissionQ;
with NVMe.IOCommandSet;
with NVMe.Host;

with Log;
with Server;
with Pciconf;

package body Storage_Interface
is

   use type Unsigned_32;
   use type Unsigned_64;

   -------------------------------------------------------------------------

   pragma Warnings (Off, "Dev_Id");
   function Dummy_Use_Return (Dev_Id : PConf.Port_Range) return Unsigned_32
   is
   begin
      return 0;
   end Dummy_Use_Return;
   pragma Warnings (On, "Dev_Id");

   pragma Warnings (GNATprove, Off, "subprogram ""Dummy_Use"" has no effect");
   procedure Dummy_Use (Dev_Id : PConf.Port_Range)
   is
   begin
      null;
   end Dummy_Use;
   pragma Warnings (GNATprove, On, "subprogram ""Dummy_Use"" has no effect");

   -------------------------------------------------------------------------

   procedure Startup
   is
      use type Unsigned_16;
   begin
      Log.Init (Epoch => 1);

      Log.Put_Line (Item => "Storage_Drv: NVMe driver subject running");
      Log.Print_PCI_Device_Info;

      if not Musinfo.Instance.Is_Valid then
         Log.Put_Line ("Sinfo invalid! Halting vCPU");
         SK.CPU.Stop;
      end if;

      --  enable PCI Bus master, memory and io space. This might habe been already
      --  done by BIOS but coreboot usually leaves BM disabled.
      declare
         Pci_Cmd : constant Unsigned_16
            := Pciconf.Instance.Header.Command;
      begin
         if (Pci_Cmd and 16#4#) /= 16#4# then
            Pciconf.Instance.Header.Command := Pci_Cmd or 16#4#;
         end if;
      end;

      if NVMe.Host.Get_Sector_Size /= 512 and NVMe.Host.Get_Sector_Size /= 4096 then
         Log.Put_Line ("NVMe Sector Size invalid");
         return;
      end if;

      pragma Assert (NVME_Check_Sector_Size);

      declare

         Info : constant Pciconf.Info_Record := Pciconf.Instance.Header.Info;

         Success : Boolean;

      begin
         if Info.Class_Code = NVMe.NVMe_Class_Code then
            Log.Put_Line (Item => "NVMe controller present");

            Server.Init (Success);
            if Success then
               Server.Process;
            end if;

            Log.Put_Line ("Server exited. Should not happen.");
         else
            Log.Put_Line (Item => "NVMe controller not present");
         end if;
      end;
   end Startup;

   -------------------------------------------------------------------------

   procedure Init
      (Devs    : out Devs_Array;
       Ports   : out Ports_Array;
       Success : out Boolean)
   is
   begin
      Ports := (others =>
         (Chan_Idx         => 0,
          Devs             => (others => Internal_Device_Type'(
               Ahci_Port     => 0,
               Partition     => PConf.Null_Partition,
               Sector_Offset => 0,
               Sector_Count  => 0,
               Is_Valid      => False,
               Current       => Null_Current))));

      NVMe.Host.ControllerInit (Success);
      -- Testing IO
      Ports (1).Chan_Idx := PConf.Port_Config (1).Chan_Idx;
      Ports (1).Devs (0) := (Ahci_Port     => 0,
                             Partition     => PConf.No_Partition,
                             Is_Valid      => True,
                             Sector_Offset => 0,
                             Sector_Count  => NVMe.Host.Get_Sector_Cnt,
                             Current       => Null_Current);
      Ports (2).Chan_Idx := PConf.Port_Config (2).Chan_Idx;
      Ports (2).Devs (0) := (Ahci_Port     => 0,
                             Partition     => PConf.Smart_Only,
                             Is_Valid      => True,
                             Sector_Offset => 0,
                             Sector_Count  => NVMe.Host.Get_Sector_Cnt,
                             Current       => Null_Current);

      Devs := (others => False);
      Devs (0) := True;
   end Init;

   -------------------------------------------------------------------------

   function Get_Sector_Size (Dev_Id : PConf.Port_Range) return Unsigned_32
   is (NVMe.Host.Get_Sector_Size + Dummy_Use_Return (Dev_Id));

   function Get_Sector_Cnt (Dev_Id : PConf.Port_Range) return Unsigned_64
   is (NVMe.Host.Get_Sector_Cnt + Unsigned_64 (Dummy_Use_Return (Dev_Id)));

   function Get_Max_Sector_Cnt (Dev_Id : PConf.Port_Range) return Unsigned_64
   is (NVMe.Host.Get_Max_Sector_Cnt + Unsigned_64 (Dummy_Use_Return (Dev_Id)));

   function Get_Size (Dev_Id : PConf.Port_Range) return Unsigned_64
   is (NVMe.Host.Get_Size + Unsigned_64 (Dummy_Use_Return (Dev_Id)));

   -------------------------------------------------------------------------

   procedure Execute_Read_Command
      (Address :     Unsigned_64;
       SLBA    :     Unsigned_64;
       NLB     :     Unsigned_32;
       Dev_Id  :     PConf.Port_Range;
       Status  : out Status_Type)
   is
      PRP_Data_Ptr : NVMe.SubmissionQ.PRP_Data_Ptr := (0, 0);
      IO_CMD       : NVMe.SubmissionQ.IO_Command;
      NVMe_Status  : NVMe.Status_Type;
   begin
      Dummy_Use (Dev_Id);

      PRP_Data_Ptr.E1 := Address;

      NVMe.IOCommandSet.CreateRead_Command
         (CMD_Identifier  => NVMe.Host.CMD_Identifier_IO,
          DPTR            => PRP_Data_Ptr,
          SLBA            => SLBA,
          NLB             => Unsigned_16'Mod (NLB),
          Command         => IO_CMD);

      NVMe.Host.ProcessIOCommand (IOCmd => IO_CMD, Status => NVMe_Status);

      case NVMe_Status is
         when NVMe.OK =>                   Status := OK;
         when NVMe.Timeout | NVMe.Fail =>  Status := EIO;
         when NVMe.Unknown =>              Status := ENOTSUP;
      end case;

   end Execute_Read_Command;

   -------------------------------------------------------------------------

   procedure Execute_Write_Command
      (Address :     Unsigned_64;
       SLBA    :     Unsigned_64;
       NLB     :     Unsigned_32;
       Dev_Id  :     PConf.Port_Range;
       Status  : out Status_Type)
   is
      PRP_Data_Ptr : NVMe.SubmissionQ.PRP_Data_Ptr := (0, 0);
      IO_CMD       : NVMe.SubmissionQ.IO_Command;
      NVMe_Status  : NVMe.Status_Type;
   begin
      Dummy_Use (Dev_Id);

      PRP_Data_Ptr.E1 := Address;

      NVMe.IOCommandSet.CreateWrite_Command
         (CMD_Identifier  => NVMe.Host.CMD_Identifier_IO,
          DPTR            => PRP_Data_Ptr,
          SLBA            => SLBA,
          NLB             => Unsigned_16'Mod (NLB),
          Command         => IO_CMD);

      NVMe.Host.ProcessIOCommand (IOCmd => IO_CMD, Status => NVMe_Status);

      case NVMe_Status is
         when NVMe.OK =>                   Status := OK;
         when NVMe.Timeout | NVMe.Fail =>  Status := EIO;
         when NVMe.Unknown =>              Status := ENOTSUP;
      end case;

   end Execute_Write_Command;

   -------------------------------------------------------------------------

   procedure Execute_Discard_Command
      (SLBA    :     Unsigned_64;
       NLB     :     Unsigned_32;
       Dev_Id  :     PConf.Port_Range;
       Status  : out Status_Type)
   is
      IO_CMD      : NVMe.SubmissionQ.IO_Command;
      NVMe_Status : NVMe.Status_Type;
   begin
      Dummy_Use (Dev_Id);

      NVMe.IOCommandSet.CreateWrite_Zeroes_Command
         (CMD_Identifier  => NVMe.Host.CMD_Identifier_IO,
          SLBA            => SLBA,
          NLB             => Unsigned_16'Mod (NLB),
          Command         => IO_CMD);

      NVMe.Host.ProcessIOCommand (IOCmd => IO_CMD, Status => NVMe_Status);

      case NVMe_Status is
         when NVMe.OK =>                   Status := OK;
         when NVMe.Timeout | NVMe.Fail =>  Status := EIO;
         when NVMe.Unknown =>              Status := ENOTSUP;
      end case;

   end Execute_Discard_Command;

   -------------------------------------------------------------------------

   procedure Check_SMART_Status (Address :     Unsigned_64;
                                 Dev_Id  :     PConf.Port_Range;
                                 Status  : out Unsigned_64)
   is
      use type NVMe.Status_Type;
      SMART_Status : NVMe.Host.SMART_Status_Type;
      NVMe_Status  : NVMe.Status_Type;
   begin
      Dummy_Use (Dev_Id);

      NVMe.Host.GetSMART (Address, SMART_Status, NVMe_Status);
      if NVMe_Status = NVMe.OK then
         case SMART_Status is
            when NVMe.Host.OK =>
               Status := MB.SMART_OK;
            when NVMe.Host.Threshold_Exceeded =>
               Status := MB.SMART_THRESHOLD_EXCEEDED;
            when NVMe.Host.Undefined =>
               Status := MB.SMART_UNDEFINED;
         end case;
      else
         Status := 0;
      end if;
   end Check_SMART_Status;

   -------------------------------------------------------------------------

   procedure Sync (Dev_Id :      PConf.Port_Range;
                   Status : out  Unsigned_64)
   is
      use type NVMe.Status_Type;
      IO_CMD      : NVMe.SubmissionQ.IO_Command;
      NVMe_Status : NVMe.Status_Type;
   begin
      Dummy_Use (Dev_Id);

      -- TODO Check for IO SQ Tail Pointer / Head Pointer
      NVMe.IOCommandSet.CreateFlush_Command (NVMe.Host.CMD_Identifier_IO, IO_CMD);
      NVMe.Host.ProcessIOCommand (IO_CMD, NVMe_Status);

      if NVMe_Status = NVMe.OK then
         Status := 0;
      else
         Status := 1;
      end if;
   end Sync;

   -------------------------------------------------------------------------

   function Is_Valid return Boolean
   is (NVMe.Host.Is_Valid);

   function NVME_Check_Sector_Size return Boolean
   is (NVMe.Host.Check_Sector_Size);

end Storage_Interface;