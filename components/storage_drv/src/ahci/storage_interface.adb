with Ahci.Constants;
with Ahci.Device;
with Ahci_Log;
with Log;
with Server;
with Pciconf;
with SK.CPU;

package body Storage_Interface
is

   procedure Startup
   is
   begin
      Log.Init (Epoch => 1);

      Log.Put_Line (Item => "Storage_Drv: AHCI driver subject running");
      Log.Print_PCI_Device_Info;
      Ahci_Log.Print_PCI_Capabilities;

      if not Musinfo.Instance.Is_Valid then
         Log.Put_Line ("Sinfo invalid! Halting vCPU");
         SK.CPU.Stop;
      end if;

      --  enable PCI Bus master, memory and io space. This might habe been already
      --  done by BIOS but coreboot usually leaves BM disabled.
      declare
         use type Unsigned_16;

         Pci_Cmd : constant Unsigned_16
            := Pciconf.Instance.Header.Command;
      begin
         if (Pci_Cmd and 16#4#) /= 16#4# then
            Pciconf.Instance.Header.Command := Pci_Cmd or 16#4#;
         end if;
      end;

      declare

         Info : constant Pciconf.Info_Record := Pciconf.Instance.Header.Info;

         Success : Boolean;

      begin
         if Info.Class_Code = Ahci.Constants.AHCI_Class_Code then
            Log.Put_Line (Item => "AHCI controller present");

            Server.Init (Success);
            if Success then
               Server.Process;
            end if;

            Log.Put_Line ("Server exited. Should not happen.");
         else
            Log.Put_Line (Item => "AHCI controller not present");
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

      Ahci.Device.Init;
      Ahci.Device.Get_Attached_Devices (Devs);
      Success := True;
   end Init;

   -------------------------------------------------------------------------

   function Get_Sector_Size (Dev_Id : PConf.Port_Range) return Unsigned_32
   is (Ahci.Device.Get_Sector_Size (Dev_Id));

   function Get_Sector_Cnt (Dev_Id : PConf.Port_Range) return Unsigned_64
   is (Ahci.Device.Get_Sector_Cnt (Dev_Id));

   function Get_Max_Sector_Cnt (Dev_Id : PConf.Port_Range) return Unsigned_64
   is (Unsigned_64 (Ahci.Device.Get_Max_Sector_Count (Dev_Id)));

   function Get_Size (Dev_Id : PConf.Port_Range) return Unsigned_64
   is (Ahci.Device.Get_Size (Dev_Id));

   -------------------------------------------------------------------------

   procedure Execute_Read_Command
      (Address :     Unsigned_64;
       SLBA    :     Unsigned_64;
       NLB     :     Unsigned_32;
       Dev_Id  :     PConf.Port_Range;
       Status  : out Status_Type)
   is
      use type Unsigned_32;
   begin
      Ahci.Device.RW_Sectors
         (ID      => Dev_Id,
          RW      => Ahci.Read,
          Start   => SLBA,
          Count   => NLB + 1,
          Address => Address,
          Ret_Val => Status);

   end Execute_Read_Command;

   -------------------------------------------------------------------------

   procedure Execute_Write_Command
      (Address :     Unsigned_64;
       SLBA    :     Unsigned_64;
       NLB     :     Unsigned_32;
       Dev_Id  :     PConf.Port_Range;
       Status  : out Status_Type)
   is
      use type Unsigned_32;
   begin
      Ahci.Device.RW_Sectors
         (ID      => Dev_Id,
          RW      => Ahci.Write,
          Start   => SLBA,
          Count   => NLB + 1,
          Address => Address,
          Ret_Val => Status);

   end Execute_Write_Command;

   -------------------------------------------------------------------------

   procedure Execute_Discard_Command
      (SLBA    :     Unsigned_64;
       NLB     :     Unsigned_32;
       Dev_Id  :     PConf.Port_Range;
       Status  : out Status_Type)
   is
      use type Unsigned_32;
   begin
      Ahci.Device.Discard_Sectors
         (ID      => Dev_Id,
          Start   => SLBA,
          Count   => NLB + 1,
          Ret_Val => Status);

   end Execute_Discard_Command;

   -------------------------------------------------------------------------

   procedure Check_SMART_Status (Address :     Unsigned_64;
                                 Dev_Id  :     PConf.Port_Range;
                                 Status  : out Unsigned_64)
   is
      SMART_Status : Ahci.Device.SMART_Status_Type;
      Ret : Status_Type;
   begin
      Ahci.Device.Get_SMART
         (ID      => Dev_Id,
          Address => Address,
          Status  => SMART_Status,
          Ret_Val => Ret);
      if Ret  = OK then
         case SMART_Status is
            when Ahci.Device.OK =>
               Status := MB.SMART_OK;
            when Ahci.Device.Threshold_Exceeded =>
               Status := MB.SMART_THRESHOLD_EXCEEDED;
            when Ahci.Device.Undefined =>
               Status := MB.SMART_UNDEFINED;
         end case;
      else
         Status := 0;
      end if;
   end Check_SMART_Status;

   -------------------------------------------------------------------------

   procedure Sync (Dev_Id :     PConf.Port_Range;
                   Status : out Unsigned_64)
   is
      Ret : Status_Type;
   begin
      Ahci.Device.Sync
         (ID      => Dev_Id,
          Ret_Val => Ret);

      if Ret = OK then
         Status := 0;
      else
         Status := 1;
      end if;
   end Sync;

   -------------------------------------------------------------------------

   function Is_Valid return Boolean
   is (Musinfo.Instance.Is_Valid);

   function NVME_Check_Sector_Size return Boolean
   is (True);

end Storage_Interface;
