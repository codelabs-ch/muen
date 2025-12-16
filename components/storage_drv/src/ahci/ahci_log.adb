with Interfaces;
with System;
with SK.Strings;

with Ahci.Ports; use Ahci.Ports;
with Pciconf;
with Log; use Log;
with Ahci; use Ahci;

with Storage_Drv_Cspecs_Wrapper;

package body Ahci_Log
with
   SPARK_Mode => Off
is
   use type Storage_Interface.PConf.Port_Range;

   type Cmd_Table_Buf_Type
   is array (Integer range 0 .. Integer ((PConf.Port_Range'Last + 1) * 16#40#))
      of Unsigned_32
   with Pack;

   Cmd_Table_Buf : Cmd_Table_Buf_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Storage_Interface.Command_Table_Address);

   procedure Dump_Cmd_Table (
      ID  : PConf.Port_Range;
      Len : Integer)
   is
      use type Interfaces.Unsigned_32;
      Local32       : Unsigned_32;
      Start_Index   : constant Integer := Integer (ID) * 16#40#;
   begin
      for I in Integer range Start_Index .. Start_Index + Len loop
         Local32 := Cmd_Table_Buf (I);
         Put_Line
           ("Cmd Table ["
            & SK.Strings.Img (Unsigned_8 (I))
            & "] "
            & SK.Strings.Img (Local32));
         Local32 := Local32 + 1;
      end loop;
   end Dump_Cmd_Table;

   -------------------------------------------------------------------------

   type Cmd_List_Buf_Type
   is array (Integer range 0 ..
               Integer ((PConf.Port_Range'Last + 1) * 16#100#))
      of Unsigned_32
   with Size => 32 * (1 + Integer (PConf.Port_Range'Last + 1) * 16#100#);

   Cmd_List_Buf : Cmd_List_Buf_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Storage_Interface.Command_Lists_Address);

   procedure Dump_Cmd_List
     (ID  : PConf.Port_Range;
      Len : Integer)
   is
      use type Interfaces. Unsigned_32;
      Local32      : Unsigned_32;
      Start_Index  : constant Integer := Integer (ID) * 16#100#;
   begin
      for I in Integer range Start_Index .. Start_Index + Len loop
         Local32 := Cmd_List_Buf (I);
         Put_Line
           ("Cmd List ["
            & SK.Strings.Img (Unsigned_8 (I))
            & "] "
            & SK.Strings.Img (Local32));
         Local32 := Local32 + 1;
      end loop;
   end Dump_Cmd_List;

   -------------------------------------------------------------------------

   type Port_Regs_Array
      is array (Integer range 0 ..
                  Integer (PConf.Port_Range'Last) * 16#20# + 17)
      of Unsigned_32;

   Port_Regs : Port_Regs_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address
        (Storage_Drv_Cspecs_Wrapper.Devices.Controller_Ahci_Registers_Address
            + 16#100#);

   procedure Dump_Port_Regs (ID  : PConf.Port_Range)
   is
      use type Interfaces.Unsigned_32;
      Local32      : Unsigned_32;
      Start_Index  : constant Integer := Integer (ID) * 16#20#;
   begin
      Put_Line ("Dumping Port  " & SK.Strings.Img (
                Unsigned_32 (ID)));
      for I in Integer range Start_Index .. 17 + Start_Index loop
         Local32 := Port_Regs (I);
         Put_Line (SK.Strings.Img (Local32));
         Local32 := Local32 + 1;
      end loop;
   end Dump_Port_Regs;

      -------------------------------------------------------------------------

   procedure Print_Port_Error (ID : PConf.Port_Range)
   is
      use type Interfaces.Unsigned_16;

      Sata_Error  : constant Port_SATA_Error_Type
        := Ahci.Ports.Instance (ID).SATA_Error;
      Intr_Status : constant Port_Interrupt_Status_Type
        := Ahci.Ports.Instance (ID).Interrupt_Status;
      T_F_Status  : constant Port_Task_File_Data_Type
        := Ahci.Ports.Instance (ID).Task_File_Data;
   begin
      if Intr_Status.OFS then
         Put_Line ("err: Overflow");
      end if;
      if Intr_Status.INFS then
         Put_Line ("err: Interface Non-Fatal Error");
      end if;
      if Intr_Status.IFS then
         Put_Line ("err: Interface Fatal Error");
      end if;
      if Intr_Status.HBDS then
         Put_Line ("err: Host Bus Data Error");
      end if;
      if Intr_Status.OFS then
         Put_Line ("err: Host Bus Fatal Error");
      end if;
      if Intr_Status.TFES then
         Put_Line ("err: Task File Error");
      end if;
      if Intr_Status.TFES then
         Put_Line ("TF Err: " & SK.Strings.Img (T_F_Status.ERR));
      end if;
      if Intr_Status.PCS then
         Put_Line ("err: Port Connect Change Status");
      end if;
      if Sata_Error.ERR /= 0 then
         Put_Line ("err: Sata Err: " & SK.Strings.Img (Sata_Error.ERR));
      end if;
      if Sata_Error.DIAG /= 0 then
         Put_Line ("err: Sata DIAG: " & SK.Strings.Img (Sata_Error.DIAG));
      end if;
   end Print_Port_Error;

   -------------------------------------------------------------------------

   procedure Print_PCI_Capabilities
   is
      use Pciconf;
      use type Storage_Interface.Unsigned_8;

      Cap_ID : Unsigned_8;
      Index  : Unsigned_8 := Pciconf.Instance.Header.Capabilities_Pointer;
   begin
      loop
         exit when Index = 0 or not (Index in Pciconf.Capability_Range);
         Cap_ID := Pciconf.Instance.Capabilities (Index);
         Put_Line (Item => " Capability : " & SK.Strings.Img (Cap_ID) & " @ "
                   & SK.Strings.Img (Index));
         Index := Pciconf.Instance.Capabilities (Index + 1);
      end loop;
   end Print_PCI_Capabilities;

end Ahci_Log;