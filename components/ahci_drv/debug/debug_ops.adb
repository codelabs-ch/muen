--
--  Copyright (C) 2014-2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with SK.Strings;

with Debuglog.Client;

with Ahci.Constants;
with Ahci.Pciconf;
with Ahci.Ports;
with Ahci.HBA;
with Ahci_Drv_Component.Devices;

package body Debug_Ops
with
   SPARK_Mode => Off
is

   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   use Ahci;

   --  Outputs 'yes' if Boolean is True, 'no' otherwise.
   procedure Put
     (Bool    : Boolean;
      Newline : Boolean := True);

   -------------------------------------------------------------------------

   procedure Init
     (Epoch : Interfaces.Unsigned_64)
      renames Debuglog.Client.Init;

   -------------------------------------------------------------------------

   procedure Put_Bit_Array (Item : Ahci.Bit_Array)
   is
   begin
      for I in reverse Item'Range loop
         if Item (I) then
            Put_String ("1");
         else
            Put_String ("0");
         end if;
      end loop;
      Put_Line ("");
   end Put_Bit_Array;

   -------------------------------------------------------------------------

   procedure Print_Ports_Info
   is
      PI  : constant Ahci.Bit_Array := HBA.Instance.Ports_Implemented;
      Sig : Interfaces.Unsigned_32;
   begin
      Put_Line (Item => "== Ports");
      for I in Port_Range loop
         if PI (Natural (I)) then
            Put_String (Item => " Port "
                        & SK.Strings.Img (Interfaces.Unsigned_8 (I)));
            Sig := Ports.Instance (I).Signature;
            if Sig = Ahci.Ports.SIG_ATA then
               Put_Line (Item => " : SATA drive");
            elsif Sig = Ahci.Ports.SIG_ATAPI then
               Put_Line (Item => " : ATAPI drive");
            elsif Sig = Ahci.Ports.SIG_SEMB then
               Put_Line (Item => " : Enclosure management bridge");
            elsif Sig = Ahci.Ports.SIG_PM then
               Put_Line (Item => " : Port multiplier");
            else
               Put_Line (Item => " : [no drive]");
            end if;
         end if;
      end loop;
   end Print_Ports_Info;

   -------------------------------------------------------------------------

   procedure Print_MBR_Partition_Table (Table : Mbr.Partition_Table_Type)
   is
   begin
      for I in Integer range 0 .. Table.Count - 1 loop
         pragma Debug (Debug_Ops.Put_Line ("Partition " &
            SK.Strings.Img (Interfaces.Unsigned_32 (I))));
         pragma Debug (Debug_Ops.Put_Line (" Start LBA : " &
            SK.Strings.Img (Table.Entries (I).Start_Lba)));
         pragma Debug (Debug_Ops.Put_Line (" Count     : " &
            SK.Strings.Img (Table.Entries (I).Sector_Cnt)));
         pragma Debug (Debug_Ops.Put_Line (" Type      : " &
            SK.Strings.Img (Table.Entries (I).Partition_Type)));
      end loop;
   end Print_MBR_Partition_Table;

   -------------------------------------------------------------------------

   type Cmd_Table_Buf_Type
   is array (Integer range 0 .. Integer ((Ahci.Port_Range'Last + 1) * 16#40#))
      of Interfaces.Unsigned_32;
   Cmd_Table_Buf : Cmd_Table_Buf_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Ahci.Command_Table_Address);

   procedure Dump_Cmd_Table (
      ID  : Ahci.Port_Range;
      Len : Integer)
   is
      Local32 : Interfaces.Unsigned_32;
      Start   : constant Integer := Integer (ID) * 16#40#;
   begin
      for I in Integer range Start .. Start + Len loop
         Local32 := Cmd_Table_Buf (I);
         pragma Debug (Debug_Ops.Put_Line ("Cmd Table ["
            & SK.Strings.Img (Interfaces.Unsigned_8 (I))
            & "] "
            & SK.Strings.Img (Local32)));
         Local32 := Local32 + 1;
      end loop;
   end Dump_Cmd_Table;

   -------------------------------------------------------------------------

   type Cmd_List_Buf_Type
   is array (Integer range 0 ..
               Integer ((Ahci.Port_Range'Last + 1) * 16#100#))
      of Interfaces.Unsigned_32;
   Cmd_List_Buf : Cmd_List_Buf_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Ahci.Command_Lists_Address);

   procedure Dump_Cmd_List
     (ID  : Ahci.Port_Range;
      Len : Integer)
   is
      Local32 : Interfaces.Unsigned_32;
      Start   : constant Integer := Integer (ID) * 16#100#;
   begin
      for I in Integer range Start .. Start + Len loop
         Local32 := Cmd_List_Buf (I);
         pragma Debug (Debug_Ops.Put_Line ("Cmd List ["
            & SK.Strings.Img (Interfaces.Unsigned_8 (I))
            & "] "
            & SK.Strings.Img (Local32)));
         Local32 := Local32 + 1;
      end loop;
   end Dump_Cmd_List;

   -------------------------------------------------------------------------

   type Port_Regs_Array
      is array (Integer range 0 ..
                  Integer (Ahci.Port_Range'Last) * 16#20# + 17)
      of Interfaces.Unsigned_32;
   Port_Regs : Port_Regs_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address
        (Ahci_Drv_Component.Devices.Ahci_Controller_Ahci_Registers_Address
            + 16#100#);

   procedure Dump_Port_Regs (ID  : Ahci.Port_Range)
   is
      Local32 : Interfaces.Unsigned_32;
      Start   : constant Integer := Integer (ID) * 16#20#;
   begin
      pragma Debug (Debug_Ops.Put_Line ("Dumping Port  " & SK.Strings.Img (
         Interfaces.Unsigned_32 (ID))));
      for I in Integer range Start .. 17 + Start loop
         Local32 := Port_Regs (I);
         pragma Debug (Debug_Ops.Put_Line (SK.Strings.Img (Local32)));
         Local32 := Local32 + 1;
      end loop;
   end Dump_Port_Regs;

   -------------------------------------------------------------------------

   procedure Print_Port_Error (ID : Ahci.Port_Range)
   is
      use Ahci.Ports;

      Sata_Error  : constant Port_SATA_Error_Type :=
                        Ahci.Ports.Instance (ID).SATA_Error;
      Intr_Status : constant Port_Interrupt_Status_Type :=
                        Ahci.Ports.Instance (ID).Interrupt_Status;
      T_F_Status  : constant Port_Task_File_Data_Type :=
                        Ahci.Ports.Instance (ID).Task_File_Data;
   begin
      pragma Debug (Intr_Status.OFS,
         Debug_Ops.Put_Line ("err: Overflow"));
      pragma Debug (Intr_Status.INFS,
         Debug_Ops.Put_Line ("err: Interface Non-Fatal Error"));
      pragma Debug (Intr_Status.IFS,
         Debug_Ops.Put_Line ("err: Interface Fata Error"));
      pragma Debug (Intr_Status.HBDS,
         Debug_Ops.Put_Line ("err: Host Bus Data Error"));
      pragma Debug (Intr_Status.OFS,
         Debug_Ops.Put_Line ("err: Host Bus Fatal Error"));
      pragma Debug (Intr_Status.TFES,
         Debug_Ops.Put_Line ("err: Task File Error"));
      pragma Debug (Intr_Status.TFES,
         Debug_Ops.Put_Line ("TF Err: " &
             SK.Strings.Img (T_F_Status.ERR)));
      pragma Debug (Intr_Status.PCS,
         Debug_Ops.Put_Line ("err: Port Connect Change Status"));
      pragma Debug (Sata_Error.ERR /= 0,
         Debug_Ops.Put_Line ("err: Sata Err: " &
            SK.Strings.Img (Sata_Error.ERR)));
      pragma Debug (Sata_Error.DIAG /= 0,
         Debug_Ops.Put_Line ("err: Sata DIAG: " &
            SK.Strings.Img (Sata_Error.DIAG)));
   end Print_Port_Error;

   -------------------------------------------------------------------------

   procedure Print_HBA_Memory_Regs
   is
      use Ahci.HBA;

      Dummy4  : Ahci.Unsigned_4;
      Dummy8  : Interfaces.Unsigned_8;
      Dummy16 : Interfaces.Unsigned_16;
      Dummy32 : Interfaces.Unsigned_32;
   begin
      Put_Line (Item => "== HBA Memory Registers");
      Dummy16 := Instance.Version.MJR;
      Put_String
        (Item => " Version                : " & SK.Strings.Img (Dummy16));
      Dummy16 := Instance.Version.MIN;
      Put_Line (Item => ":" & SK.Strings.Img (Dummy16));

      Put_String (Item => " Interface Speed        : Gen ");
      Dummy4 := Instance.Host_Capabilities.ISS;
      case Dummy4 is
         when Ahci.Constants.Interface_Speed_Gen_1 =>
            Put_Line (Item => "1 (1.5 Gbps)");
         when Ahci.Constants.Interface_Speed_Gen_2 =>
            Put_Line (Item => "2 (3 Gbps)");
         when Ahci.Constants.Interface_Speed_Gen_3 =>
            Put_Line (Item => "3 (6 Gbps)");
         when others =>
            Put_Line (Item => "unknown");
      end case;

      Dummy8 := Interfaces.Unsigned_8 (Instance.Host_Capabilities.NP) + 1;
      Put_Line (Item => " Number of ports        : "
                & SK.Strings.Img (Dummy8));
      Put_Line
        (Item => " Command slots          : " & SK.Strings.Img
           (SK.Byte (Instance.Host_Capabilities.NCS)));
      Put_String (Item => " Native Command Queuing : ");
      Put (Bool => Instance.Host_Capabilities.SNCQ);
      Put_String (Item => " 64-bit support         : ");
      Put (Bool => Instance.Host_Capabilities.S64A);
      Print_Ports_Info;
   end Print_HBA_Memory_Regs;

   -------------------------------------------------------------------------

   procedure Print_PCI_Capabilities
   is
      use Ahci.Pciconf;

      Cap_ID : Interfaces.Unsigned_8;
      Index  : Interfaces.Unsigned_8 := Instance.Header.Capabilities_Pointer;
   begin
      loop
         exit when Index = 0 or not (Index in Ahci.Pciconf.Capability_Range);
         Cap_ID := Instance.Capabilities (Index);
         Put_Line (Item => " Capability : " & SK.Strings.Img (Cap_ID) & " @ "
                   & SK.Strings.Img (Index));
         Index := Instance.Capabilities (Index + 1);
      end loop;
   end Print_PCI_Capabilities;

   -------------------------------------------------------------------------

   procedure Print_PCI_Device_Info
   is
      use Ahci.Pciconf;

      Dummy8  : Interfaces.Unsigned_8;
      Dummy16 : Interfaces.Unsigned_16;
      Dummy32 : Interfaces.Unsigned_32;
   begin
      Put_Line (Item => "== PCI config space");
      Dummy16 := Instance.Header.Vendor_ID;
      Put_Line (Item => " Vendor ID  : " & SK.Strings.Img (Dummy16));
      Dummy16 := Instance.Header.Device_ID;
      Put_Line (Item => " Device ID  : " & SK.Strings.Img (Dummy16));
      Dummy8 := Instance.Header.Revision_ID;
      Put_Line (Item => " Revision   : " & SK.Strings.Img (Dummy8));
      Dummy32 := Interfaces.Unsigned_32 (Instance.Header.Class_Code);
      Put_Line (Item => " Class      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Instance.Header.Base_Address_Register_5;
      Put_Line (Item => " ABAR       : " & SK.Strings.Img (Dummy32));
      Dummy16 := Instance.Header.Command;
      Put_Line (Item => " CMD        : " & SK.Strings.Img (Dummy16));
   end Print_PCI_Device_Info;

   -------------------------------------------------------------------------

   procedure Put
     (Bool    : Boolean;
      Newline : Boolean := True)
   is
   begin
      if Bool then
         Put_String (Item => "yes");
      else
         Put_String (Item => "no");
      end if;

      if Newline then
         Debuglog.Client.New_Line;
      end if;
   end Put;

   -------------------------------------------------------------------------

   procedure Put_Line (Item : String) renames Debuglog.Client.Put_Line;

   -------------------------------------------------------------------------

   procedure Put_String (Item : String) renames Debuglog.Client.Put;

end Debug_Ops;
