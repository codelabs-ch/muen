--
--  Copyright (C) 2020 secunet Security Networks AG
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

with Debug_Ops;
with Interfaces;
with SK.Strings;
with System;

with Ahci.Commands;
with Ahci.FIS;
with Ahci.Ports;

use type Interfaces.Unsigned_16;
use type Interfaces.Unsigned_32;
use type Interfaces.Unsigned_64;

package body Ata
is
   -------------------------------------------------------------------------

   Fis_Host_To_Device : constant := 16#27#;
   Fis_H2d_Cmd        : constant := 16#80#;
   Fis_H2d_Dev_Lba    : constant := 16#40#;

   -------------------------------------------------------------------------
   --  ATA Commands

   Ata_Data_Set_Management : constant := 16#06#;
   Ata_Identify_Device_Cmd : constant := 16#ec#;

   Ata_Flush_Cache         : constant := 16#e7#;
   Ata_Flush_Cache_Ext     : constant := 16#ea#;

   Ata_Read_Dma            : constant := 16#c8#;
   Ata_Read_Dma_Ext        : constant := 16#25#;

   Ata_Write_Dma           : constant := 16#ca#;
   Ata_Write_Dma_Ext       : constant := 16#35#;

   Ata_Smart               : constant := 16#b0#;

   -------------------------------------------------------------------------
   --  SMART Features

   SMART_Read_Data              : constant := 16#d0#;
   --  SMART_Attribute_Autosav      : constant := 16#d2#;
   --  SMART_Exec_Offline_Immediate : constant := 16#d4#;
   --  SMART_Read_Log               : constant := 16#d5#;
   --  SMART_Write_Log              : constant := 16#d6#;
   SMART_Enable_Operations      : constant := 16#d8#;
   SMART_Disable_Operations     : constant := 16#d9#;
   SMART_Return_Status          : constant := 16#da#;

   -------------------------------------------------------------------------

   function To_Byte (LWord : Interfaces.Unsigned_64;
                     Pos   : Integer)
       return Interfaces.Unsigned_8
       with Pre => Pos < 8
   is
      Shift : constant Integer := Pos * 8;
   begin
      return Interfaces.Unsigned_8
         (Interfaces.Shift_Right (LWord, Shift) and 16#ff#);
   end To_Byte;

   function To_Byte (DWord : Interfaces.Unsigned_32;
                     Pos   : Integer)
       return Interfaces.Unsigned_8
       with Pre => Pos < 4
   is
      Shift : constant Integer := Pos * 8;
   begin
      return Interfaces.Unsigned_8
         (Interfaces.Shift_Right (DWord, Shift) and 16#ff#);
   end To_Byte;

   function To_Byte (Word : Interfaces.Unsigned_16;
                     Pos  : Integer)
       return Interfaces.Unsigned_8
       with Pre => Pos < 2
   is
      Shift : constant Integer := Pos * 8;
   begin
      return Interfaces.Unsigned_8
         (Interfaces.Shift_Right (Word, Shift) and 16#ff#);
   end To_Byte;

   -------------------------------------------------------------------------

   procedure Setup_H2D_Cmd
      (ID       : Ahci.Port_Range;
       Cmd      : Interfaces.Unsigned_8;
       Start    : Interfaces.Unsigned_64;
       Sectors  : Interfaces.Unsigned_32;
       Features : Interfaces.Unsigned_16)
   is
   begin
      --  Serial Ata Rev 3.0 10.3.4 -> Register Host to Device
      Ahci.Commands.Command_Table (ID).Cfis (0) := Fis_Host_To_Device;
      Ahci.Commands.Command_Table (ID).Cfis (1) := Fis_H2d_Cmd;
      Ahci.Commands.Command_Table (ID).Cfis (2) := Cmd;
      Ahci.Commands.Command_Table (ID).Cfis (3) := To_Byte (Features, 0);
      Ahci.Commands.Command_Table (ID).Cfis (4) := To_Byte (Start, 0);
      Ahci.Commands.Command_Table (ID).Cfis (5) := To_Byte (Start, 1);
      Ahci.Commands.Command_Table (ID).Cfis (6) := To_Byte (Start, 2);
      Ahci.Commands.Command_Table (ID).Cfis (7) := Fis_H2d_Dev_Lba;
      Ahci.Commands.Command_Table (ID).Cfis (8) := To_Byte (Start, 3);

      if Ahci.Devices (ID).Support_48Bit then
         Ahci.Commands.Command_Table (ID).Cfis (9)  := To_Byte (Start, 4);
         Ahci.Commands.Command_Table (ID).Cfis (10) := To_Byte (Start, 5);
      else
         Ahci.Commands.Command_Table (ID).Cfis (9) :=  0;
         Ahci.Commands.Command_Table (ID).Cfis (10) := 0;
      end if;
      Ahci.Commands.Command_Table (ID).Cfis (11) := To_Byte (Features, 1);
      Ahci.Commands.Command_Table (ID).Cfis (12) := To_Byte (Sectors, 0);
      Ahci.Commands.Command_Table (ID).Cfis (13) := To_Byte (Sectors, 1);
      Ahci.Commands.Command_Table (ID).Cfis (14) := 0;
      Ahci.Commands.Command_Table (ID).Cfis (15) := 0;
      Ahci.Commands.Command_Table (ID).Cfis (16) := 0;
      Ahci.Commands.Command_Table (ID).Cfis (17) := 0;
      Ahci.Commands.Command_Table (ID).Cfis (18) := 0;
      Ahci.Commands.Command_Table (ID).Cfis (19) := 0;
   end Setup_H2D_Cmd;

   -------------------------------------------------------------------------

   type LBA_Range_Type is record
      LBA    : Ahci.Unsigned_48;
      Length : Interfaces.Unsigned_16;
   end record
   with Size => 64;

   for LBA_Range_Type use record
      LBA    at 0 range 0 .. 6 * 8 - 1;
      Length at 6 range 0 .. 2 * 8 - 1;
   end record;

   LBA_Range_Entry_Max : constant Integer := Ahci.DMA_Mem_Size / 8 - 1;
   type LBA_Range is range 0 .. LBA_Range_Entry_Max;
   type LBA_Range_List_Type is array (LBA_Range) of LBA_Range_Type;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   LBA_Range_List : LBA_Range_List_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Ahci.DMA_Mem_Base_Address),
      Size    => Ahci.DMA_Mem_Size * 8;
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   procedure Discard_Sectors
      (ID      :     Ahci.Port_Range;
       Start   :     Interfaces.Unsigned_64;
       Count   :     Interfaces.Unsigned_32;
       Ret_Val : out Ahci.Status_Type)
   with
      SPARK_Mode => Off
   is
      use type Ahci.Unsigned_48;

      Cnt     : Interfaces.Unsigned_32 := Count;
      Now     : Interfaces.Unsigned_16;
      Length  : Interfaces.Unsigned_32 := 0;
      Sec     : Ahci.Unsigned_48 := Ahci.Unsigned_48 (Start);
      Len     : Interfaces.Unsigned_32;
      Idx     : LBA_Range := LBA_Range'First;
      Success : Boolean;
   begin
      if (not Ahci.Devices (ID).Support_Discard)
         or (Ahci.Devices (ID).Support_48Bit and
            ((Start >= 16#1_0000_0000_0000#) or (Count > 64 * 1024)))
         or (not Ahci.Devices (ID).Support_48Bit)
      then
            Ret_Val := Ahci.ENOTSUP;
            return;
      end if;
      --  build a list of LBA Range entries as described in
      --  ATA Attachment 8 ATA/ATAP Command Set 7.22.3.6
      while Cnt /= 0 loop
         if Cnt > 65535 then
            Now := 65535;
         else
            Now := Interfaces.Unsigned_16 (Cnt);
         end if;

         LBA_Range_List (Idx).LBA    := Sec;
         LBA_Range_List (Idx).Length := Now;

         Length := Length + (LBA_Range_Type'Size / 8);
         Idx := Idx + 1;
         Cnt := Cnt - Interfaces.Unsigned_32 (Now);
         Sec := Sec + Ahci.Unsigned_48 (Now);
      end loop;

      --  length must be a multiple of 512Byte(64 entries) fill up with zeros
      while (Idx mod 64) /= 0 loop
         LBA_Range_List (Idx).LBA    := 0;
         LBA_Range_List (Idx).Length := 0;
         Idx := Idx + 1;
         Length := Length + (LBA_Range_Type'Size / 8);
      end loop;

      Len := Length;
      Ahci.Commands.Cmd_Slot_Prepare
         (ID, Length, Ahci.DMA_Mem_Base_Address, Ahci.Write);

      if Len /= Length then
         Ret_Val := Ahci.EIO;
      end if;

      Setup_H2D_Cmd (ID       => ID,
                     Cmd      => Ata_Data_Set_Management,
                     Start    => 0,
                     Sectors  => Length / 512,
                     Features => 1);

      Ahci.Ports.Execute (ID      => ID,
                          Timeout => 30,
                          Success => Success);
      pragma Debug (not Success, Debug_Ops.Dump_Cmd_List (ID, 8));
      pragma Debug (not Success, Debug_Ops.Dump_Cmd_Table (ID, 40));

      if Success then
         Ret_Val := Ahci.OK;
      else
         Ret_Val := Ahci.EIO;
      end if;
   end Discard_Sectors;

   ------------------------------------------------------------------------

   function Get_Max_Sector_Count (ID : Ahci.Port_Range)
      return Interfaces.Unsigned_32
   is
   begin
      if Ahci.Devices (ID).Support_48Bit then
         return 64 * 1024;
      else
         return 256;
      end if;
   end Get_Max_Sector_Count;

   -------------------------------------------------------------------------

   procedure RW_Sectors
      (ID      :     Ahci.Port_Range;
       RW      :     Ahci.RW_Type;
       Start   :     Interfaces.Unsigned_64; --  Start Sector
       Count   :     Interfaces.Unsigned_32; --  Number of Sectors
       Address :     Interfaces.Unsigned_64; --  DMA Buffer address
       Ret_Val : out Ahci.Status_Type)
   is
      Bytes    : Interfaces.Unsigned_32;
      Bytes_IO : Interfaces.Unsigned_32;
      Cmd      : Interfaces.Unsigned_8;
      Sectors  : Interfaces.Unsigned_32;
      Success  : Boolean;

      subtype Cmd_Type is Boolean;
      type Cmd_Table_Type is array (Ahci.RW_Type, Cmd_Type)
         of Interfaces.Unsigned_8;

      Cmd_Table : constant Cmd_Table_Type := (
         Ahci.Read  => (Ata_Read_Dma, Ata_Read_Dma_Ext),
         Ahci.Write => (Ata_Write_Dma, Ata_Write_Dma_Ext));
   begin
      if (Ahci.Devices (ID).Support_48Bit and
            ((Start >= 16#1_0000_0000_0000#) or (Count > 64 * 1024)))
         or ((not Ahci.Devices (ID).Support_48Bit) and
            ((Start >= 16#1000_0000#) or (Count > 256)))
      then
         Ret_Val := Ahci.ENOTSUP;
         return;
      end if;

      Cmd   := Cmd_Table (RW, Ahci.Devices (ID).Support_48Bit);
      Bytes := Interfaces.Shift_Left
                  (Count, Ahci.Devices (ID).Sector_Size_Shift);

      Ahci.Commands.Cmd_Slot_Prepare (ID, Bytes, Address, RW);

      Sectors := Interfaces.Shift_Right
                  (Bytes, Ahci.Devices (ID).Sector_Size_Shift);

      if (Ahci.Devices (ID).Support_48Bit and (Sectors = 65536))
         or ((not Ahci.Devices (ID).Support_48Bit) and (Sectors = 256))
      then
         Sectors := 0;
      end if;

      Setup_H2D_Cmd (ID       => ID,
                     Cmd      => Cmd,
                     Start    => Start,
                     Sectors  => Sectors,
                     Features => 0);

      Ahci.Ports.Execute (ID      => ID,
                          Timeout => 30,
                          Success => Success);
      pragma Debug (not Success, Debug_Ops.Dump_Cmd_List (ID, 8));
      pragma Debug (not Success, Debug_Ops.Dump_Cmd_Table (ID, 40));

      Bytes_IO := Ahci.Commands.Command_Lists (ID)(0).PRDBC;
      pragma Debug (Bytes_IO /= Bytes,
         Debug_Ops.Put_Line ("Bytes_IO: " & SK.Strings.Img (Bytes_IO) &
            "/=" & SK.Strings.Img (Bytes)));
      if Success and (Bytes_IO = Bytes)
      then
         Ret_Val := Ahci.OK;
      else
         Ret_Val := Ahci.EIO;
      end if;

   end RW_Sectors;

   -------------------------------------------------------------------------

   procedure Sync
      (ID      :     Ahci.Port_Range;
       Ret_Val : out Ahci.Status_Type)
   is
      Cmd     : Interfaces.Unsigned_8;
      Success : Boolean;
   begin
      if Ahci.Devices (ID).Support_48Bit then
         Cmd := Ata_Flush_Cache_Ext;
      else
         Cmd := Ata_Flush_Cache;
      end if;

      Setup_H2D_Cmd (ID       => ID,
                     Cmd      => Cmd,
                     Start    => 0,
                     Sectors  => 0,
                     Features => 0);

      --  spec says "This command may take longer than 30 seconds to complete."
      --  use 60sec for timeout...
      Ahci.Ports.Execute (ID      => ID,
                          Timeout => 60,
                          Success => Success);
      pragma Debug (not Success, Debug_Ops.Put_Line ("Flush Cache failed!"));
      if Success then
         Ret_Val := Ahci.OK;
      else
         Ret_Val := Ahci.EIO;
      end if;
   end Sync;

   -------------------------------------------------------------------------

   procedure SMART_Execute_Cmd
      (ID      :     Ahci.Port_Range;
       Feature :     Interfaces.Unsigned_16;
       Address :     Interfaces.Unsigned_64 := Ahci.DMA_Mem_Base_Address;
       Ret_Val : out Ahci.Status_Type)
   is
      RW      : constant Ahci.RW_Type := Ahci.Read;
      Length  : Interfaces.Unsigned_32 := 512;
      Success : Boolean;
   begin
      Ret_Val := Ahci.EIO;

      Ahci.Commands.Cmd_Slot_Prepare (Port_ID => ID,
                                      Len     => Length,
                                      Address => Address,
                                      RW      => RW);

      if Length /= 512 then
         return;
      end if;

      Setup_H2D_Cmd (ID       => ID,
                     Cmd      => Ata_Smart,
                     Start    => 16#c24f00#,
                     Sectors  => 0,
                     Features => Feature);

      Ahci.Ports.Execute (ID      => ID,
                          Timeout => 10,
                          Success => Success);

      if Success then
         Ret_Val := Ahci.OK;
      end if;

   end SMART_Execute_Cmd;

   -------------------------------------------------------------------------

   procedure Get_SMART
      (ID      :     Ahci.Port_Range;
       Address :     Interfaces.Unsigned_64; --  DMA Buffer address
       Status  : out Ahci.Device.SMART_Status_Type;
       Ret_Val : out Ahci.Status_Type)
   is
      use type Ahci.Status_Type;
      use type Interfaces.Unsigned_24;
      Return_Status : Interfaces.Unsigned_24;
   begin
      Status := Ahci.Device.Undefined;
      if not Ahci.Devices (ID).Support_SMART then
         Ret_Val := Ahci.ENOTSUP;
         return;
      end if;

      SMART_Execute_Cmd (ID      => ID,
                         Feature => SMART_Read_Data,
                         Address => Address,
                         Ret_Val => Ret_Val);

      if Ret_Val /= Ahci.OK then
         return;
      end if;

      SMART_Execute_Cmd (ID      => ID,
                         Feature => SMART_Return_Status,
                         Ret_Val => Ret_Val);

      if Ret_Val = Ahci.OK then
         Return_Status := Ahci.FIS.Fis_Array (ID).RFIS.LBA0_23;
         if (Return_Status and 16#ffff00#) = 16#c24f00# then
            Status := Ahci.Device.OK;
         elsif (Return_Status and 16#ffff00#) = 16#2cf400# then
            Status := Ahci.Device.Threshold_Exceeded;
         else
            Status := Ahci.Device.Undefined;
         end if;
      end if;
   end Get_SMART;

   -------------------------------------------------------------------------

   procedure SMART_Enable_Disable
      (ID      :     Ahci.Port_Range;
       Enable  :     Boolean;
       Ret_Val : out Ahci.Status_Type)
   is
      Feature : Interfaces.Unsigned_16;
   begin
      if Enable then
         Feature := SMART_Enable_Operations;
      else
         Feature := SMART_Disable_Operations;
      end if;

      SMART_Execute_Cmd (ID      => ID,
                         Feature => Feature,
                         Ret_Val => Ret_Val);
   end SMART_Enable_Disable;

   -------------------------------------------------------------------------
   --  Word 106 type
   type Sector_Size_Type is record
      Size          : Ahci.Unsigned_4;
      Reserved      : Ahci.Bit_Array (4 .. 11);
      Logical_Valid : Boolean;
      Multi_Logical : Boolean;
      Valid         : Ahci.Unsigned_2;
   end record
   with Size => 16;

   for Sector_Size_Type use record
      Size          at 0 range 0 .. 3;
      Reserved      at 0 range 4 .. 11;
      Logical_Valid at 0 range 12 .. 12;
      Multi_Logical at 0 range 13 .. 13;
      Valid         at 0 range 14 .. 15;
   end record;

   type Cmds_Features_Type is record
      SMART_Supported    : Boolean;
      Security_Supported : Boolean;
      Obsolete_82_1      : Boolean;
      PM_Supported       : Boolean;
      Packet_Feat_Sup    : Boolean;
      Volatile_WCache    : Boolean;
      Read_Look_Ahead    : Boolean;
      Release_Int_Sup    : Boolean;
      Service_Int_Sup    : Boolean;
      Reset_Sup          : Boolean;
      HPA_Sup            : Boolean;
      Obsolete_82_2      : Boolean;
      Write_Buffer_Cmd   : Boolean;
      Read_Buffer_Cmd    : Boolean;
      NOP_Cmd            : Boolean;
      Obsolete_82_3      : Boolean;

      Download_Microcode : Boolean;
      TCQ                : Boolean;
      CFA                : Boolean;
      APM                : Boolean;
      Unused_1           : Boolean;
      PUIS               : Boolean;
      Set_features_req   : Boolean;
      Reserved_1         : Boolean;
      HPA                : Boolean;
      AAM                : Boolean;
      Support_48Bit      : Boolean;
      DCO                : Boolean;
      Flush_Cache        : Boolean;
      Flush_Cache_Ext    : Boolean;
      Unused_14          : Boolean;
      Unused_15          : Boolean;

      SMART_Err          : Boolean;
      SMART_Self_Test    : Boolean;
      Media_SN           : Boolean;
      MC_Pass_Through    : Boolean;
      Streaming_Feature  : Boolean;
      GPL_Feature        : Boolean;
      WDMA_FAU_EXT       : Boolean;
      WDMA_QUEUE_FUA_EXT : Boolean;
      Word_Wide_Name_64  : Boolean;
      Obsolete_5         : Boolean;
      Obsolete_6         : Boolean;
      TLC_Reserved_1     : Boolean;
      TLC_Reserved_2     : Boolean;
      IDLE_IMM           : Boolean;
      Unused_14_1        : Boolean;
      Unused_15_1        : Boolean;

      SMART_Enabled      : Boolean;
      Security_Enabled   : Boolean;
      Obsolete_85_1      : Boolean;
      PM_Supported_1     : Boolean;
      Packet_Feat_Sup_1  : Boolean;
      Volatile_WCache_En : Boolean;
      Read_Look_Ahead_En : Boolean;
      Release_Int_En     : Boolean;
      Service_Int_En     : Boolean;
      Reset_Sup_1        : Boolean;
      HPA_Sup_1          : Boolean;
      Obsolete_85_2      : Boolean;
      Write_Buffer_Cmd_1 : Boolean;
      Read_Buffer_Cmd_1  : Boolean;
      NOP_Cmd_1          : Boolean;
      Obsolete_85_3      : Boolean;

   end record
     with Size => 8 * 8;

   for Cmds_Features_Type use record
      SMART_Supported    at 0 range  0 ..  0;
      Security_Supported at 0 range  1 ..  1;
      Obsolete_82_1      at 0 range  2 ..  2;
      PM_Supported       at 0 range  3 ..  3;
      Packet_Feat_Sup    at 0 range  4 ..  4;
      Volatile_WCache    at 0 range  5 ..  5;
      Read_Look_Ahead    at 0 range  6 ..  6;
      Release_Int_Sup    at 0 range  7 ..  7;
      Service_Int_Sup    at 0 range  8 ..  8;
      Reset_Sup          at 0 range  9 ..  9;
      HPA_Sup            at 0 range 10 .. 10;
      Obsolete_82_2      at 0 range 11 .. 11;
      Write_Buffer_Cmd   at 0 range 12 .. 12;
      Read_Buffer_Cmd    at 0 range 13 .. 13;
      NOP_Cmd            at 0 range 14 .. 14;
      Obsolete_82_3      at 0 range 15 .. 15;

      Download_Microcode at 2 range  0 ..  0;
      TCQ                at 2 range  1 ..  1;
      CFA                at 2 range  2 ..  2;
      APM                at 2 range  3 ..  3;
      Unused_1           at 2 range  4 ..  4;
      PUIS               at 2 range  5 ..  5;
      Set_features_req   at 2 range  6 ..  6;
      Reserved_1         at 2 range  7 ..  7;
      HPA                at 2 range  8 ..  8;
      AAM                at 2 range  9 ..  9;
      Support_48Bit      at 2 range 10 .. 10;
      DCO                at 2 range 11 .. 11;
      Flush_Cache        at 2 range 12 .. 12;
      Flush_Cache_Ext    at 2 range 13 .. 13;
      Unused_14          at 2 range 14 .. 14;
      Unused_15          at 2 range 15 .. 15;

      SMART_Err          at 4 range  0 ..  0;
      SMART_Self_Test    at 4 range  1 ..  1;
      Media_SN           at 4 range  2 ..  2;
      MC_Pass_Through    at 4 range  3 ..  3;
      Streaming_Feature  at 4 range  4 ..  4;
      GPL_Feature        at 4 range  5 ..  5;
      WDMA_FAU_EXT       at 4 range  6 ..  6;
      WDMA_QUEUE_FUA_EXT at 4 range  7 ..  7;
      Word_Wide_Name_64  at 4 range  8 ..  8;
      Obsolete_5         at 4 range  9 ..  9;
      Obsolete_6         at 4 range 10 .. 10;
      TLC_Reserved_1     at 4 range 11 .. 11;
      TLC_Reserved_2     at 4 range 12 .. 12;
      IDLE_IMM           at 4 range 13 .. 13;
      Unused_14_1        at 4 range 14 .. 14;
      Unused_15_1        at 4 range 15 .. 15;

      SMART_Enabled      at 6 range  0 ..  0;
      Security_Enabled   at 6 range  1 ..  1;
      Obsolete_85_1      at 6 range  2 ..  2;
      PM_Supported_1     at 6 range  3 ..  3;
      Packet_Feat_Sup_1  at 6 range  4 ..  4;
      Volatile_WCache_En at 6 range  5 ..  5;
      Read_Look_Ahead_En at 6 range  6 ..  6;
      Release_Int_En     at 6 range  7 ..  7;
      Service_Int_En     at 6 range  8 ..  8;
      Reset_Sup_1        at 6 range  9 ..  9;
      HPA_Sup_1          at 6 range 10 .. 10;
      Obsolete_85_2      at 6 range 11 .. 11;
      Write_Buffer_Cmd_1 at 6 range 12 .. 12;
      Read_Buffer_Cmd_1  at 6 range 13 .. 13;
      NOP_Cmd_1          at 6 range 14 .. 14;
      Obsolete_85_3      at 6 range 15 .. 15;
   end record;

   type Ata_Identify_Response_Type is record
      Unused_1            : Ahci.Word_Array (0 .. 22);
      FW                  : String (1 ..  8);
      Model               : String (1 .. 40);
      Unused_2            : Ahci.Word_Array (47 .. 59);
      Number_Of_Sectors   : Interfaces.Unsigned_32;
      Unused_3            : Ahci.Word_Array (62 .. 68);
      Additional_Support  : Interfaces.Unsigned_16;
      Unused_4            : Ahci.Word_Array (70 .. 81);
      Cmds_Features       : Cmds_Features_Type;
      Unused_5            : Ahci.Word_Array (86 .. 99);
      Number_Of_Sectors_2 : Interfaces.Unsigned_64;
      Unused_6            : Ahci.Word_Array (104 .. 105);
      Sector_Size         : Sector_Size_Type;
      Unused_7            : Ahci.Word_Array (107 .. 116);
      Logical_Sector_Size : Interfaces.Unsigned_32;
      Unused_8            : Ahci.Word_Array (119 .. 168);
      DSM_Support         : Ahci.Bit_Array  (0 ..  15);
      Unused_9            : Ahci.Word_Array (170 .. 255);
   end record
   with
      Size => 512 * 8;

   for Ata_Identify_Response_Type use record
      Unused_1            at   0 * 2 range 0 .. 23 * 16 - 1;
      FW                  at  23 * 2 range 0 ..  4 * 16 - 1;
      Model               at  27 * 2 range 0 .. 20 * 16 - 1;
      Unused_2            at  47 * 2 range 0 .. 13 * 16 - 1;
      Number_Of_Sectors   at  60 * 2 range 0 ..  1 * 32 - 1;
      Unused_3            at  62 * 2 range 0 ..  7 * 16 - 1;
      Additional_Support  at  69 * 2 range 0 ..  1 * 16 - 1;
      Unused_4            at  70 * 2 range 0 .. 12 * 16 - 1;
      Cmds_Features       at  82 * 2 range 0 ..  4 * 16 - 1;
      Unused_5            at  86 * 2 range 0 .. 14 * 16 - 1;
      Number_Of_Sectors_2 at 100 * 2 range 0 ..  1 * 64 - 1;
      Unused_6            at 104 * 2 range 0 ..  2 * 16 - 1;
      Sector_Size         at 106 * 2 range 0 ..  1 * 16 - 1;
      Unused_7            at 107 * 2 range 0 .. 10 * 16 - 1;
      Logical_Sector_Size at 117 * 2 range 0 ..  1 * 32 - 1;
      Unused_8            at 119 * 2 range 0 .. 50 * 16 - 1;
      DSM_Support         at 169 * 2 range 0 ..  1 * 16 - 1;
      Unused_9            at 170 * 2 range 0 .. 86 * 16 - 1;
   end record;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Ata_Identify_Response : Ata_Identify_Response_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Ahci.DMA_Mem_Base_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   -------------------------------------------------------------------------

   procedure Convert_Ata_String (Src : in out String)
   with
      SPARK_Mode => Off
   is
      Tmp  : Character;
      Pos  : Natural          := Src'First;
      Last : constant Natural := Src'Last;
   begin
      Convert_Loop : loop
         Tmp := Src (Pos);
         Src (Pos) := Src (Pos + 1);
         Src (Pos + 1) := Tmp;
         Pos := Pos + 2;
         exit Convert_Loop when Pos = Last - 1;
      end loop Convert_Loop;
   end Convert_Ata_String;

   -------------------------------------------------------------------------

   procedure Identify_Device
      (Port_ID   : Ahci.Port_Range)
   with
      SPARK_Mode => Off
   is
      use type Ahci.Unsigned_2;

      Length              : Interfaces.Unsigned_32 :=  512;
      Success             : Boolean;
      FW                  : String (1 ..  8);
      Model               : String (1 .. 40);
      Sector_Size         : Sector_Size_Type;
      Sector_Size_Bytes   : Interfaces.Unsigned_32;
      Logical_Sector_Size : Interfaces.Unsigned_32;
      Number_Of_Sectors   : Interfaces.Unsigned_64;
      RW                  : constant Ahci.RW_Type := Ahci.Read;
   begin
      Ahci.Commands.Cmd_Slot_Prepare (Port_ID => Port_ID,
                                      Len     => Length,
                                      Address => Ahci.DMA_Mem_Base_Address,
                                      RW      => RW);

      if Length /= 512 then
         return;
      end if;

      Setup_H2D_Cmd (ID       => Port_ID,
                     Cmd      => Ata_Identify_Device_Cmd,
                     Start    => 0,
                     Sectors  => 0,
                     Features => 0);

      Ahci.Ports.Execute (ID      => Port_ID,
                          Timeout => 30,
                          Success => Success);

      if Success then
         FW := Ata_Identify_Response.FW;
         Convert_Ata_String (FW);
         Model := Ata_Identify_Response.Model;
         Convert_Ata_String (Model);

         Ahci.Devices (Port_ID).Signature := Ahci.Sata;

         pragma Debug (Debug_Ops.Put_Line
            ("ata: device found: " & Model & " [" & FW & "]"));

         Ahci.Devices (Port_ID).Support_48Bit :=
            Ata_Identify_Response.Cmds_Features.Support_48Bit;

         pragma Debug (Ahci.Devices (Port_ID).Support_48Bit,
            Debug_Ops.Put_Line ("ata: Support for LBA-48 enabled."));

         if Ahci.Devices (Port_ID).Support_48Bit then
            Number_Of_Sectors := Ata_Identify_Response.Number_Of_Sectors_2;
         else
            Number_Of_Sectors := Interfaces.Unsigned_64
               (Ata_Identify_Response.Number_Of_Sectors);
         end if;
         Ahci.Devices (Port_ID).Number_Of_Sectors := Number_Of_Sectors;

         Sector_Size := Ata_Identify_Response.Sector_Size;
         if Sector_Size.Valid /= 2 or not Sector_Size.Logical_Valid
         then
            --  set default sector size
            Ahci.Devices (Port_ID).Sector_Size := 512;
         else
            Logical_Sector_Size :=
               Ata_Identify_Response.Logical_Sector_Size;
            Ahci.Devices (Port_ID).Sector_Size := Logical_Sector_Size * 2;
         end if;

         Ahci.Devices (Port_ID).Sector_Size_Shift := 0;
         Sector_Size_Bytes := Ahci.Devices (Port_ID).Sector_Size;

         Get_Shift : loop
            Sector_Size_Bytes := Sector_Size_Bytes / 2;
            exit Get_Shift when Sector_Size_Bytes = 0;
            Ahci.Devices (Port_ID).Sector_Size_Shift :=
               Ahci.Devices (Port_ID).Sector_Size_Shift + 1;
         end loop Get_Shift;

         pragma Debug (Debug_Ops.Put_Line
               ("ata: Sector Size:" & SK.Strings.Img (
                  Ahci.Devices (Port_ID).Sector_Size)));
         pragma Debug (Debug_Ops.Put_Line
               ("ata: Sector Size Shift:" & SK.Strings.Img (
                  Interfaces.Unsigned_32 (
                     Ahci.Devices (Port_ID).Sector_Size_Shift))));

         Ahci.Devices (Port_ID).Support_Discard :=
            Ata_Identify_Response.DSM_Support (0);
         pragma Debug (Ahci.Devices (Port_ID).Support_Discard,
            Debug_Ops.Put_Line ("ata: Trim supported"));

         Ahci.Devices (Port_ID).Support_SMART :=
            Ata_Identify_Response.Cmds_Features.SMART_Supported;
         pragma Debug (Ahci.Devices (Port_ID).Support_SMART,
            Debug_Ops.Put_Line ("ata: SMART supported"));

         declare
            Ret_Val       : Ahci.Status_Type;
            Smart_Enabled : constant Boolean :=
               Ata_Identify_Response.Cmds_Features.SMART_Enabled;
         begin
            if Ahci.Devices (Port_ID).Support_SMART then
               if not Smart_Enabled then
                  SMART_Enable_Disable (Port_ID, True, Ret_Val);
               end if;
            end if;
         end;
      end if;
   end Identify_Device;
end Ata;
