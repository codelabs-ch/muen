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

with Pciconf;
with Debuglog.Client;

package body Log
with
   SPARK_Mode => Off
is

   -------------------------------------------------------------------------

   procedure Init
     (Epoch : Interfaces.Unsigned_64) renames Debuglog.Client.Init;

   -------------------------------------------------------------------------

   procedure Put_Bit_Array (Item : Bit_Array)
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

   procedure Print_MBR_Partition_Table (Table : Partitions.Partition_Table_Type)
   is
   begin
      for I in Integer range 0 .. Table.Count - 1 loop
         Put_Line
           ("Partition " & SK.Strings.Img
              (Interfaces.Unsigned_32 (I)));
         Put_Line
           (" Start LBA : " & SK.Strings.Img
              (Table.Entries (I).Start_Lba));
         Put_Line
           (" Count     : " & SK.Strings.Img
              (Table.Entries (I).Sector_Cnt));
         Put_Line
           (" Type      : " & SK.Strings.Img
              (Table.Entries (I).Partition_Type));
      end loop;
   end Print_MBR_Partition_Table;

   -------------------------------------------------------------------------

   function Boolean_Image (B : Boolean) return Bool_Image
   is
      T : constant Bool_Image := "True ";
      F : constant Bool_Image := "False";
   begin
      if B then
         return T;
      else
         return F;
      end if;
   end Boolean_Image;

   -------------------------------------------------------------------------

   procedure Put_NVMe_AdminCMD_Image (TypeID : Interfaces.Unsigned_8)
   is
   begin
      case TypeID is
         when 16#00# => Put_String ("Delete_IO_SQ");
         when 16#01# => Put_String ("Create_IO_SQ");
         when 16#02# => Put_String ("Get_Log_Page");
         when 16#04# => Put_String ("Delete_IO_CQ");
         when 16#05# => Put_String ("Create_IO_CQ");
         when 16#06# => Put_String ("Identify");
         when 16#08# => Put_String ("Abort_CMD");
         when 16#09# => Put_String ("Set_Features");
         when 16#0A# => Put_String ("Get_Features");
         when 16#0C# => Put_String ("Async_Event_Req");
         when 16#0D# => Put_String ("Namespace_Mngmt");
         when 16#10# => Put_String ("Firmware_Commit");
         when 16#11# => Put_String ("Firmware_Img_Download");
         when 16#14# => Put_String ("Device_Self_Test");
         when 16#15# => Put_String ("Namespace_Attachment");
         when 16#18# => Put_String ("Keep_Alive");
         when 16#19# => Put_String ("Directive_Send");
         when 16#1A# => Put_String ("Directive_Receive");
         when 16#1C# => Put_String ("Virtualization_Mngmt");
         when 16#1D# => Put_String ("NVMe_MI_Send");
         when 16#1E# => Put_String ("NVMe_MI_Receive");
         when 16#20# => Put_String ("Capacity_Mngmt");
         when 16#24# => Put_String ("Lockdown");
         when 16#7C# => Put_String ("Doorbell_Buffer_Config");
         when 16#7F# => Put_String ("Fabric_Commands");
         when 16#80# => Put_String ("Format_NVM");
         when 16#81# => Put_String ("Security_Send");
         when 16#82# => Put_String ("Security_Receive");
         when 16#84# => Put_String ("Sanitize");
         when 16#86# => Put_String ("Get_LBA_Status");
         when others => Put_String ("Unknown_Command");
      end case;
   end Put_NVMe_AdminCMD_Image;

   -------------------------------------------------------------------------

   procedure Put_NVMe_IOCMD_Image (TypeID : Interfaces.Unsigned_8)
   is
   begin
      case TypeID is
         when 16#00# => Put_String ("Flush");
         when 16#01# => Put_String ("Write");
         when 16#02# => Put_String ("Read");
         when 16#04# => Put_String ("Write_Uncorrectable");
         when 16#05# => Put_String ("Compare");
         when 16#08# => Put_String ("Write_Zeroes");
         when 16#09# => Put_String ("Dataset_Mngmt");
         when 16#0C# => Put_String ("Verify");
         when 16#0D# => Put_String ("Reservation_Register");
         when 16#0E# => Put_String ("Reservation_Report");
         when 16#11# => Put_String ("Reservation_Acquire");
         when 16#15# => Put_String ("Reservation_Release");
         when 16#19# => Put_String ("Copy");
         when others => Put_String ("Unknown_Command");
      end case;
   end Put_NVMe_IOCMD_Image;

   -------------------------------------------------------------------------

   procedure Print_GPT_Partition_Table_Entry
     (Partition : Gpt.Partition_Entry_Type;
      Index     : Partitions.Partition_Array_Length)
   is
      procedure Print_Wide_String (WS : String)
      is
         I : Positive := 1;
      begin
         while I <= WS'Length loop
            exit when WS (I) = Character'Val (0);
            Write_Character (WS (I));
            I := I + 2;
         end loop;
         Put_Line ("");
      end Print_Wide_String;
   begin

      Put_Line ("====== GPT Partition " & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (Index + 1)) &
                   " of " & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (Partitions.Partition_Array_Length'Last)));
      Put_String (" Partition Name:       "); Print_Wide_String (Partition.Partition_Name);
      Put_Line (" Partition Type GUID:  " & SK.Strings.Img (Partition.Partition_Type_GUID));
      Put_Line (" Unique Partition GUID:" & SK.Strings.Img (Partition.Unique_Partition_GUID));
      Put_Line (" Starting LBA:         " & SK.Strings.Img_Dec (Partition.Starting_LBA));
      Put_Line (" Ending LBA:           " & SK.Strings.Img_Dec (Partition.Ending_LBA));
      Put_Line (" Partition Attributes:");
      Put_Line ("  Platform is Required: " & Boolean_Image (Partition.Attributes.Platform_Required));
      Put_Line ("  EFI do not read:      " & Boolean_Image (Partition.Attributes.EFI_Should_Ignore));
      Put_Line ("  Legacy BIOS bootable: " & Boolean_Image (Partition.Attributes.Legacy_Bios_Bootable));

   end Print_GPT_Partition_Table_Entry;

   -------------------------------------------------------------------------

   procedure Print_GPT_Header (Header : Gpt.GPT_Header_Type)
   is
   begin
      Put_Line ("=========== GPT Header ===========================");
      Put_Line (" Signature:            " & Header.Signature);
      Put_Line (" Revision:             " & SK.Strings.Img_Dec (Unsigned_64 (Header.Revision)));
      Put_Line (" Header Size:          " & SK.Strings.Img_Dec (Unsigned_64 (Header.Header_Size)));
      Put_Line (" My LBA:               " & SK.Strings.Img_Dec (Header.My_LBA));
      Put_Line (" Alternate LBA:        " & SK.Strings.Img_Dec (Header.Alternate_LBA));
      Put_Line (" First Usable LBA:     " & SK.Strings.Img_Dec (Header.First_Useable_LBA));
      Put_Line (" Last Usable LBA:      " & SK.Strings.Img_Dec (Header.Last_Useable_LBA));
      Put_Line (" Disk GUID:            " & SK.Strings.Img     (Header.Disk_GUID));
      Put_Line (" Partition Entry LBA:  " & SK.Strings.Img_Dec (Header.Partition_Entry_LBA));
      Put_Line (" Nr. Partition Entries:" & SK.Strings.Img_Dec (Unsigned_64 (Header.Number_Of_Partition_Entries)));
      Put_Line (" Partition Entry Size: " & SK.Strings.Img_Dec (Unsigned_64 (Header.Number_Of_Partition_Entries)));

   end Print_GPT_Header;

   -------------------------------------------------------------------------

   procedure Print_Request (Request : Muenblock.Block_Request_Type)
   is
   begin
      Put_Line ("Request: ");
      Put_Line
        (" Kind: " & SK.Strings.Img
           (Interfaces.Unsigned_16 (Request.Request_Kind)));
      Put_Line
        (" Device_ID     : " & SK.Strings.Img (Request.Device_Id));
      Put_Line
        (" Request_Tag   : " & SK.Strings.Img (Request.Request_Tag));
      Put_Line
        (" Request_Length: " & SK.Strings.Img (Request.Request_Length));
      Put_Line
        (" Device_Offset : " & SK.Strings.Img (Request.Device_Offset));
      Put_Line
        (" Buffer_Offset : " & SK.Strings.Img (Request.Buffer_Offset));
   end Print_Request;

   -------------------------------------------------------------------------

   procedure Print_PCI_Device_Info
   is
      use Pciconf;

      Dummy8  : Interfaces.Unsigned_8;
      Dummy16 : Interfaces.Unsigned_16;
      Dummy32 : Interfaces.Unsigned_32;
   begin
      Put_Line (Item => "== PCI config space");
      Dummy16 := Instance.Header.Vendor_ID;
      Put_Line (Item => " Vendor ID  : " & SK.Strings.Img (Dummy16));
      Dummy16 := Instance.Header.Device_ID;
      Put_Line (Item => " Device ID  : " & SK.Strings.Img (Dummy16));
      Dummy8 := Instance.Header.Info.Revision_ID;
      Put_Line (Item => " Revision   : " & SK.Strings.Img (Dummy8));
      Dummy32 := Interfaces.Unsigned_32 (Instance.Header.Info.Class_Code);
      Put_Line (Item => " Class      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Instance.Header.Base_Address_Register_5;
      Put_Line (Item => " ABAR       : " & SK.Strings.Img (Dummy32));
      Dummy16 := Instance.Header.Command;
      Put_Line (Item => " CMD        : " & SK.Strings.Img (Dummy16));
   end Print_PCI_Device_Info;

   -------------------------------------------------------------------------

   procedure Put_Line (Item : String) renames Debuglog.Client.Put_Line;

   -------------------------------------------------------------------------

   procedure Put_String (Item : String) renames Debuglog.Client.Put;

   -------------------------------------------------------------------------

   procedure Write_Character (Item : Character) renames Debuglog.Client.Put;

   -------------------------------------------------------------------------

   procedure New_Line renames Debuglog.Client.New_Line;

   -------------------------------------------------------------------------

   procedure Print_Status_Code (SC : Interfaces.Unsigned_8; SCT : Unsigned_3)
   is
   begin
      case SCT is
      when 0 =>
         case SC is
               when 0 =>
                  Put_Line ("Status Code: Generic - Successful Completion");
               when 1 =>
                  Put_Line ("Status Code: Generic - Invalid Command Opcode");
               when 2 =>
                  Put_Line ("Status Code: Generic - Invalid Field in Command");
               when 3 =>
                  Put_Line ("Status Code: Generic - Command ID Conflict");
               when 4 =>
                  Put_Line ("Status Code: Generic - Data Transfer Error");
               when 5 =>
                  Put_Line ("Status Code: Generic - Commands Aborted due to Power Loss Notification");
               when 6 =>
                  Put_Line ("Status Code: Generic - Internal Error");
               when 7 =>
                  Put_Line ("Status Code: Generic - Command Abort Requested");
               when 8 =>
                  Put_Line ("Status Code: Generic - Command Aborted due to SQ Deletion");
               when 9 =>
                  Put_Line ("Status Code: Generic - Command Aborted due to Failed Fused Command");
               when 10 =>
                  Put_Line ("Status Code: Generic - Command Aborted due to Missing Fused Command");
               when 11 =>
                  Put_Line ("Status Code: Generic - Invalid Namespace or Format");
               when 12 =>
                  Put_Line ("Status Code: Generic - Command Sequence Error");
               when 13 =>
                  Put_Line ("Status Code: Generic - Invalid SGL Segment Descriptor");
               when 14 =>
                  Put_Line ("Status Code: Generic - Invalid Number of SGL Descriptors");
               when 15 =>
                  Put_Line ("Status Code: Generic - Data SGL Length Invalid");
               when 16 =>
                  Put_Line ("Status Code: Generic - Metadata SGL Length Invalid");
               when 17 =>
                  Put_Line ("Status Code: Generic - SGL Descriptor Type Invalid");
               when 18 =>
                  Put_Line ("Status Code: Generic - Invalid Use of Controller Memory Buffer");
               when 19 =>
                  Put_Line ("Status Code: Generic - PRP Offset Invalid");
               when 20 =>
                  Put_Line ("Status Code: Generic - Atomic Write Unit Exceeded (NVM, ZNS)");
               when 21 =>
                  Put_Line ("Status Code: Generic - Operation Denied");
               when 22 =>
                  Put_Line ("Status Code: Generic - SGL Offset Invalid");
               when 23 =>
                  Put_Line ("Status Code: Generic - RESERVED");
               when 24 =>
                  Put_Line ("Status Code: Generic - Host Identifier Inconsistent Format");
               when 25 =>
                  Put_Line ("Status Code: Generic - Keep Alive Timer Expired");
               when 26 =>
                  Put_Line ("Status Code: Generic - Keep Alive Timeout Invalid");
               when 27 =>
                  Put_Line ("Status Code: Generic - Command Aborted due to Preempt and Abort");
               when 28 =>
                  Put_Line ("Status Code: Generic - Sanitize Failed");
               when 29 =>
                  Put_Line ("Status Code: Generic - Sanitize In Progress");
               when 30 =>
                  Put_Line ("Status Code: Generic - SGL Data Block Granularity Invalid (NVM, ZNS)");
               when 31 =>
                  Put_Line ("Status Code: Generic - Command Not Supported for Queue in CMB");
               when 32 =>
                  Put_Line ("Status Code: Generic - Namespace is Write Protected");
               when 33 =>
                  Put_Line ("Status Code: Generic - Command Interrupted");
               when 34 =>
                  Put_Line ("Status Code: Generic - Transient Transport Error");
               when 35 =>
                  Put_Line ("Status Code: Generic - Command Prohibited by Command and Feature Lockdown");
               when 36 =>
                  Put_Line ("Status Code: Generic - Admin Command Media Not Ready");
               when 37 .. 127 =>
                  Put_Line ("Status Code: Generic - RESERVED");
               when 128 =>
                  Put_Line ("Status Code: Generic - LBA Out of Range (NVM, ZNS)");
               when 129 =>
                  Put_Line ("Status Code: Generic - Capacity Exceeded");
               when 130 =>
                  Put_Line ("Status Code: Generic - Namespace Not Ready");
               when 131 =>
                  Put_Line ("Status Code: Generic - Reservation Conflict");
               when 132 =>
                  Put_Line ("Status Code: Generic - Format In Progress (NVM, ZNS)");
               when 133 =>
                  Put_Line ("Status Code: Generic - Invalid Value Size");
               when 134 =>
                  Put_Line ("Status Code: Generic - Invalid Key Size");
               when 135 =>
                  Put_Line ("Status Code: Generic - KV Key Does Not Exist");
               when 136 =>
                  Put_Line ("Status Code: Generic - Unrecovered Error");
               when 137 =>
                  Put_Line ("Status Code: Generic - Key Exists");
               when 138 .. 191 =>
                  Put_Line ("Status Code: Generic - RESERVED");
               when 192 .. 255 =>
                  Put_Line ("Status Code: Generic - Vendor Specific");
         end case;
      when 1 =>
         case SC is
               when 0 =>
                  Put_Line ("Status Code: Command Specific - Completion Queue Invalid");
               when 1 =>
                  Put_Line ("Status Code: Command Specific - Invalid Queue Identifier");
               when 2 =>
                  Put_Line ("Status Code: Command Specific - Invalid Queue Size");
               when 3 =>
                  Put_Line ("Status Code: Command Specific - Abort Command Limit Exceeded");
               when 4 =>
                  Put_Line ("Status Code: Command Specific - RESERVED");
               when 5 =>
                  Put_Line ("Status Code: Command Specific - Asynchronous Event Request Limit Exceeded");
               when 6 =>
                  Put_Line ("Status Code: Command Specific - Invalid Firmware Slot");
               when 7 =>
                  Put_Line ("Status Code: Command Specific - Invalid Firmware Image");
               when 8 =>
                  Put_Line ("Status Code: Command Specific - Invalid Interrupt Vector");
               when 9 =>
                  Put_Line ("Status Code: Command Specific - Invalid Log Page");
               when 10 =>
                  Put_Line ("Status Code: Command Specific - Invalid Format");
               when 11 =>
                  Put_Line ("Status Code: Command Specific - Firmware Activation Requires Conventional Reset");
               when 12 =>
                  Put_Line ("Status Code: Command Specific - Invalid Queue Deletion");
               when 13 =>
                  Put_Line ("Status Code: Command Specific - Feature Identifier Not Saveable");
               when 14 =>
                  Put_Line ("Status Code: Command Specific - Feature Not Changeable");
               when 15 =>
                  Put_Line ("Status Code: Command Specific - Feature Not Namespace Specific");
               when 16 =>
                  Put_Line ("Status Code: Command Specific - Firmware Activation Requires NVM Subsystem Reset");
               when 17 =>
                  Put_Line ("Status Code: Command Specific - Firmware Activation Requires Controller Level Reset");
               when 18 =>
                  Put_Line ("Status Code: Command Specific - Firmware Activation Requires Maximum Time Violation");
               when 19 =>
                  Put_Line ("Status Code: Command Specific - Firmware Activation Prohibited");
               when 20 =>
                  Put_Line ("Status Code: Command Specific - Overlapping Range");
               when 21 =>
                  Put_Line ("Status Code: Command Specific - Namespace Insufficient Capacity");
               when 22 =>
                  Put_Line ("Status Code: Command Specific - Namespace Identifier Unavailable");
               when 23 =>
                  Put_Line ("Status Code: Command Specific - RESERVED");
               when 24 =>
                  Put_Line ("Status Code: Command Specific - Namespace Already Attached");
               when 25 =>
                  Put_Line ("Status Code: Command Specific - Namespace Is Private");
               when 26 =>
                  Put_Line ("Status Code: Command Specific - Namespace Not Attached");
               when 27 =>
                  Put_Line ("Status Code: Command Specific - Controller List Invalid");
               when 28 =>
                  Put_Line ("Status Code: Command Specific - Device Self-test In Progress");
               when 29 =>
                  Put_Line ("Status Code: Command Specific - Boot Partition Write Prohibited");
               when 30 =>
                  Put_Line ("Status Code: Command Specific - Invalid Controller Identifier");
               when 31 =>
                  Put_Line ("Status Code: Command Specific - Invalid Secondary Controller State");
               when 32 =>
                  Put_Line ("Status Code: Command Specific - Invalid Number of Controller Resources");
               when 33 =>
                  Put_Line ("Status Code: Command Specific - Invalid Resource Identifier");
               when 34 =>
                  Put_Line ("Status Code: Command Specific - Sanitize Prohibited While Persistent Memory Region is Enabled");
               when 35 =>
                  Put_Line ("Status Code: Command Specific - ANA Group Identifier Invalid");
               when 36 =>
                  Put_Line ("Status Code: Command Specific - ANA Attach Failed");
               when 37 =>
                  Put_Line ("Status Code: Command Specific - Insufficient Capacity");
               when 38 =>
                  Put_Line ("Status Code: Command Specific - ANA Attach Failed");
               when 39 =>
                  Put_Line ("Status Code: Command Specific - Namespace Attachment Limit Exceeded");
               when 40 =>
                  Put_Line ("Status Code: Command Specific - Prohibition of Command Execution Not Supported");
               when 41 =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set Not Supported");
               when 42 =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set Not Enabled");
               when 43 =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set Combination Rejected");
               when 44 =>
                  Put_Line ("Status Code: Command Specific - Invalid I/O Command Set");
               when 45 =>
                  Put_Line ("Status Code: Command Specific - Identifier Unavailable");
               when 46 .. 111 =>
                  Put_Line ("Status Code: Command Specific - RESERVED");
               when 112 .. 127 =>
                  Put_Line ("Status Code: Command Specific - Directive Specific"); -- Kapitel 8.7
               when 128  =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Conflicting Attributes");
               when 129 =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Invalid Protection Information");
               when 130 =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Attempted Write to Read Only Range");
               when 131 =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Command Size Limit Exceeded");
               when 132 .. 183 =>
                  Put_Line ("Status Code: Command Specific - RESERVED");
               when 184 =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Zoned Boundary Error");
               when 185 =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Zone Is Full");
               when 186 =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Zone is Read Only");
               when 187 =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Zone is Offline");
               when 188  =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Zone is Invalid Write");
               when 189  =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Too Many Active Zones");
               when 190  =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Too Many Open Zones");
               when 191  =>
                  Put_Line ("Status Code: Command Specific - I/O Command Set - Invalid Zone State Transition");
               when 192 .. 255 =>
                  Put_Line ("Status Code: Command Specific - Vendor Specific");
         end case;
      when 2 =>
         case SC is
               when 0 .. 127 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - RESERVED");
               when 128 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - Write Fault");
               when 129 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - Unrecoverd Read Error");
               when 130 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - End-to-end Guard Check Error");
               when 131 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - End-to-end Application Tag Check Error");
               when 132 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - End-to-end Reference Tag Check Error");
               when 133 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - Compare Failure (NVM)");
               when 134 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - Access Denied");
               when 135 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - Deallocated or Unwritten Logical Block (NVM)");
               when 136 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - End-to-End Storage Tag Check Error");
               when 137 .. 191 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - RESERVED");
               when 192 .. 255 =>
                  Put_Line ("Status Code: Media and Data Integrity Errors - Vendor Specific");
         end case;
      when 3 =>
         case SC is
               when 0 =>
                  Put_Line ("Status Code: Path Related Status - Internal Path Error");
               when 1 =>
                  Put_Line ("Status Code: Path Related Status - Asymmetric Access Persistent Loss");
               when 2 =>
                  Put_Line ("Status Code: Path Related Status - Asymmetric Access Inaccessible");
               when 3 =>
                  Put_Line ("Status Code: Path Related Status - Asymmetric Access Transition");
               when 4 .. 95 =>
                  Put_Line ("Status Code: Path Related Status - RESERVED");
               when 96 =>
                  Put_Line ("Status Code: Path Related Status - Controller Pathing Error - A pathing error was detected by the controller.");
               when 97 .. 111 =>
                  Put_Line ("Status Code: Path Related Status - RESERVED");
               when 112 =>
                  Put_Line ("Status Code: Path Related Status - Host Pathing Error - A pathing error was detected by the host.");
               when 113 =>
                  Put_Line ("Status Code: Path Related Status - Command Aborted By Host");
               when 114 .. 127 =>
                  Put_Line ("Status Code: Path Related Status - RESERVED");
               when 128 .. 191 =>
                  Put_Line ("Status Code: Path Related Status - Other Pathing error - I/O Command Set Specific");
               when 192 .. 255 =>
                  Put_Line ("Status Code: Path Related Status - Vendor Specific");
         end case;
      when 4 .. 6 =>
         Put_Line ("Status Code: RESERVED");
      when 7 =>
         Put_Line ("Status Code: Vendor Specific");
      end case;

   end Print_Status_Code;

end Log;
