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

with Debuglog.Client;

with SK.Strings;

with Pciconf;

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

   procedure Print_GPT_Partition_Table_Entry
     (Partition : Gpt.Partition_Entry_Type;
      Index     : Partitions.Partition_Array_Length)
   is
      procedure Print_Wide_String (WS : Gpt.Partition_Name_String)
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
      Put_Line (" First Usable LBA:     " & SK.Strings.Img_Dec (Header.First_Usable_LBA));
      Put_Line (" Last Usable LBA:      " & SK.Strings.Img_Dec (Header.Last_Usable_LBA));
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

end Log;
