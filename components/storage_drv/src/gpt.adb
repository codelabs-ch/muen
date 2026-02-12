with System;

with SK.Strings;

with CRC32;
with Log;
with Storage_Drv_Cspecs_Wrapper;

package body Gpt
is
   -------------------------------------------------------------------------
   -- Named Address Numbers
   -------------------------------------------------------------------------

   GPT_Offset  : constant := 0;
   GPT_Address : constant := Storage_Drv_Cspecs_Wrapper.Memory.Dma_Region_Address + GPT_Offset;

   GPT_Entries_Offset  : constant := 16#200#;
   GPT_Entries_Address : constant := GPT_Address + GPT_Entries_Offset;

   GPT_Header_Array_for_CRC_Address : constant := GPT_Address;

   Alternate_GPT_Header_Offset  : constant := 16#4600#;
   Alternate_GPT_Header_Address : constant := Storage_Drv_Cspecs_Wrapper.Memory.Dma_Region_Address + Alternate_GPT_Header_Offset;

   Alternate_GPT_Header_Array_for_CRC_Address : constant := Alternate_GPT_Header_Address;

   Partition_Entry_Array_for_CRC_Offset  : constant := 16#200#;
   Partition_Entry_Array_for_CRC_Address : constant := GPT_Address + Partition_Entry_Array_for_CRC_Offset;

   Alternate_Partition_Entry_Array_for_CRC_Offset  : constant := 16#4800#;
   Alternate_Partition_Entry_Array_for_CRC_Address : constant := Storage_Drv_Cspecs_Wrapper.Memory.Dma_Region_Address + Alternate_Partition_Entry_Array_for_CRC_Offset;

   -------------------------------------------------------------------------

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   PGPT : Primary_GPT
   with
      Async_Writers,
      Volatile,
      Address => System'To_Address (GPT_Address);

   -- Header

   type CRC_Header_Byte_Array is array (0 .. 91) of Character
   with
      Pack,
      Size => 92 * 8;

   -- same Address as GPT
   GPT_Header_Array_for_CRC : CRC_Header_Byte_Array
   with
      Async_Writers,
      Volatile,
      Address => System'To_Address (GPT_Header_Array_for_CRC_Address);

   Alternate_GPT_Header : GPT_Header_Type
   with
      Async_Writers,
      Volatile,
      Address => System'To_Address (Alternate_GPT_Header_Address);

   Alternate_GPT_Header_Array_for_CRC : CRC_Header_Byte_Array
   with
      Async_Writers,
      Volatile,
      Address => System'To_Address (Alternate_GPT_Header_Array_for_CRC_Address);

   -------------------------------------------------------------------------

   type CRC_Entries_Byte_Array_Index_Type is new Interfaces.Unsigned_32 range 1 .. 128 * 128;
   type CRC_Entries_Byte_Array is array (CRC_Entries_Byte_Array_Index_Type) of Character with Pack;
   Partition_Entry_Array_for_CRC : CRC_Entries_Byte_Array
   with
      Async_Writers,
      Volatile,
      Address => System'To_Address (Partition_Entry_Array_for_CRC_Address);

   Alternate_Partition_Entry_Array_for_CRC : CRC_Entries_Byte_Array
   with
      Async_Writers,
      Volatile,
      Address => System'To_Address (Alternate_Partition_Entry_Array_for_CRC_Address);

   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");

   -------------------------------------------------------------------------

   procedure Calc_Partition_Array_CRC32
      (CRC32_Check                 : Interfaces.Unsigned_32;
       Number_Of_Partition_Entries : Interfaces.Unsigned_32;
       Size_Of_Partition_Entry     : Interfaces.Unsigned_32;
       Is_Alternate_GPT            : Boolean;
       Success                     : out Boolean)
   with Pre => (Number_Of_Partition_Entries in 1 .. 128 and
                Size_Of_Partition_Entry in 1 .. 128)
   is
      CRC        : CRC32.CRC_32;
      CRC32_Calc : Interfaces.Unsigned_32;

      subtype Index_Type is CRC_Entries_Byte_Array_Index_Type
      range 1 ..  CRC_Entries_Byte_Array_Index_Type (Size_Of_Partition_Entry * Number_Of_Partition_Entries);

   begin
      CRC32.Initialize (CRC);
      Success := True;

      if Is_Alternate_GPT then
         for Index in Index_Type'Range loop
            declare
               Partition_Entry : constant Character := Alternate_Partition_Entry_Array_for_CRC (Index);
            begin
               CRC32.Update (CRC, Partition_Entry);
            end;
         end loop;
      else
         for Index in Index_Type'Range loop
            declare
               Partition_Entry : constant Character := Partition_Entry_Array_for_CRC (Index);
            begin
               CRC32.Update (CRC, Partition_Entry);
            end;
         end loop;
      end if;
      CRC32_Calc := CRC32.Get_Value (CRC);

      if CRC32_Calc /= CRC32_Check then
         Success := False;
         Log.Put_Line ("GPT: CRC of " & (if Is_Alternate_GPT then "alternate GPT entry array size wrong" else "GPT entry array size wrong"));
      end if;
   end Calc_Partition_Array_CRC32;

   -------------------------------------------------------------------------

   procedure Calc_Header_CRC32
      (CRC32_Check      :     Interfaces.Unsigned_32;
       Is_Alternate_GPT :     Boolean;
       Success          : out Boolean)
   is
      CRC        : CRC32.CRC_32;
      CRC32_Calc : Interfaces.Unsigned_32;
   begin
      CRC32.Initialize (CRC);
      Success := True;

      if Is_Alternate_GPT then
         pragma Warnings (GNATprove, off, "unused assignment");
         Alternate_GPT_Header.Header_CRC32 := 0;
         pragma Warnings (GNATprove, on, "unused assignment");

         declare
            GPT_Header_Array_for_CRC_TMP : constant CRC_Header_Byte_Array := Alternate_GPT_Header_Array_for_CRC;
         begin
            for Index in CRC_Header_Byte_Array'Range loop
               CRC32.Update (CRC, GPT_Header_Array_for_CRC_TMP (Index));
            end loop;
         end;
         CRC32_Calc := CRC32.Get_Value (CRC);

         Alternate_GPT_Header.Header_CRC32 := CRC32_Check;

         if CRC32_Calc /= CRC32_Check then
            Log.Put_Line ("GPT: CRC of alternate GPT header wrong");
            Log.Put_Line ("GPT: calculated" & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (CRC32_Calc)));
            Log.Put_Line ("GPT: expected  " & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (CRC32_Check)));
            Success := False;

         end if;
      else
         pragma Warnings (GNATprove, off, "unused assignment");
         PGPT.Primary_GPT_Header.Header_CRC32 := 0;
         pragma Warnings (GNATprove, on, "unused assignment");

         declare
            GPT_Header_Array_for_CRC_TMP : constant CRC_Header_Byte_Array := GPT_Header_Array_for_CRC;
         begin
            for Index in CRC_Header_Byte_Array'Range loop
               CRC32.Update (CRC, GPT_Header_Array_for_CRC_TMP (Index));
            end loop;
         end;
         CRC32_Calc := CRC32.Get_Value (CRC);

         PGPT.Primary_GPT_Header.Header_CRC32 := CRC32_Check;

         if CRC32_Calc /= CRC32_Check then
            Log.Put_Line ("GPT: CRC of primary GPT header wrong");
            Log.Put_Line ("GPT: calculated" & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (CRC32_Calc)));
            Log.Put_Line ("GPT: expected  " & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (CRC32_Check)));
            Success := False;
         end if;
      end if;
   end Calc_Header_CRC32;

   -------------------------------------------------------------------------

   -- References
   -- https://source.denx.de/u-boot/u-boot/-/blob/master/doc/README.gpt
   -- https://source.denx.de/u-boot/u-boot/-/blob/master/disk/part_efi.c
   -- https://github.com/ceph/simplegpt/blob/master/simplegpt.py
   -- https://github.com/pvachon/pygpt/blob/master/partition_table_header.py

   procedure Parse
      (ID         :     Ports_Config.Port_Range;
       Part_Table : out Partitions.Partition_Table_Type)
   is
      use type Storage_Interface.Status_Type;

      Status      : Storage_Interface.Status_Type;
      Num_of_LBA  : Interfaces.Unsigned_16;
      Partition   : Partition_Entry_Type;
      Success     : Boolean;

      ----------------------------------------------------------------------

      function Is_Efi_Part (Signature : String) return Boolean
      is
      begin
         return Signature'Length = 8 and then
            Signature (Signature'First)     = 'E' and then
            Signature (Signature'First + 1) = 'F' and then
            Signature (Signature'First + 2) = 'I' and then
            Signature (Signature'First + 3) = ' ' and then
            Signature (Signature'First + 4) = 'P' and then
            Signature (Signature'First + 5) = 'A' and then
            Signature (Signature'First + 6) = 'R' and then
            Signature (Signature'First + 7) = 'T';
      end Is_Efi_Part;

      ----------------------------------------------------------------------

      function Is_Empty_Partition (Partition_Entry : Partition_Entry_Type) return Boolean
      is
      begin
         return Partition_Entry.Unique_Partition_GUID = 0 and then
            Partition_Entry.Partition_Type_GUID = 0;
      end Is_Empty_Partition;

   begin
      Part_Table := Partitions.Null_Partition_Table;

      Log.Put_Line ("GPT: Checking for GPT header");

      -- Read Header
      Storage_Interface.Execute_Read_Command
         (Address => GPT_Address,
          SLBA    => 1,
          NLB     => 0,
          Dev_Id  => ID,
          Status  => Status);

      if Status /= Storage_Interface.OK then
         Log.Put_Line ("GPT: Failed to read GPT header from disk");
         return;
      end if;

      declare
         GPT_Primary_Header : constant GPT_Header_Type := PGPT.Primary_GPT_Header;
      begin
         if not Is_Efi_Part (GPT_Primary_Header.Signature) then
            Log.Put_Line ("GPT: Invalid signature: " & GPT_Primary_Header.Signature);
            return;
         end if;
         if GPT_Primary_Header.Header_Size < 92 then
            Log.Put_Line ("GPT: Invalid GPT header size");
            return;
         end if;

         -- Check Header CRC
         Calc_Header_CRC32
            (CRC32_Check       => GPT_Primary_Header.Header_CRC32,
             Is_Alternate_GPT  => False,
             Success           => Success);
         if not Success then
            return;
         end if;

         if GPT_Primary_Header.My_LBA /= 1 then
            Log.Put_Line ("GPT: Current LBA mismatch");
            Log.Put_Line ("GPT: Header says: " & SK.Strings.Img (GPT_Primary_Header.My_LBA));
            Log.Put_Line ("GPT: expected:    " & SK.Strings.Img (Interfaces.Unsigned_64 (1)));
            return;
         end if;

         -- Check if usable LBA range is within disk
         if GPT_Primary_Header.First_Usable_LBA > Storage_Interface.Get_Sector_Cnt (ID) then
            Log.Put_Line ("GPT: First uable LBA is invalid");
            return;
         end if;
         if GPT_Primary_Header.Last_Usable_LBA > Storage_Interface.Get_Sector_Cnt (ID) then
            Log.Put_Line ("GPT: Last usable LBA is invalid");
            return;
         end if;

         -- Check alternate GPT Header
         Storage_Interface.Execute_Read_Command
            (Address => Alternate_GPT_Header_Address,
             SLBA    => GPT_Primary_Header.Alternate_LBA,
             NLB     => 0,
             Dev_Id  => ID,
             Status  => Status);

         if Status /= Storage_Interface.OK then
            Log.Put_Line ("GPT: Failed to read alternate GPT header from disk");
            return;
         end if;

         declare
            Alternate_GPT_Header_TMP : constant GPT_Header_Type := Alternate_GPT_Header;
         begin
            if not Is_Efi_Part (Alternate_GPT_Header_TMP.Signature) then
               Log.Put_Line ("GPT: alternate GPT header signature invalid");
               Log.Put_Line ("GPT: Primary:   " & GPT_Primary_Header.Signature);
               Log.Put_Line ("GPT: Alternate: " & Alternate_GPT_Header_TMP.Signature);
               return;
            end if;
            if GPT_Primary_Header.Revision /= Alternate_GPT_Header_TMP.Revision then
               Log.Put_Line ("GPT: alternate GPT header revision mismatch");
               Log.Put_Line ("GPT: Primary:   " & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (GPT_Primary_Header.Revision)));
               Log.Put_Line ("GPT: Alternate: " & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (Alternate_GPT_Header_TMP.Revision)));
               return;
            end if;
            if Alternate_GPT_Header_TMP.Header_Size < 92 then
               Log.Put_Line ("GPT: alternate GPT header invalid size");
               return;
            end if;
            if Alternate_GPT_Header_TMP.My_LBA /= GPT_Primary_Header.Alternate_LBA then
               Log.Put_Line ("GPT: alternate GPT eader myLBA mismatch");
               Log.Put_Line ("GPT: Primary wants: " & SK.Strings.Img_Dec (GPT_Primary_Header.Alternate_LBA));
               Log.Put_Line ("GPT: Alternate is:  " & SK.Strings.Img_Dec (Alternate_GPT_Header_TMP.My_LBA));
               return;
            end if;
            if Alternate_GPT_Header_TMP.Alternate_LBA /= GPT_Primary_Header.My_LBA then
               Log.Put_Line ("GPT: alternate GPT header alternate LBA mismatch");
               Log.Put_Line ("GPT: Primary wants: " & SK.Strings.Img_Dec (GPT_Primary_Header.My_LBA));
               Log.Put_Line ("GPT: Alternate is:  " & SK.Strings.Img_Dec (Alternate_GPT_Header_TMP.Alternate_LBA));
               return;
            end if;
            if GPT_Primary_Header.First_Usable_LBA /= Alternate_GPT_Header_TMP.First_Usable_LBA then
               Log.Put_Line ("GPT: alternate GPT header first usable LBA mismatch");
               Log.Put_Line ("GPT: Primary:   " & SK.Strings.Img_Dec (GPT_Primary_Header.First_Usable_LBA));
               Log.Put_Line ("GPT: Alternate: " & SK.Strings.Img_Dec (Alternate_GPT_Header_TMP.First_Usable_LBA));
               return;
            end if;
            if GPT_Primary_Header.Last_Usable_LBA /= Alternate_GPT_Header_TMP.Last_Usable_LBA then
               Log.Put_Line ("GPT: alternate GPT header last usable LBA mismatch");
               Log.Put_Line ("GPT: Primary:   " & SK.Strings.Img_Dec (GPT_Primary_Header.Last_Usable_LBA));
               Log.Put_Line ("GPT: Alternate: " & SK.Strings.Img_Dec (Alternate_GPT_Header_TMP.Last_Usable_LBA));
               return;
            end if;
            if GPT_Primary_Header.Disk_GUID /= Alternate_GPT_Header_TMP.Disk_GUID then
               Log.Put_Line ("GPT: alternate GPT header disk GUID mismatch");
               Log.Put_Line ("GPT: Primary:   " & SK.Strings.Img (GPT_Primary_Header.Disk_GUID));
               Log.Put_Line ("GPT: Alternate: " & SK.Strings.Img (Alternate_GPT_Header_TMP.Disk_GUID));
               return;
            end if;
            if GPT_Primary_Header.Number_Of_Partition_Entries /= Alternate_GPT_Header_TMP.Number_Of_Partition_Entries then
               Log.Put_Line ("GPT: alternate GPT header number of partition entries mismatch");
               Log.Put_Line ("GPT: Primary:   " & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (GPT_Primary_Header.Number_Of_Partition_Entries)));
               Log.Put_Line ("GPT: Alternate: " & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (Alternate_GPT_Header_TMP.Number_Of_Partition_Entries)));
               return;
            end if;
            if GPT_Primary_Header.Size_Of_Partition_Entry /= Alternate_GPT_Header_TMP.Size_Of_Partition_Entry then
               Log.Put_Line ("GPT: alternate GPT header size of partition entry mismatch");
               Log.Put_Line ("GPT: Primary:   " & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (GPT_Primary_Header.Size_Of_Partition_Entry)));
               Log.Put_Line ("GPT: Alternate: " & SK.Strings.Img_Dec (Interfaces.Unsigned_64 (Alternate_GPT_Header_TMP.Size_Of_Partition_Entry)));
               return;
            end if;

            -- Check alternate GPT Header if CRC is correct
            Calc_Header_CRC32
               (CRC32_Check      => Alternate_GPT_Header_TMP.Header_CRC32,
                Is_Alternate_GPT => True,
                Success          => Success);
            if not Success then
               return;
            end if;

            Num_of_LBA := Interfaces.Unsigned_16'Mod (GPT_Primary_Header.Number_Of_Partition_Entries) *
                           Interfaces.Unsigned_16'Mod (GPT_Primary_Header.Size_Of_Partition_Entry) / 512; -- -1?

            -- Check alternate GPT Partition Table if CRC is also correct
            Storage_Interface.Execute_Read_Command
               (Address => Alternate_Partition_Entry_Array_for_CRC_Address,
                SLBA    => Alternate_GPT_Header_TMP.Partition_Entry_LBA,
                NLB     => Interfaces.Unsigned_32 (Num_of_LBA),
                Dev_Id  => ID,
                Status  => Status);

            if Status /= Storage_Interface.OK then
               Log.Put_Line ("GPT: Failed to Read Alternate Partition Entry Array from Disk");
               return;
            end if;

            if Alternate_GPT_Header_TMP.Number_Of_Partition_Entries not in 1 .. 128 then
               Log.Put_Line ("GPT: (Alternate) Invalid Number of Partition Entries.");
               return;
            end if;

            if Alternate_GPT_Header_TMP.Size_Of_Partition_Entry not in 1 .. 128 then
               Log.Put_Line ("GPT: (Alternate) Invalid Size of Partition Entries.");
               return;
            end if;

            Calc_Partition_Array_CRC32
               (CRC32_Check                 => Alternate_GPT_Header_TMP.Partition_Entry_Array_CRC32,
                Number_Of_Partition_Entries => Alternate_GPT_Header_TMP.Number_Of_Partition_Entries,
                Size_Of_Partition_Entry     => Alternate_GPT_Header_TMP.Size_Of_Partition_Entry,
                Is_Alternate_GPT            => True,
                Success                     => Success);
            if not Success
            then
               return;
            end if;

            Log.Put_Line ("GPT: Primary and alternate GPT headers are correct.");
         end;

         -- Check Partiton Entry Array CRC
         -- load Entries

         Storage_Interface.Execute_Read_Command
            (Address => GPT_Entries_Address,
             SLBA    => GPT_Primary_Header.Partition_Entry_LBA,
             NLB     => Interfaces.Unsigned_32 (Num_of_LBA),
             Dev_Id  => ID,
             Status  => Status);

         if Status /= Storage_Interface.OK then
            Log.Put_Line ("GPT: Failed to Read Partition Entry Array from Disk");
            return;
         end if;

         if GPT_Primary_Header.Number_Of_Partition_Entries not in 1 .. 128 then
            Log.Put_Line ("GPT: Invalid Number of Partition Entries.");
            return;
         end if;
         if GPT_Primary_Header.Size_Of_Partition_Entry not in 1 .. 128 then
            Log.Put_Line ("GPT: Invalid Size of Partition Entries.");
            return;
         end if;

         Calc_Partition_Array_CRC32 (GPT_Primary_Header.Partition_Entry_Array_CRC32,
            GPT_Primary_Header.Number_Of_Partition_Entries,
            GPT_Primary_Header.Size_Of_Partition_Entry,
            Is_Alternate_GPT => False,
            Success => Success);
         if not Success
         then
            return;
         end if;
      end;

      -- Parse Partitions
      for I in Entry_Array_Type'Range loop
         pragma Loop_Invariant (Part_Table.Count <= I);
         Partition := PGPT.Entry_Array (I);
         if Partition.Partition_Type_GUID /= Partitions.PARTITION_TYPE_EMPTY then
            Part_Table.Count := Part_Table.Count + 1;
            Part_Table.Entries (I).Partition_Type := Interfaces.Unsigned_8 (16#83#); --ef
            Part_Table.Entries (I).Start_Lba := Partition.Starting_LBA;
            Part_Table.Entries (I).Sector_Cnt := Partition.Ending_LBA - Partition.Starting_LBA;
         end if;
      end loop;

      declare
         GPT_Primary_Header : constant GPT_Header_Type  := PGPT.Primary_GPT_Header;
      begin
         Log.Print_GPT_Header (GPT_Primary_Header);
      end;

      Log.Put_Line ("=========== GPT Partition Table ==================");
      for Index in Partitions.Partition_Array_Range loop
         declare
            Partition_Entry : constant Partition_Entry_Type := PGPT.Entry_Array (Index);
         begin
            if not Is_Empty_Partition (Partition_Entry)
            then
               Log.Print_GPT_Partition_Table_Entry (Partition_Entry, Index);
            end if;
         end;
      end loop;
      Log.Put_Line ("==================================================");
   end Parse;

end Gpt;
