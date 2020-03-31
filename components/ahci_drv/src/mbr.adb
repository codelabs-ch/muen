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

with Ahci.Device;
with Interfaces;
with Debug_Ops;
with System;

package body Mbr
is
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;

   type CHS_Address_Type is record
      Head          : Interfaces.Unsigned_8;
      Sector        : Ahci.Unsigned_6;
      Cylinder_High : Ahci.Unsigned_2;
      Cylinder_Low  : Interfaces.Unsigned_8;
   end record
   with
      Size => 3 * 8;

   for CHS_Address_Type use record
      Head           at 0 range 0 .. 7;
      Sector         at 1 range 0 .. 5;
      Cylinder_High  at 1 range 6 .. 7;
      Cylinder_Low   at 2 range 0 .. 7;
   end record;

   type Partition_Entry_Type is record
      Status           : Interfaces.Unsigned_8;
      CHS_First_Sector : CHS_Address_Type;
      Partition_Type   : Interfaces.Unsigned_8;
      CHS_Last_Sector  : CHS_Address_Type;
      LBA_First_Sector : Interfaces.Unsigned_32;
      Sector_Cnt       : Interfaces.Unsigned_32;
   end record
   with
      Size => 16 * 8;

   for Partition_Entry_Type use record
      Status           at 16#0# range 0 .. 1 * 8 - 1;
      CHS_First_Sector at 16#1# range 0 .. 3 * 8 - 1;
      Partition_Type   at 16#4# range 0 .. 1 * 8 - 1;
      CHS_Last_Sector  at 16#5# range 0 .. 3 * 8 - 1;
      LBA_First_Sector at 16#8# range 0 .. 4 * 8 - 1;
      Sector_Cnt       at 16#c# range 0 .. 4 * 8 - 1;
   end record;

   type Partition_Array is array (Natural range <>) of Partition_Entry_Type
   with
      Pack;

   type MBR_Type is record
      Bootstrap_Code : Ahci.Byte_Array (0 .. 445);
      Partition_1_4  : Partition_Array (1 .. 4);
      Boot_Signature : Interfaces.Unsigned_16;
   end record
   with
      Size => 512 * 8;

   for MBR_Type use record
      Bootstrap_Code at 16#000# range 0 .. 446 * 8 - 1;
      Partition_1_4  at 16#1be# range 0 .. 4 * 16 * 8 - 1;
      Boot_Signature at 16#1fe# range 0 .. 2 * 8 - 1;
   end record;

   MBR_Entry : MBR_Type
   with
      Async_Writers,
      Volatile,
      Address => System'To_Address (Ahci.DMA_Mem_Base_Address);

   procedure Parse
      (ID         :     Ahci.Port_Range;
       Part_Table : out Partition_Table_Type)
   is
      use type Ahci.Status_Type;
      Ret           : Ahci.Status_Type;
      Sig           : Interfaces.Unsigned_16;
      Sector        : Interfaces.Unsigned_64 := 0;
      Start         : Integer := 0;
      Found         : Integer := 0;
      Stop_Parsing  : Boolean := True;
      Partition     : Partition_Entry_Type;
      EBR_Base      : Interfaces.Unsigned_32 := 0;
   begin
      Part_Table.Count := 0;
      Parse_Loop : loop
         --  read sector containing a MBR / EBR entry
         Ahci.Device.RW_Sectors (ID => ID,
            RW       => Ahci.Read,
            Start    => Sector,
            Count    => 1,
            Address  => Ahci.DMA_Mem_Base_Address,
            Ret_Val  => Ret);

         Sig := MBR_Entry.Boot_Signature;
         if Ret /= Ahci.OK or Sig /= 16#aa55# then
            pragma Debug (Ret /= Ahci.OK,
               Debug_Ops.Put_Line ("Read failed"));

            pragma Debug (Sig /= 16#aa55#,
               Debug_Ops.Put_Line ("Signature invalid"));
            Part_Table.Count := -1;
            return;
         end if;

         Found := 0;
         Stop_Parsing := True;

         for I in Natural range 1 .. 4 loop
            Partition := MBR_Entry.Partition_1_4 (I);
            if Partition.Partition_Type /= Partitions.PARTITION_TYPE_EMPTY
            then
               if Partition.Partition_Type /= Partitions.PARTITION_TYPE_EBR
               then
                  --  for normal partitions the start_lba gives the LBA
                  --  relative to the start of the current EBR LBA
                  Part_Table.Entries (Part_Table.Count + Found).Start_Lba :=
                     Interfaces.Unsigned_64 (Partition.LBA_First_Sector) +
                     Sector;
               else
                  --  for EBR sector is relative addressed to start of ebr
                  Part_Table.Entries (Part_Table.Count + Found).Start_Lba :=
                     Interfaces.Unsigned_64 (Partition.LBA_First_Sector +
                     EBR_Base);
                  if EBR_Base = 0 then
                     EBR_Base := Partition.LBA_First_Sector;
                  end if;

                  --  the next partiton table is at
                  Sector :=
                     Part_Table.Entries (Part_Table.Count + Found).Start_Lba;

                  Stop_Parsing := False;
               end if;

               Part_Table.Entries (Part_Table.Count + Found).Sector_Cnt :=
                  Interfaces.Unsigned_64 (Partition.Sector_Cnt);
               Part_Table.Entries (Part_Table.Count + Found).Partition_Type :=
                  Partition.Partition_Type;
               Found := Found + 1;
            end if;
         end loop;
         Part_Table.Count := Part_Table.Count + Found;
         exit Parse_Loop when Stop_Parsing;

         Start := Start + Found;
      end loop Parse_Loop;
   end Parse;
end Mbr;
