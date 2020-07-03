--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Unchecked_Conversion;

with Interfaces;

with Ahci_Drv_Component.Memory;

package Ahci
is

   type Unsigned_2 is mod 2 ** 2;
   for Unsigned_2'Size use 2;

   type Unsigned_3 is mod 2 ** 3;
   for Unsigned_3'Size use 3;

   type Unsigned_4 is mod 2 ** 4;
   for Unsigned_4'Size use 4;

   type Unsigned_5 is mod 2 ** 5;
   for Unsigned_5'Size use 5;

   type Unsigned_6 is mod 2 ** 6;
   for Unsigned_6'Size use 6;

   type Unsigned_10 is mod 2 ** 10;
   for Unsigned_10'Size use 10;

   type Unsigned_22 is mod 2 ** 22;
   for Unsigned_22'Size use 22;

   type Unsigned_25 is mod 2 ** 25;
   for Unsigned_25'Size use 25;

   type Unsigned_31 is mod 2 ** 31;
   for Unsigned_31'Size use 31;

   type Unsigned_48 is mod 2 ** 48;
   for Unsigned_48'Size use 48;

   type Bit_Array is array (Natural range <>) of Boolean
   with
      Pack;

   type Byte_Array is array (Natural range <>) of Interfaces.Unsigned_8
   with
      Pack;

   type Word_Array is array (Natural range <>) of Interfaces.Unsigned_16
   with
      Pack;

   type Port_Range is range 0 .. 31;

   type RW_Type is (Read, Write);

   --  Error constants (matching linux/blk_types.h)
   type Status_Type is  (OK, ENOTSUP, EIO)
      with Size => 8 * 8;
   for  Status_Type use (OK => 0, ENOTSUP => 1, EIO => 10);

   function Status_To_Unsigned64 is new Ada.Unchecked_Conversion
         (Status_Type, Interfaces.Unsigned_64);

   --  Memory Setup Fixme: add individual memory regions to cfg
   Command_Lists_Address : constant :=
      Ahci_Drv_Component.Memory.Dma_Region_Address;
   Command_Lists_Size    : constant := (Port_Range'Last + 1) * 16#400#;

   Command_Table_Address : constant :=
      Command_Lists_Address + Command_Lists_Size;
   Command_Table_Size    : constant := (Port_Range'Last + 1) * 16#100#;

   Fis_Base_Address      : constant :=
      Command_Table_Address + Command_Table_Size;
   Fis_Table_Size        : constant := (Port_Range'Last + 1) * 16#100#;

   DMA_Mem_Base_Address : constant :=
      Fis_Base_Address + Fis_Table_Size;
   DMA_Mem_Size         : constant :=
      Ahci_Drv_Component.Memory.Dma_Region_Size
         - Fis_Table_Size - Command_Table_Size - Command_Lists_Size;

   --  array for internal storage of detected devices
   type Signature_Type is (Empty, Sata, Atapi);
   type Device_Type is record
      Signature         : Signature_Type;
      Support_48Bit     : Boolean;
      Support_Discard   : Boolean;
      Support_SMART     : Boolean;
      Sector_Size       : Interfaces.Unsigned_32;
      Sector_Size_Shift : Integer;
      Number_Of_Sectors : Interfaces.Unsigned_64;
   end record;

   Null_Device : Device_Type :=
      (Signature         => Empty,
       Support_48Bit     => False,
       Support_Discard   => False,
       Support_SMART     => False,
       Sector_Size       => 0,
       Sector_Size_Shift => 0,
       Number_Of_Sectors => 0);

   type Devices_Array is array (Port_Range) of Device_Type;
   Devices : Devices_Array := (others => Null_Device);

end Ahci;
