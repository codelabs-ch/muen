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

with Interfaces;
with System;

package Ahci.FIS
is

   --  Serial ATA Revision 3.0, section 10.3.1.
   type FIS_Kind_Type is
     (Host_To_Device, Device_To_Host, DMA_Activate, DMA_Setup,
      Data, BIST_Active, PIO_Setup, Set_Device_Bits);

   FIS_Val : array (FIS_Kind_Type) of Interfaces.Unsigned_8
     := (Host_To_Device  => 16#27#,
         Device_To_Host  => 16#24#,
         DMA_Activate    => 16#39#,
         DMA_Setup       => 16#41#,
         Data            => 16#46#,
         BIST_Active     => 16#58#,
         PIO_Setup       => 16#5f#,
         Set_Device_Bits => 16#a1#);

   --  Serial ATA AHCI 1.3.1 Specification, section 4.2.3.1.
   type Command_FIS_Type is record
      FIS_Type     : Interfaces.Unsigned_8;
      PM_Port      : Unsigned_4;
      Reserved_1   : Bit_Array (12 .. 14);
      C            : Boolean;
      Command      : Interfaces.Unsigned_8;
      Features0_7  : Interfaces.Unsigned_8;
      LBA0_23      : Interfaces.Unsigned_24;
      Device       : Interfaces.Unsigned_8;
      LBA_24_47    : Interfaces.Unsigned_24;
      Features8_15 : Interfaces.Unsigned_8;
      Count        : Interfaces.Unsigned_16;
      ICC          : Bit_Array (16 .. 23);
      Control      : Interfaces.Unsigned_8;
      Reserved_2   : Bit_Array (0 .. 31);
   end record
   with
      Size => 20 * 8;

   for Command_FIS_Type use record
      FIS_Type     at  0 range  0 ..  7;
      PM_Port      at  0 range  8 .. 11;
      Reserved_1   at  0 range 12 .. 14;
      C            at  0 range 15 .. 15;
      Command      at  0 range 16 .. 23;
      Features0_7  at  0 range 24 .. 31;
      LBA0_23      at  4 range  0 .. 23;
      Device       at  4 range 24 .. 31;
      LBA_24_47    at  8 range  0 .. 23;
      Features8_15 at  8 range 24 .. 31;
      Count        at 12 range  0 .. 15;
      ICC          at 12 range 16 .. 23;
      Control      at 12 range 24 .. 31;
      Reserved_2   at 16 range  0 .. 31;
   end record;

   --  Serial ATA Revision 3.0, section 10.3.5.
   type Device_To_Host_FIS_Type is record
      FIS_Type   : Interfaces.Unsigned_8;
      PM_Port    : Unsigned_4;
      Reserved_1 : Bit_Array (12 .. 13);
      I          : Boolean;
      Reserved_2 : Boolean;
      Status     : Interfaces.Unsigned_8;
      Error      : Interfaces.Unsigned_8;
      LBA0_23    : Interfaces.Unsigned_24;
      Device     : Interfaces.Unsigned_8;
      LBA_24_47  : Interfaces.Unsigned_24;
      Reserved_3 : Bit_Array (24 .. 31);
      Count      : Interfaces.Unsigned_16;
      Reserved_4 : Bit_Array (16 .. 31);
      Reserved_5 : Bit_Array (0 .. 31);
   end record
   with
      Size => 20 * 8;

   for Device_To_Host_FIS_Type use record
      FIS_Type   at  0 range  0 ..  7;
      PM_Port    at  0 range  8 .. 11;
      Reserved_1 at  0 range 12 .. 13;
      I          at  0 range 14 .. 14;
      Reserved_2 at  0 range 15 .. 15;
      Status     at  0 range 16 .. 23;
      Error      at  0 range 24 .. 31;
      LBA0_23    at  4 range  0 .. 23;
      Device     at  4 range 24 .. 31;
      LBA_24_47  at  8 range  0 .. 23;
      Reserved_3 at  8 range 24 .. 31;
      Count      at 12 range  0 .. 15;
      Reserved_4 at 12 range 16 .. 31;
      Reserved_5 at 16 range  0 .. 31;
   end record;

   --  Serial ATA Revision 3.0, section 10.3.6.
   type Set_Device_Bits_FIS_Type is record
      FIS_Type          : Interfaces.Unsigned_8;
      PM_Port           : Unsigned_4;
      Reserved_1        : Bit_Array (12 .. 13);
      I                 : Boolean;
      N                 : Boolean;
      Status_Lo         : Unsigned_3;
      Reserved_2        : Boolean;
      Status_Hi         : Unsigned_3;
      Reserved_3        : Boolean;
      Error             : Interfaces.Unsigned_8;
      Protocol_Specific : Interfaces.Unsigned_32;
   end record
   with
      Size => 8 * 8;

   for Set_Device_Bits_FIS_Type use record
      FIS_Type          at 0 range  0 ..  7;
      PM_Port           at 0 range  8 .. 11;
      Reserved_1        at 0 range 12 .. 13;
      I                 at 0 range 14 .. 14;
      N                 at 0 range 15 .. 15;
      Status_Lo         at 0 range 16 .. 18;
      Reserved_2        at 0 range 19 .. 19;
      Status_Hi         at 0 range 20 .. 22;
      Reserved_3        at 0 range 23 .. 23;
      Error             at 0 range 24 .. 31;
      Protocol_Specific at 4 range  0 .. 31;
   end record;

   --  Serial ATA Revision 3.0, section 10.3.7.
   type DMA_Activate_FIS_Type is record
      FIS_Type : Interfaces.Unsigned_8;
      PM_Port  : Unsigned_4;
      Reserved : Bit_Array (12 .. 31);
   end record
   with
      Size => 4 * 8;

   for DMA_Activate_FIS_Type use record
      FIS_Type at 0 range  0 ..  7;
      PM_Port  at 0 range  8 .. 11;
      Reserved at 0 range 12 .. 31;
   end record;

   --  Serial ATA Revision 3.0, section 10.3.8.
   type DMA_Setup_FIS_Type is record
      FIS_Type           : Interfaces.Unsigned_8;
      PM_Port            : Unsigned_4;
      R                  : Boolean;
      D                  : Boolean;
      I                  : Boolean;
      A                  : Boolean;
      Reserved_1         : Bit_Array (16 .. 31);
      DMA_Buffer_ID_Low  : Interfaces.Unsigned_32;
      DMA_Buffer_ID_High : Interfaces.Unsigned_32;
      Reserved_2         : Interfaces.Unsigned_32;
      DMA_Buffer_Offset  : Interfaces.Unsigned_32;
      DMA_Transfer_Count : Interfaces.Unsigned_32;
      Reserved_3         : Bit_Array (0 .. 31);
   end record
   with
      Size => 28 * 8;

   for DMA_Setup_FIS_Type use record
      FIS_Type           at  0 range  0 ..  7;
      PM_Port            at  0 range  8 .. 11;
      R                  at  0 range 12 .. 12;
      D                  at  0 range 13 .. 13;
      I                  at  0 range 14 .. 14;
      A                  at  0 range 15 .. 15;
      Reserved_1         at  0 range 16 .. 31;
      DMA_Buffer_ID_Low  at  4 range  0 .. 31;
      DMA_Buffer_ID_High at  8 range  0 .. 31;
      Reserved_2         at 12 range  0 .. 31;
      DMA_Buffer_Offset  at 16 range  0 .. 31;
      DMA_Transfer_Count at 20 range  0 .. 31;
      Reserved_3         at 24 range  0 .. 31;
   end record;

   --  Serial ATA Revision 3.0, section 10.3.10.
   type PIO_Setup_FIS_Type is record
      FIS_Type       : Interfaces.Unsigned_8;
      PM_Port        : Unsigned_4;
      Reserved_1     : Boolean;
      D              : Boolean;
      I              : Boolean;
      Reserved_2     : Boolean;
      Status         : Interfaces.Unsigned_8;
      Error          : Interfaces.Unsigned_8;
      LBA0_23        : Interfaces.Unsigned_24;
      Device         : Interfaces.Unsigned_8;
      LBA_24_47      : Interfaces.Unsigned_24;
      Reserved_3     : Bit_Array (24 .. 31);
      Count          : Interfaces.Unsigned_16;
      Reserved_4     : Bit_Array (16 .. 23);
      E_Status       : Interfaces.Unsigned_8;
      Transfer_Count : Interfaces.Unsigned_16;
      Reserved_5     : Bit_Array (16 .. 31);
   end record
   with
      Size => 20 * 8;

   for PIO_Setup_FIS_Type use record
      FIS_Type       at  0 range  0 ..  7;
      PM_Port        at  0 range  8 .. 11;
      Reserved_1     at  0 range 12 .. 12;
      D              at  0 range 13 .. 13;
      I              at  0 range 14 .. 14;
      Reserved_2     at  0 range 15 .. 15;
      Status         at  0 range 16 .. 23;
      Error          at  0 range 24 .. 31;
      LBA0_23        at  4 range  0 .. 23;
      Device         at  4 range 24 .. 31;
      LBA_24_47      at  8 range  0 .. 23;
      Reserved_3     at  8 range 24 .. 31;
      Count          at 12 range  0 .. 15;
      Reserved_4     at 12 range 16 .. 23;
      E_Status       at 12 range 24 .. 31;
      Transfer_Count at 16 range  0 .. 15;
      Reserved_5     at 16 range 16 .. 31;
   end record;

   --  Serial ATA AHCI 1.3.1 Specification, section 4.2.1.
   type Received_FIS_Type is record
      DSFIS     : DMA_Setup_FIS_Type;
      Reserved1 : Interfaces.Unsigned_32;
      PSFIS     : PIO_Setup_FIS_Type;
      Reserved2 : Byte_Array (0 .. 11);
      RFIS      : Device_To_Host_FIS_Type;
      Reserved3 : Interfaces.Unsigned_32;
      SDBFIS    : Set_Device_Bits_FIS_Type;
      UFIS      : Byte_Array (0 .. 63);
      Reserved4 : Byte_Array (0 .. 95);
   end record
   with
      Size => 256 * 8;

   for Received_FIS_Type use record
      DSFIS     at 16#00# range 0 .. 28 * 8 - 1;
      Reserved1 at 16#1c# range 0 .. 31;
      PSFIS     at 16#20# range 0 .. 16#14# * 8 - 1;
      Reserved2 at 16#34# range 0 .. 12 * 8 - 1;
      RFIS      at 16#40# range 0 .. 16#14# * 8 - 1;
      Reserved3 at 16#54# range 0 .. 31;
      SDBFIS    at 16#58# range 0 .. 16#08# * 8 - 1;
      UFIS      at 16#60# range 0 .. 64 * 8 - 1;
      Reserved4 at 16#A0# range 0 .. 96 * 8 - 1;
   end record;

   type FIS_Array_Type is array (Port_Range) of Received_FIS_Type;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Fis_Array : FIS_Array_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Fis_Base_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

end Ahci.FIS;
