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

with System;

with Interfaces;

with Musinfo;

with Ahci_Drv_Component.Devices;

package Ahci.Ports
is

   use Ahci_Drv_Component.Devices;

   type Clear_Error_Type is record
      Sata : Boolean;
      Intr : Boolean;
   end record;

   --  Clear Sata / Interrupt error registers of the port specified by 'ID'.
   procedure Clear_Errors
      (ID    : Port_Range;
       Clear : Clear_Error_Type);

   --  Enable port specified by ID.
   procedure Enable
     (ID      :     Port_Range;
      Success : out Boolean);

   --  Execute commandslot
   --  Timeout: abort command if no response from device withing Timeout sec
   procedure Execute
      (ID      :     Port_Range;
       Timeout :     Integer;
       Success : out Boolean)
   with
      Pre => Timeout >= Natural (Musinfo.TSC_Tick_Rate_Khz_Type'First)
             and Timeout <= Natural (Musinfo.TSC_Tick_Rate_Khz_Type'Last);

   --  Test if the port is active (device detected)
   procedure Is_Active
      (ID     :     Port_Range;
       Active : out Boolean);

   --  Power up port specified by ID.
   procedure Power_Up (ID : Port_Range);

   --  Reset port specified by ID.
   procedure Reset
     (ID      :     Port_Range;
      Success : out Boolean);

   --  Spin up device
   procedure Spin_Up (ID : Port_Range);

   --  Start command processing of command list for port specified by ID.
   procedure Start (ID : Port_Range);

   --  Stop command processing of command list for port specified by ID.
   procedure Stop (ID : Port_Range);

   --  Serial ATA AHCI 1.3.1 Specification, section 3.3.

   type Port_Interrupt_Status_Type is record
      DHRS       : Boolean;
      PSS        : Boolean;
      DSS        : Boolean;
      SDBS       : Boolean;
      UFS        : Boolean;
      DPS        : Boolean;
      PCS        : Boolean;
      DMPS       : Boolean;
      Reserved_1 : Bit_Array (8 .. 21);
      PRCS       : Boolean;
      IPMS       : Boolean;
      OFS        : Boolean;
      Reserved_2 : Boolean;
      INFS       : Boolean;
      IFS        : Boolean;
      HBDS       : Boolean;
      HBFS       : Boolean;
      TFES       : Boolean;
      CPDS       : Boolean;
   end record
   with
      Size => 4 * 8;

   for Port_Interrupt_Status_Type use record
      DHRS       at 16#00# range  0 ..  0;
      PSS        at 16#00# range  1 ..  1;
      DSS        at 16#00# range  2 ..  2;
      SDBS       at 16#00# range  3 ..  3;
      UFS        at 16#00# range  4 ..  4;
      DPS        at 16#00# range  5 ..  5;
      PCS        at 16#00# range  6 ..  6;
      DMPS       at 16#00# range  7 ..  7;
      Reserved_1 at 16#00# range  8 .. 21;
      PRCS       at 16#00# range 22 .. 22;
      IPMS       at 16#00# range 23 .. 23;
      OFS        at 16#00# range 24 .. 24;
      Reserved_2 at 16#00# range 25 .. 25;
      INFS       at 16#00# range 26 .. 26;
      IFS        at 16#00# range 27 .. 27;
      HBDS       at 16#00# range 28 .. 28;
      HBFS       at 16#00# range 29 .. 29;
      TFES       at 16#00# range 30 .. 30;
      CPDS       at 16#00# range 31 .. 31;
   end record;

   Null_Port_Interrupt_Status : constant Port_Interrupt_Status_Type
     := (Reserved_1 => (others => False),
         others     => False);

   type Port_Interrupt_Enable_Type is record
      DHRE       : Boolean;
      PSE        : Boolean;
      DSE        : Boolean;
      SDBE       : Boolean;
      UFE        : Boolean;
      DPE        : Boolean;
      PCE        : Boolean;
      DMPE       : Boolean;
      Reserved_1 : Bit_Array (8 .. 21);
      PRCE       : Boolean;
      IPME       : Boolean;
      OFE        : Boolean;
      Reserved_2 : Boolean;
      INFE       : Boolean;
      IFE        : Boolean;
      HBDE       : Boolean;
      HBFE       : Boolean;
      TFEE       : Boolean;
      CPDE       : Boolean;
   end record
   with
      Size => 4 * 8;

   for Port_Interrupt_Enable_Type use record
      DHRE       at 16#00# range  0 ..  0;
      PSE        at 16#00# range  1 ..  1;
      DSE        at 16#00# range  2 ..  2;
      SDBE       at 16#00# range  3 ..  3;
      UFE        at 16#00# range  4 ..  4;
      DPE        at 16#00# range  5 ..  5;
      PCE        at 16#00# range  6 ..  6;
      DMPE       at 16#00# range  7 ..  7;
      Reserved_1 at 16#00# range  8 .. 21;
      PRCE       at 16#00# range 22 .. 22;
      IPME       at 16#00# range 23 .. 23;
      OFE        at 16#00# range 24 .. 24;
      Reserved_2 at 16#00# range 25 .. 25;
      INFE       at 16#00# range 26 .. 26;
      IFE        at 16#00# range 27 .. 27;
      HBDE       at 16#00# range 28 .. 28;
      HBFE       at 16#00# range 29 .. 29;
      TFEE       at 16#00# range 30 .. 30;
      CPDE       at 16#00# range 31 .. 31;
   end record;

   type Port_Command_Status_Type is record
      ST       : Boolean;
      SUD      : Boolean;
      POD      : Boolean;
      CLO      : Boolean;
      FRE      : Boolean;
      Reserved : Bit_Array (5 .. 7);
      CCS      : Unsigned_5;
      MPSS     : Boolean;
      FR       : Boolean;
      CR       : Boolean;
      CPS      : Boolean;
      PMA      : Boolean;
      HPCP     : Boolean;
      MPSP     : Boolean;
      CPD      : Boolean;
      ESP      : Boolean;
      FBSCP    : Boolean;
      APSTE    : Boolean;
      ATAPI    : Boolean;
      DLAE     : Boolean;
      ALPE     : Boolean;
      ASP      : Boolean;
      ICC      : Unsigned_4;
   end record
   with
      Size => 4 * 8;

   for Port_Command_Status_Type use record
      ST       at 16#00# range  0 ..  0;
      SUD      at 16#00# range  1 ..  1;
      POD      at 16#00# range  2 ..  2;
      CLO      at 16#00# range  3 ..  3;
      FRE      at 16#00# range  4 ..  4;
      Reserved at 16#00# range  5 ..  7;
      CCS      at 16#00# range  8 .. 12;
      MPSS     at 16#00# range 13 .. 13;
      FR       at 16#00# range 14 .. 14;
      CR       at 16#00# range 15 .. 15;
      CPS      at 16#00# range 16 .. 16;
      PMA      at 16#00# range 17 .. 17;
      HPCP     at 16#00# range 18 .. 18;
      MPSP     at 16#00# range 19 .. 19;
      CPD      at 16#00# range 20 .. 20;
      ESP      at 16#00# range 21 .. 21;
      FBSCP    at 16#00# range 22 .. 22;
      APSTE    at 16#00# range 23 .. 23;
      ATAPI    at 16#00# range 24 .. 24;
      DLAE     at 16#00# range 25 .. 25;
      ALPE     at 16#00# range 26 .. 26;
      ASP      at 16#00# range 27 .. 27;
      ICC      at 16#00# range 28 .. 31;
   end record;

   type Port_Task_File_Status_Type is record
      ERR        : Boolean;
      Specific_1 : Bit_Array (1 .. 2);
      DRQ        : Boolean;
      Specific_2 : Bit_Array (4 .. 6);
      BSY        : Boolean;
   end record
   with
      Size => 8;

   for Port_Task_File_Status_Type use record
      ERR        at 16#00# range 0 .. 0;
      Specific_1 at 16#00# range 1 .. 2;
      DRQ        at 16#00# range 3 .. 3;
      Specific_2 at 16#00# range 4 .. 6;
      BSY        at 16#00# range 7 .. 7;
   end record;

   type Port_Task_File_Data_Type is record
      STS      : Port_Task_File_Status_Type;
      ERR      : Interfaces.Unsigned_8;
      Reserved : Interfaces.Unsigned_16;
   end record
   with
      Size => 4 * 8;

   for Port_Task_File_Data_Type use record
      STS      at 16#00# range  0 ..  7;
      ERR      at 16#00# range  8 .. 15;
      Reserved at 16#00# range 16 .. 31;
   end record;

   type Port_SATA_Status_Type is record
      DET      : Unsigned_4;
      SPD      : Unsigned_4;
      IPM      : Unsigned_4;
      Reserved : Bit_Array (12 .. 31);
   end record
   with
      Size => 4 * 8;

   for Port_SATA_Status_Type use record
      DET      at 16#00# range  0 ..  3;
      SPD      at 16#00# range  4 ..  7;
      IPM      at 16#00# range  8 .. 11;
      Reserved at 16#00# range 12 .. 31;
   end record;

   type Port_SATA_Control_Type is record
      DET      : Unsigned_4;
      SPD      : Unsigned_4;
      IPM      : Unsigned_4;
      Reserved : Bit_Array (12 .. 31);
   end record
   with
      Size => 4 * 8;

   for Port_SATA_Control_Type use record
      DET      at 16#00# range  0 ..  3;
      SPD      at 16#00# range  4 ..  7;
      IPM      at 16#00# range  8 .. 11;
      Reserved at 16#00# range 12 .. 31;
   end record;

   type Port_SATA_Error_Type is record
      ERR  : Interfaces.Unsigned_16;
      DIAG : Interfaces.Unsigned_16;
   end record
   with
      Size => 4 * 8;

   for Port_SATA_Error_Type use record
      ERR  at 16#00# range  0 .. 15;
      DIAG at 16#00# range 16 .. 31;
   end record;

   type Port_SATA_Notification_Type is record
      PMN      : Bit_Array (0 .. 15);
      Reserved : Interfaces.Unsigned_16;
   end record
   with
      Size => 4 * 8;

   for Port_SATA_Notification_Type use record
      PMN      at 16#00# range  0 .. 15;
      Reserved at 16#00# range 16 .. 31;
   end record;

   type FIS_Based_Switching_Control_Type is record
      EN         : Boolean;
      DEC        : Boolean;
      SDE        : Boolean;
      Reserved_1 : Bit_Array (3 .. 7);
      DEV        : Unsigned_4;
      ADO        : Unsigned_4;
      DWE        : Unsigned_4;
      Reserved_2 : Bit_Array (20 .. 31);
   end record
   with
      Size => 4 * 8;

   for FIS_Based_Switching_Control_Type use record
      EN         at 16#00# range  0 ..  0;
      DEC        at 16#00# range  1 ..  1;
      SDE        at 16#00# range  2 ..  2;
      Reserved_1 at 16#00# range  3 ..  7;
      DEV        at 16#00# range  8 .. 11;
      ADO        at 16#00# range 12 .. 15;
      DWE        at 16#00# range 16 .. 19;
      Reserved_2 at 16#00# range 20 .. 31;
   end record;

   type Device_Sleep_Type is record
      ADSE     : Boolean;
      DSP      : Boolean;
      DETO     : Interfaces.Unsigned_8;
      MDAT     : Unsigned_5;
      DITO     : Unsigned_10;
      DM       : Unsigned_4;
      Reserved : Bit_Array (29 .. 31);
   end record
   with
      Size => 4 * 8;

   for Device_Sleep_Type use record
      ADSE     at 16#00# range  0 ..  0;
      DSP      at 16#00# range  1 ..  1;
      DETO     at 16#00# range  2 ..  9;
      MDAT     at 16#00# range 10 .. 14;
      DITO     at 16#00# range 15 .. 24;
      DM       at 16#00# range 25 .. 28;
      Reserved at 16#00# range 29 .. 31;
   end record;

   type Port_Registers_Type is record
      Cmd_List_Base_Addr       : Interfaces.Unsigned_32;
      Cmd_List_Base_Upper_Addr : Interfaces.Unsigned_32;
      FIS_Base_Addr            : Interfaces.Unsigned_32;
      FIS_Base_Upper_Addr      : Interfaces.Unsigned_32;
      Interrupt_Status         : Port_Interrupt_Status_Type;
      Interrupt_Enable         : Port_Interrupt_Enable_Type;
      Command_And_Status       : Port_Command_Status_Type;
      Reserved_1               : Interfaces.Unsigned_32;
      Task_File_Data           : Port_Task_File_Data_Type;
      Signature                : Interfaces.Unsigned_32;
      SATA_Status              : Port_SATA_Status_Type;
      SATA_Control             : Port_SATA_Control_Type;
      SATA_Error               : Port_SATA_Error_Type;
      SATA_Active              : Bit_Array (0 .. 31);
      Command_Issue            : Bit_Array (0 .. 31);
      SATA_Notification        : Port_SATA_Notification_Type;
      FIS_Based_Switching_Ctrl : FIS_Based_Switching_Control_Type;
      Device_Sleep             : Device_Sleep_Type;
      Reserved_2               : Byte_Array (16#48# .. 16#6f#);
      Vendor_Specific          : Byte_Array (16#70# .. 16#7f#);
   end record
   with
      Size => 16#80# * 8;

   for Port_Registers_Type use record
      Cmd_List_Base_Addr       at 16#00# range 0 ..  31;
      Cmd_List_Base_Upper_Addr at 16#04# range 0 ..  31;
      FIS_Base_Addr            at 16#08# range 0 ..  31;
      FIS_Base_Upper_Addr      at 16#0c# range 0 ..  31;
      Interrupt_Status         at 16#10# range 0 ..  31;
      Interrupt_Enable         at 16#14# range 0 ..  31;
      Command_And_Status       at 16#18# range 0 ..  31;
      Reserved_1               at 16#1c# range 0 ..  31;
      Task_File_Data           at 16#20# range 0 ..  31;
      Signature                at 16#24# range 0 ..  31;
      SATA_Status              at 16#28# range 0 ..  31;
      SATA_Control             at 16#2c# range 0 ..  31;
      SATA_Error               at 16#30# range 0 ..  31;
      SATA_Active              at 16#34# range 0 ..  31;
      Command_Issue            at 16#38# range 0 ..  31;
      SATA_Notification        at 16#3c# range 0 ..  31;
      FIS_Based_Switching_Ctrl at 16#40# range 0 ..  31;
      Device_Sleep             at 16#44# range 0 ..  31;
      Reserved_2               at 16#48# range 0 .. 319;
      Vendor_Specific          at 16#70# range 0 .. 127;
   end record;

   --  Port Signature Constantsa
   --  ATA/ATAPI Command Set - 3 (ACS-3) T13/2161-D Revision 5, Table 206
   SIG_ATA   : constant := 16#00000101#;
   SIG_ATAPI : constant := 16#eb140101#;
   SIG_SEMB  : constant := 16#c33c0101#;
   SIG_PM    : constant := 16#96690101#;

   type Ports_Array is array (Port_Range) of Port_Registers_Type
   with
      Pack;

   Instance : Ports_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address
        (Ahci_Controller_Ahci_Registers_Address + 16#100#);

end Ahci.Ports;
