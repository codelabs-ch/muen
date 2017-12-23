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

with Ahci_Drv_Component.Devices;

package Ahci.Registers
is

   use Ahci_Drv_Component.Devices;

   --  Serial ATA AHCI 1.3.1 Specification, section 3.1.

   type Unsigned_4 is mod 2 ** 4;
   for Unsigned_4'Size use 4;

   type Unsigned_5 is mod 2 ** 5;
   for Unsigned_5'Size use 5;

   type Bit_Array is array (Natural range <>) of Boolean
   with
      Pack;

   type Byte_Array is array (Natural range <>) of Interfaces.Unsigned_8
   with
      Pack;

   type HBA_Caps_Type is record
      NP       : Unsigned_5;
      SXS      : Boolean;
      EMS      : Boolean;
      CCCS     : Boolean;
      NCS      : Unsigned_5;
      PSC      : Boolean;
      SSC      : Boolean;
      PMD      : Boolean;
      FBSS     : Boolean;
      SPM      : Boolean;
      SAM      : Boolean;
      Reserved : Boolean;
      ISS      : Unsigned_4;
      SCLO     : Boolean;
      SAL      : Boolean;
      SALP     : Boolean;
      SSS      : Boolean;
      SMPS     : Boolean;
      SSNTF    : Boolean;
      SNCQ     : Boolean;
      S64A     : Boolean;
   end record
   with
      Size => 4 * 8;

   for HBA_Caps_Type use record
      NP       at 0 range  0 ..  4;
      SXS      at 0 range  5 ..  5;
      EMS      at 0 range  6 ..  6;
      CCCS     at 0 range  7 ..  7;
      NCS      at 0 range  8 .. 12;
      PSC      at 0 range 13 .. 13;
      SSC      at 0 range 14 .. 14;
      PMD      at 0 range 15 .. 15;
      FBSS     at 0 range 16 .. 16;
      SPM      at 0 range 17 .. 17;
      SAM      at 0 range 18 .. 18;
      Reserved at 0 range 19 .. 19;
      ISS      at 0 range 20 .. 23;
      SCLO     at 0 range 24 .. 24;
      SAL      at 0 range 25 .. 25;
      SALP     at 0 range 26 .. 26;
      SSS      at 0 range 27 .. 27;
      SMPS     at 0 range 28 .. 28;
      SSNTF    at 0 range 29 .. 29;
      SNCQ     at 0 range 30 .. 30;
      S64A     at 0 range 31 .. 31;
   end record;

   type Global_HBA_Control_Type is record
      HR       : Boolean;
      IE       : Boolean;
      MRSM     : Boolean;
      Reserved : Bit_Array (3 .. 30);
      AE       : Boolean;
   end record
   with
      Size => 4 * 8;

   for Global_HBA_Control_Type use record
      HR       at 0 range  0 ..  0;
      IE       at 0 range  1 ..  1;
      MRSM     at 0 range  2 ..  2;
      Reserved at 0 range  3 .. 30;
      AE       at 0 range 31 .. 31;
   end record;

   type Version_Type is record
      MIN : Interfaces.Unsigned_16;
      MJR : Interfaces.Unsigned_16;
   end record
   with
      Size => 4 * 8;

   for Version_Type use record
      MIN at 0 range  0 .. 15;
      MJR at 0 range 16 .. 31;
   end record;

   type CCC_Control_Type is record
      EN       : Boolean;
      Reserved : Bit_Array (1 .. 2);
      Int      : Unsigned_5;
      CC       : Interfaces.Unsigned_8;
      TV       : Interfaces.Unsigned_16;
   end record
   with
      Size => 4 * 8;

   for CCC_Control_Type use record
      EN       at 0 range  0 ..  0;
      Reserved at 0 range  1 ..  2;
      Int      at 0 range  3 ..  7;
      CC       at 0 range  8 .. 15;
      TV       at 0 range 16 .. 31;
   end record;

   type EM_Location_Type is record
      SZ   : Interfaces.Unsigned_16;
      OFST : Interfaces.Unsigned_16;
   end record
   with
      Size => 4 * 8;

   for EM_Location_Type use record
      SZ   at 0 range  0 .. 15;
      OFST at 0 range 16 .. 31;
   end record;

   type EM_Control_Type is record
      STS_MR     : Boolean;
      Reserved_1 : Bit_Array (1 .. 7);
      CTL_TM     : Boolean;
      CTL_RST    : Boolean;
      Reserved_2 : Bit_Array (10 .. 15);
      SUPP_LED   : Boolean;
      SUPP_SAFTE : Boolean;
      SUPP_SES2  : Boolean;
      SUPP_SGPIO : Boolean;
      Reserved_3 : Bit_Array (20 .. 23);
      ATTR_SMB   : Boolean;
      ATTR_XMT   : Boolean;
      ATTR_ALHD  : Boolean;
      ATTR_PM    : Boolean;
      Reserved_4 : Bit_Array (28 .. 31);
   end record
   with
      Size => 4 * 8;

   for EM_Control_Type use record
      STS_MR     at 0 range  0 ..  0;
      Reserved_1 at 0 range  1 ..  7;
      CTL_TM     at 0 range  8 ..  8;
      CTL_RST    at 0 range  9 ..  9;
      Reserved_2 at 0 range 10 .. 15;
      SUPP_LED   at 0 range 16 .. 16;
      SUPP_SAFTE at 0 range 17 .. 17;
      SUPP_SES2  at 0 range 18 .. 18;
      SUPP_SGPIO at 0 range 19 .. 19;
      Reserved_3 at 0 range 20 .. 23;
      ATTR_SMB   at 0 range 24 .. 24;
      ATTR_XMT   at 0 range 25 .. 25;
      ATTR_ALHD  at 0 range 26 .. 26;
      ATTR_PM    at 0 range 27 .. 27;
      Reserved_4 at 0 range 28 .. 31;
   end record;

   type HBA_Caps_Ext_Type is record
      BOH      : Boolean;
      NVMP     : Boolean;
      APST     : Boolean;
      SDS      : Boolean;
      SADM     : Boolean;
      DESO     : Boolean;
      Reserved : Bit_Array (6 .. 31);
   end record
   with
      Size => 4 * 8;

   for HBA_Caps_Ext_Type use record
      BOH      at 0 range 0 ..  0;
      NVMP     at 0 range 1 ..  1;
      APST     at 0 range 2 ..  2;
      SDS      at 0 range 3 ..  3;
      SADM     at 0 range 4 ..  4;
      DESO     at 0 range 5 ..  5;
      Reserved at 0 range 6 .. 31;
   end record;

   type BIOS_HO_Status_Type is record
      BOS      : Boolean;
      OOS      : Boolean;
      SOOE     : Boolean;
      OOC      : Boolean;
      BB       : Boolean;
      Reserved : Bit_Array (5 .. 31);
   end record
   with
      Size => 4 * 8;

   for BIOS_HO_Status_Type use record
      BOS      at 0 range 0 ..  0;
      OOS      at 0 range 1 ..  1;
      SOOE     at 0 range 2 ..  2;
      OOC      at 0 range 3 ..  3;
      BB       at 0 range 4 ..  4;
      Reserved at 0 range 5 .. 31;
   end record;

   type Generic_Host_Control_Type is record
      Host_Capabilities     : HBA_Caps_Type;
      Global_Host_Control   : Global_HBA_Control_Type;
      Interrupt_Status      : Bit_Array (0 .. 31);
      Ports_Implemented     : Bit_Array (0 .. 31);
      Version               : Version_Type;
      CCC_Control           : CCC_Control_Type;
      CCC_Ports             : Bit_Array (1 .. 31);
      Enclosure_Mgmt_Loc    : EM_Location_Type;
      Enclosure_Mgmt_Ctrl   : EM_Control_Type;
      Host_Capabilities_Ext : HBA_Caps_Ext_Type;
      BIOS_HO_Status_Ctrl   : BIOS_HO_Status_Type;
   end record
   with
      Size => 16#2c# * 8;

   for Generic_Host_Control_Type use record
      Host_Capabilities     at 16#00# range 0 .. 31;
      Global_Host_Control   at 16#04# range 0 .. 31;
      Interrupt_Status      at 16#08# range 0 .. 31;
      Ports_Implemented     at 16#0c# range 0 .. 31;
      Version               at 16#10# range 0 .. 31;
      CCC_Control           at 16#14# range 0 .. 31;
      CCC_Ports             at 16#18# range 0 .. 31;
      Enclosure_Mgmt_Loc    at 16#1c# range 0 .. 31;
      Enclosure_Mgmt_Ctrl   at 16#20# range 0 .. 31;
      Host_Capabilities_Ext at 16#24# range 0 .. 31;
      BIOS_HO_Status_Ctrl   at 16#28# range 0 .. 31;
   end record;

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

   type Port_Task_File_Data_Type is record
      STS      : Interfaces.Unsigned_8;
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
      --  SPM  : Unsigned_4; This field is not used by AHCI.
      --  PMP  : Unsigned_4; This field is not used by AHCI.
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
      FIS_Based_Switching_Ctrl : Interfaces.Unsigned_32;
      Device_Sleep             : Interfaces.Unsigned_32;
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

   type Ports_Array is array (0 .. 31) of Port_Registers_Type
   with
      Pack;

   Instance : Generic_Host_Control_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Ahci_Controller_Ahci_Registers_Address);

   Ports : Ports_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address
        (Ahci_Controller_Ahci_Registers_Address + 16#100#);

end Ahci.Registers;
