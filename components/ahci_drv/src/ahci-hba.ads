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

package Ahci.HBA
is

   use Ahci_Drv_Component.Devices;

   --  Enable the Host Bus Adapter.
   procedure Enable;

   --  Reset the HBA.
   procedure Reset;

   --  Serial ATA AHCI 1.3.1 Specification, section 3.1.

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

   Generic_Host_Control_Size : constant := 16#2c# * 8;

   type Generic_Host_Control_Type is record
      Host_Capabilities     : HBA_Caps_Type;
      Global_Host_Control   : Global_HBA_Control_Type;
      Interrupt_Status      : Bit_Array (0 .. 31);
      Ports_Implemented     : Bit_Array (0 .. 31);
      Version               : Version_Type;
      CCC_Control           : CCC_Control_Type;
      CCC_Ports             : Bit_Array (0 .. 31);
      Enclosure_Mgmt_Loc    : EM_Location_Type;
      Enclosure_Mgmt_Ctrl   : EM_Control_Type;
      Host_Capabilities_Ext : HBA_Caps_Ext_Type;
      BIOS_HO_Status_Ctrl   : BIOS_HO_Status_Type;
   end record
   with
      Object_Size => Generic_Host_Control_Size,
      Size        => Generic_Host_Control_Size;

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

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Instance : Generic_Host_Control_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Ahci_Controller_Ahci_Registers_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

end Ahci.HBA;
