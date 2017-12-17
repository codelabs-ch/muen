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

   type Generic_Host_Control_Type is record
      Host_Capabilities     : HBA_Caps_Type;
      Global_Host_Control   : Interfaces.Unsigned_32;
      Interrupt_Status      : Interfaces.Unsigned_32;
      Ports_Implemented     : Interfaces.Unsigned_32;
      Version               : Interfaces.Unsigned_32;
      CCC_Control           : Interfaces.Unsigned_32;
      CCC_Ports             : Interfaces.Unsigned_32;
      Enclosure_Mgmt_Loc    : Interfaces.Unsigned_32;
      Enclosure_Mgmt_Ctrl   : Interfaces.Unsigned_32;
      Host_Capabilities_Ext : Interfaces.Unsigned_32;
      BIOS_HO_Status_Ctrl   : Interfaces.Unsigned_32;
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

   Instance : Generic_Host_Control_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Ahci_Controller_Ahci_Registers_Address);

end Ahci.Registers;
