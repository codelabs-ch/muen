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

package Ahci.Pciconf
is

   package Spec renames Ahci_Drv_Component.Devices;

   type Header_Type is record
      Vendor_ID               : Interfaces.Unsigned_16;
      Device_ID               : Interfaces.Unsigned_16;
      Command                 : Interfaces.Unsigned_16;
      Status                  : Interfaces.Unsigned_16;
      Revision_ID             : Interfaces.Unsigned_8;
      Class_Code              : Interfaces.Unsigned_24;
      Cache_Line_Size         : Interfaces.Unsigned_8;
      Master_Latency_Timer    : Interfaces.Unsigned_8;
      Header_Type             : Interfaces.Unsigned_8;
      Buitin_Self_Test        : Interfaces.Unsigned_8;
      Base_Address_Register_0 : Interfaces.Unsigned_32;
      Base_Address_Register_1 : Interfaces.Unsigned_32;
      Base_Address_Register_2 : Interfaces.Unsigned_32;
      Base_Address_Register_3 : Interfaces.Unsigned_32;
      Base_Address_Register_4 : Interfaces.Unsigned_32;
      Base_Address_Register_5 : Interfaces.Unsigned_32;
      Cardbus_CIS_Pointer     : Interfaces.Unsigned_32;
      Subsystem_Vendor_ID     : Interfaces.Unsigned_16;
      Subsystem_ID            : Interfaces.Unsigned_16;
      Expansion_ROM_Base_Addr : Interfaces.Unsigned_32;
      Capabilities_Pointer    : Interfaces.Unsigned_8;
      Reserved_1              : Interfaces.Unsigned_24;
      Reserved_2              : Interfaces.Unsigned_32;
      Interrupt_Line          : Interfaces.Unsigned_8;
      Interrupt_Pin           : Interfaces.Unsigned_8;
      Min_Grant               : Interfaces.Unsigned_8;
      Max_Latency             : Interfaces.Unsigned_8;
   end record
   with
      Size => 64 * 8;

   for Header_Type use record
      Vendor_ID               at 16#00# range  0 .. 15;
      Device_ID               at 16#00# range 16 .. 31;
      Command                 at 16#04# range  0 .. 15;
      Status                  at 16#04# range 16 .. 31;
      Revision_ID             at 16#08# range  0 ..  7;
      Class_Code              at 16#08# range  8 .. 31;
      Cache_Line_Size         at 16#0c# range  0 ..  7;
      Master_Latency_Timer    at 16#0c# range  8 .. 15;
      Header_Type             at 16#0c# range 16 .. 23;
      Buitin_Self_Test        at 16#0c# range 24 .. 31;
      Base_Address_Register_0 at 16#10# range  0 .. 31;
      Base_Address_Register_1 at 16#14# range  0 .. 31;
      Base_Address_Register_2 at 16#18# range  0 .. 31;
      Base_Address_Register_3 at 16#1c# range  0 .. 31;
      Base_Address_Register_4 at 16#20# range  0 .. 31;
      Base_Address_Register_5 at 16#24# range  0 .. 31;
      Cardbus_CIS_Pointer     at 16#28# range  0 .. 31;
      Subsystem_Vendor_ID     at 16#2c# range  0 .. 15;
      Subsystem_ID            at 16#2c# range 16 .. 31;
      Expansion_ROM_Base_Addr at 16#30# range  0 .. 31;
      Capabilities_Pointer    at 16#34# range  0 ..  7;
      Reserved_1              at 16#34# range  8 .. 31;
      Reserved_2              at 16#38# range  0 .. 31;
      Interrupt_Line          at 16#3c# range  0 ..  7;
      Interrupt_Pin           at 16#3c# range  8 .. 15;
      Min_Grant               at 16#3c# range 16 .. 23;
      Max_Latency             at 16#3c# range 24 .. 31;
   end record;

   subtype Capability_Range is Interfaces.Unsigned_8 range 16#40# .. 16#ff#;

   type Caps_Array is array (Capability_Range) of Interfaces.Unsigned_8
   with
      Pack;

   Header : Header_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Spec.Ahci_Controller_Mmconf_Address);

end Ahci.Pciconf;
