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

package Ahci.Registers
is

   --  Serial ATA AHCI 1.3.1 Specification, section 3.1.
   type Generic_Host_Control_Type is record
      Host_Capabilities     : Interfaces.Unsigned_32;
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

end Ahci.Registers;
