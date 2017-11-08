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

private package Devices.Pciconf.Quirks
is

   --  Register device specific PCI config space quirks.
   procedure Register
     (Dev_State : in out Device_Type;
      Vendor    :        SK.Word16;
      Device    :        SK.Word16;
      Class     :        SK.Word32);

   --  Write given value to XUSB2PR register of device specified by SID.
   --  Enforces that only bits 14:0 are changed.
   procedure Write_XUSB2PR
     (SID   : Musinfo.SID_Type;
      Value : SK.Word16);

   --  Write given value to PSSEN register of device specified by SID. Enforces
   --  that only bits 5:0 are changed.
   procedure Write_PSSEN
     (SID   : Musinfo.SID_Type;
      Value : SK.Byte);

end Devices.Pciconf.Quirks;
