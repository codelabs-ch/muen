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

with SK;

with Types;

package Devices.Pciconf
with
   Abstract_State => State
is

   --  Init PCI config space emulation.
   procedure Init (Device_Base : SK.Word64)
   with
     Global => (Output => State);

   --  Mediate PCI config space access.
   procedure Mediate
     (Info   :     Types.EPTV_Info_Type;
      Action : out Types.Subject_Action_Type)
   with
      Global => (Input => State);

end Devices.Pciconf;
