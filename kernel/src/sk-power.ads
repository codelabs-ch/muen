--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with X86_64;

--D @Interface
--D This package provides facilities for rebooting or powering off the system.
package SK.Power
is

   --  Reboot system via Reset Control Register (port 16#cf9#). If the
   --  Power_Cycle argument is True, a full cold reset with system power cycle
   --  is performed.
   --
   --  See RST_CNT - Reset Control Register specification in the Intel ICH
   --  (I/O Controller Hub) specification for details.
   procedure Reboot (Power_Cycle : Boolean)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ Power_Cycle),
      No_Return;

   --  Perform ACPI shutdown.
   procedure Shutdown
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null),
      No_Return;

   --  Enable Intel Hardware-Controlled Performance States.
   procedure Enable_HWP
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

end SK.Power;
