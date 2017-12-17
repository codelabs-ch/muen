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

with SK.CPU;

with Debug_Ops;

procedure Ahci_Drv
is
begin
   pragma Debug (Debug_Ops.Init (Epoch => 1));
   pragma Debug (Debug_Ops.Put_Line (Item => "AHCI driver subject running"));
   pragma Debug (Debug_Ops.Print_PCI_Device_Info);
   pragma Debug (Debug_Ops.Print_PCI_Capabilities);
   pragma Debug (Debug_Ops.Print_HBA_Memory_Regs);
   SK.CPU.Stop;
end Ahci_Drv;
