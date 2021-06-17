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

with SK.CPU;

with Musinfo.Instance;

with Ahci;
with Ahci.Constants;
with Ahci.HBA;
with Ahci.Pciconf;

with Server;

with Debug_Ops;

procedure Ahci_Drv
is
begin
   pragma Debug (Debug_Ops.Init (Epoch => 1));
   pragma Debug (Debug_Ops.Put_Line (Item => "AHCI driver subject running"));
   pragma Debug (Debug_Ops.Print_PCI_Device_Info);
   pragma Debug (Debug_Ops.Print_PCI_Capabilities);

   if not Musinfo.Instance.Is_Valid then
      pragma Debug
        (Debug_Ops.Put_Line ("Sinfo invalid! Halting vCPU"));
      SK.CPU.Stop;
   end if;

   --  enable PCI Bus master, memory and io space. This might habe been already
   --  done by BIOS but coreboot usually leaves BM disabled.
   declare
      use type Interfaces.Unsigned_16;

      Pci_Cmd : constant Interfaces.Unsigned_16
         := Ahci.Pciconf.Instance.Header.Command;
   begin
      if (Pci_Cmd and 16#4#) /= 16#4# then
         Ahci.Pciconf.Instance.Header.Command := Pci_Cmd or 16#4#;
      end if;
   end;

   declare
      use type Interfaces.Unsigned_24;
      Class_Code : constant Interfaces.Unsigned_24
        := Ahci.Pciconf.Instance.Header.Class_Code;
   begin

      if Class_Code = Ahci.Constants.AHCI_Class_Code then
         pragma Debug
            (Debug_Ops.Put_Line (Item => "AHCI controller present"));
         Ahci.HBA.Reset;
         Ahci.HBA.Enable;
         pragma Debug
            (Debug_Ops.Put_Line (Item => "HBA enabled"));

         Server.Init;
         Server.Process;
         pragma Debug
            (Debug_Ops.Put_Line ("Server exited. Should not happen."));
      end if;
   end;

   SK.CPU.Stop;
end Ahci_Drv;
