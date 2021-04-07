--
--  Copyright (C) 2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Musinfo.Instance;

with Debuglog.Client;

with ITS.Events;
with ITS.IO_Ports;
with ITS.Memory;
with ITS.MSRs;
with ITS.Results;

procedure Isolation_Tests
is
begin
   Debuglog.Client.Init (Epoch => 1);

   if not Musinfo.Instance.Is_Valid then
      Debuglog.Client.Put_Line (Item => "Error: Sinfo data not valid");
      SK.CPU.Stop;
   end if;

   ITS.Memory.Write_To_Read_Only_Region;
   ITS.Memory.Write_To_Unmapped_Region;
   ITS.Memory.Read_From_Unmapped_Region;

   ITS.MSRs.Write_To_Read_Only_Register;
   ITS.MSRs.Write_To_Disallowed_Register;
   ITS.MSRs.Read_From_Disallowed_Register;

   ITS.IO_Ports.Write_To_Disallowed_IO_Port;
   ITS.IO_Ports.Read_From_Disallowed_IO_Port;

   ITS.Events.Trigger_Invalid_Event;

   ITS.Results.Report;

   SK.CPU.Hlt;
end Isolation_Tests;
