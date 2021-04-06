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

with Debuglog.Client;

with ITS.Log_Buffer;
with ITS.Memory;

procedure Isolation_Tests
is
begin
   Debuglog.Client.Init (Epoch => 1);

   ITS.Memory.Write_To_Read_Only_Region;
   ITS.Memory.Write_To_Unmapped_Region;

   --  Output log buffer.

   for I in ITS.Log_Buffer.Log_Entries_Range loop
      ITS.Log_Buffer.Print_Entry (ID => I);
   end loop;

   SK.CPU.Hlt;
end Isolation_Tests;
