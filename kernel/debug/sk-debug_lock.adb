--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.CPU_Info;
with SK.Constants;
with SK.Locks;

package body SK.Debug_Lock
is

   Global_Debug_Lock : Locks.Spin_Lock_Type
   with
      Linker_Section => Constants.Global_Data_Section;

   -------------------------------------------------------------------------

   procedure Acquire
   is
   begin
      Locks.Acquire (Lock => Global_Debug_Lock);
   end Acquire;

   -------------------------------------------------------------------------

   procedure Release
   is
   begin
      Locks.Release (Lock => Global_Debug_Lock);
   end Release;

   -------------------------------------------------------------------------

begin
   if CPU_Info.Is_BSP then

      --  The lock is a single instance shared by all CPUs. So it must only be
      --  initialized by a single CPU, i.e. BSP.

      Locks.Initialize (Lock => Global_Debug_Lock);
   end if;
end SK.Debug_Lock;
