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

with Musinfo.Instance;

package ITS.Instructions
is

   --  Read Time-Stamp Counter.
   procedure Execute_RDTSC
   with
      Pre => Musinfo.Instance.Is_Valid;

   --  Disable VMX.
   procedure Execute_VMXOFF
   with
      Pre => Musinfo.Instance.Is_Valid;

   --  Write to Control-Register 0 (e.g. disable Paging).
   procedure Write_To_CR0
   with
      Pre => Musinfo.Instance.Is_Valid;

   --  Write to Control-Register 3 (i.e. change pagetables).
   procedure Write_To_CR3
   with
      Pre => Musinfo.Instance.Is_Valid;

end ITS.Instructions;
