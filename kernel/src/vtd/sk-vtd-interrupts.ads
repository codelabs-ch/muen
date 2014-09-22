--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.IO_Apic;
with SK.CPU_Registry;

package SK.VTd.Interrupts
is

   --  Setup I/O APIC IRQ routing.
   procedure Setup_IRQ_Routing (VTd_Enabled : Boolean)
   with
      Global  => (Input => CPU_Registry.State, In_Out => IO_Apic.State),
      Depends => (IO_Apic.State =>+ (CPU_Registry.State, VTd_Enabled));

end SK.VTd.Interrupts;
