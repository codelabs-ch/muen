--
--  Copyright (C) 2013-2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp.Interrupts;

with SK.Apic;

pragma Elaborate_All (SK.Apic, X86_64);
pragma Unreferenced (X86_64);

package SK.CPU_Info
is

   use type Skp.CPU_Range;

   --  ID of the local CPU.
   CPU_ID : constant Skp.CPU_Range
   with
      Import,
      Size       => 8,
      Convention => C,
      Link_Name  => "cpu_id";

   APIC_ID : constant Skp.Interrupts.APIC_ID_Range
     := Skp.Interrupts.APIC_ID_Range (2 * CPU_ID);

   Is_BSP : constant Boolean := Apic.Is_BSP;

end SK.CPU_Info;
