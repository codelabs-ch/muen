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

with Skp.IOMMU;

with X86_64;

with SK.CPU_Global;
with SK.CPU_Registry;
with SK.Interrupts;
with SK.IO_Apic;
with SK.MP;
with SK.Subjects;
with SK.Subjects_Sinfo;
with SK.Timers;
with SK.VTd;
with SK.VMX;
with SK.GDT;

package SK.Kernel
is

   --  Kernel initialization.
   procedure Initialize (Subject_Registers : out SK.CPU_Registers_Type)
   with
      Global =>
        (Input  => (CPU_Global.CPU_ID, GDT.GDT_Pointer, VMX.State),
         Output => CPU_Global.State,
         In_Out => (CPU_Registry.State, Interrupts.State, IO_Apic.State,
                    MP.Barrier, Subjects.State, Subjects_Sinfo.State,
                    Timers.State, VTd.State, Skp.IOMMU.State, X86_64.State)),
      Export,
      Convention => C,
      Link_Name  => "sk_initialize";

end SK.Kernel;
