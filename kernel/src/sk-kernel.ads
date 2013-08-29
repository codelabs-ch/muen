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

--# inherit
--#    X86_64,
--#    SK.Apic,
--#    SK.Interrupts,
--#    SK.GDT,
--#    SK.Scheduler,
--#    SK.System_State,
--#    SK.Subjects,
--#    SK.VMX,
--#    SK.MP,
--#    SK.IO_Apic,
--#    SK.CPU_Global,
--#    SK.CPU;
package SK.Kernel
is

   --  Kernel initialization.
   procedure Initialize (Subject_Registers : out SK.CPU_Registers_Type);
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Scheduler.State;
   --#    in     VMX.State;
   --#    in out Interrupts.State;
   --#    in out Subjects.State;
   --#    in out MP.Barrier;
   --#    in out IO_Apic.State;
   --#    in out CPU_Global.State;
   --#    in out X86_64.State;
   --# derives
   --#    Interrupts.State from *, X86_64.State &
   --#    MP.Barrier, IO_Apic.State, Subjects.State from
   --#       *,
   --#       X86_64.State,
   --#       Interrupts.State &
   --#    CPU_Global.State from
   --#       *,
   --#       X86_64.State,
   --#       Scheduler.State,
   --#       Interrupts.State &
   --#    X86_64.State from
   --#       *,
   --#       GDT.GDT_Pointer,
   --#       Subjects.State,
   --#       Scheduler.State,
   --#       VMX.State,
   --#       Interrupts.State,
   --#       X86_64.State &
   --#    Subject_Registers from
   --#       Subjects.State,
   --#       Scheduler.State,
   --#       Interrupts.State,
   --#       X86_64.State;
   pragma Export (C, Initialize, "sk_initialize");

end SK.Kernel;
