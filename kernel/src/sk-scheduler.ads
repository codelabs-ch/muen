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

with Skp.Scheduling;
with Skp.Subjects;

use type Skp.CPU_Range;
use type Skp.Dst_Vector_Range;
use type Skp.Scheduling.Minor_Frame_Range;
use type Skp.Subjects.Trap_Entry_Type;
use type Skp.Subjects.Event_Entry_Type;
use type Skp.Subjects.Profile_Kind;

--# inherit
--#    Skp.Kernel,
--#    Skp.Subjects,
--#    Skp.Scheduling,
--#    Skp.Interrupts,
--#    X86_64,
--#    SK.Constants,
--#    SK.CPU,
--#    SK.CPU_Global,
--#    SK.GDT,
--#    SK.Interrupts,
--#    SK.Subjects,
--#    SK.VMX,
--#    SK.Apic,
--#    SK.MP,
--#    SK.Events;
package SK.Scheduler
--# own
--#    State;
--# initializes
--#    State;
is

   --  Init scheduler.
   procedure Init;
   --# global
   --#    in     State;
   --#    in     Interrupts.State;
   --#    in     GDT.GDT_Pointer;
   --#    in     VMX.State;
   --#    in out Subjects.State;
   --#    in out CPU_Global.State;
   --#    in out X86_64.State;
   --# derives
   --#    Subjects.State   from * &
   --#    CPU_Global.State from *, State &
   --#    X86_64.State from
   --#       *,
   --#       State,
   --#       Interrupts.State,
   --#       GDT.GDT_Pointer,
   --#       VMX.State,
   --#       CPU_Global.State;

private

   --  VMX exit handler.
   procedure Handle_Vmx_Exit
     (Subject_Registers : in out SK.CPU_Registers_Type);
   --# global
   --#    in out CPU_Global.State;
   --#    in out State;
   --#    in out MP.Barrier;
   --#    in out Subjects.State;
   --#    in out Events.State;
   --#    in out X86_64.State;
   --# derives
   --#    CPU_Global.State, Subjects.State from
   --#       *,
   --#       State,
   --#       Subject_Registers,
   --#       CPU_Global.State,
   --#       X86_64.State &
   --#    Subject_Registers, Events.State from
   --#       *,
   --#       State,
   --#       Subject_Registers,
   --#       CPU_Global.State,
   --#       Subjects.State,
   --#       X86_64.State &
   --#    MP.Barrier, State from
   --#       *,
   --#       State,
   --#       CPU_Global.State,
   --#       X86_64.State &
   --#    X86_64.State from
   --#       *,
   --#       State,
   --#       Subject_Registers,
   --#       CPU_Global.State,
   --#       Subjects.State,
   --#       Events.State;
   pragma Export (C, Handle_Vmx_Exit, "handle_vmx_exit");

end SK.Scheduler;
