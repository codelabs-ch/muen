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

with SK.CPU_Global;
with SK.Events;
with SK.FPU;
with SK.Interrupts;
with SK.MP;
with SK.Subjects;
with SK.Subjects_Sinfo;
with SK.Tau0_Interface;
with SK.Timers;
with SK.VTd;
with X86_64;

package SK.Scheduler
with
   Abstract_State => State,
   Initializes    => State
is

   --  Init scheduler.
   procedure Init
   with
      Global  =>
        (Input  => Interrupts.State,
         In_Out => (CPU_Global.State, MP.Barrier, State, Subjects.State,
                    Subjects_Sinfo.State, Timers.State, X86_64.State)),
      Depends =>
        ((CPU_Global.State,
          MP.Barrier,
          Subjects.State,
          Timers.State)       =>+ null,
         Subjects_Sinfo.State =>+ (CPU_Global.State, X86_64.State),
         State                =>+ X86_64.State,
         X86_64.State         =>+ (CPU_Global.State, Interrupts.State));

   --  Set VMX-preemption timer of the currently active VMCS to trigger at the
   --  current deadline. If the deadline has alread passed the timer is set to
   --  zero.
   procedure Set_VMX_Exit_Timer
   with
      Global  =>
        (Input  => (CPU_Global.State, State),
         In_Out => X86_64.State),
      Depends => (X86_64.State =>+ (CPU_Global.State, State));

   --  Handle_Vmx_Exit could be private if spark/init.adb did not need access.

   --  VMX exit handler.
   procedure Handle_Vmx_Exit
     (Subject_Registers : in out SK.CPU_Registers_Type)
   with
      Global     =>
        (Input  => Tau0_Interface.State,
         In_Out => (CPU_Global.State, Events.State, FPU.State, MP.Barrier,
                    State, Subjects.State, Subjects_Sinfo.State, Timers.State,
                    VTd.State, X86_64.State)),
      Depends    =>
       (CPU_Global.State     =>+ (Subject_Registers, Tau0_Interface.State,
                                  X86_64.State),
        Events.State         =>+ (CPU_Global.State, Subjects.State,
                                  Subject_Registers, Tau0_Interface.State,
                                  Timers.State, X86_64.State),
        Subject_Registers    =>+ (CPU_Global.State, Subjects.State,
                                  Subject_Registers, Tau0_Interface.State,
                                  X86_64.State),
        (MP.Barrier,
         Timers.State)       =>+ (CPU_Global.State, Tau0_Interface.State,
                                  X86_64.State),
        (State,
         FPU.State)          =>+ (CPU_Global.State, X86_64.State),
        (Subjects.State,
         VTd.State)          =>+ (CPU_Global.State, Subjects.State,
                                  Subject_Registers, X86_64.State),
        Subjects_Sinfo.State =>+ (CPU_Global.State, X86_64.State, State,
                                  Subject_Registers, Tau0_Interface.State),
        X86_64.State         =>+ (CPU_Global.State, Events.State, State,
                                  FPU.State, Subjects.State, Subject_Registers,
                                  Timers.State, Tau0_Interface.State)),
      Export,
      Convention => C,
      Link_Name  => "handle_vmx_exit";

end SK.Scheduler;
