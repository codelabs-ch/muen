--
--  Copyright (C) 2013, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with SK.FPU;
with SK.Interrupt_Tables;
with SK.MP;
with SK.Scheduling_Info;
with SK.Subjects;
with SK.Subjects_Events;
with SK.Subjects_Interrupts;
with SK.Subjects_MSR_Store;
with SK.Tau0_Interface;
with SK.Timed_Events;
with SK.VMX;

package SK.Scheduler
with
   Abstract_State => State,
   Initializes    => State
is

   --  Init scheduler.
   procedure Init
   with
      Global =>
        (Input  => (CPU_Global.CPU_ID, VMX.Exit_Address,
                    Interrupt_Tables.State),
         In_Out => (CPU_Global.State, FPU.State, MP.Barrier, Subjects.State,
                    Scheduling_Info.State, Subjects_Events.State,
                    Subjects_Interrupts.State, Subjects_MSR_Store.State,
                    Timed_Events.State, VMX.VMCS_State, X86_64.State));

   --  Set VMX-preemption timer of the currently active VMCS to trigger at the
   --  current deadline. If the deadline has alread passed the timer is set to
   --  zero.
   procedure Set_VMX_Exit_Timer
   with
      Global =>
        (Input  => (CPU_Global.State, CPU_Global.CPU_ID),
         In_Out => X86_64.State);

   --  Handle_Vmx_Exit could be private if spark/init.adb did not need access.

   --  VMX exit handler.
   procedure Handle_Vmx_Exit
     (Subject_Registers : in out SK.CPU_Registers_Type)
   with
      Global     =>
         (Input  => (Tau0_Interface.State, CPU_Global.CPU_ID,
                     Interrupt_Tables.State, VMX.Exit_Address),
          In_Out => (CPU_Global.State, FPU.State, MP.Barrier, Subjects.State,
                     Scheduling_Info.State, Subjects_Events.State,
                     Subjects_Interrupts.State, Subjects_MSR_Store.State,
                     Timed_Events.State, Skp.IOMMU.State, VMX.VMCS_State,
                     X86_64.State)),
      Export,
      Convention => C,
      Link_Name  => "handle_vmx_exit";

end SK.Scheduler;
