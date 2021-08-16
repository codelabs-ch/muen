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

with Skp.Scheduling;

with X86_64;

with SK.Constants;
with SK.CPU_Info;
with SK.FPU;
with SK.Interrupt_Tables;
with SK.IO_Apic;
with SK.MCE;
with SK.MP;
with SK.Scheduling_Info;
with SK.Subjects;
with SK.Subjects_Events;
with SK.Subjects_Interrupts;
with SK.Subjects_MSR_Store;
with SK.Tau0_Interface;
with SK.Timed_Events;
with SK.VMX;
with SK.Crash_Audit;

--D @Interface
--D This package implements the fixed-cyclic scheduler and additional, required
--D functionality. It contains the exit handler procedure which implements the
--D main processing loop.
package SK.Scheduler
with
   Abstract_State => State,
   Initializes    => State
is

   --  Returns the subject ID of the currently active scheduling group.
   function Get_Current_Subject_ID return Skp.Global_Subject_ID_Type
   with
      Global => (Input => (State, CPU_Info.CPU_ID));

   --  Init scheduler.
   procedure Init
   with
      Global =>
        (Input  => (CPU_Info.APIC_ID, CPU_Info.CPU_ID, CPU_Info.Is_BSP,
                    Interrupt_Tables.State, VMX.Exit_Address),
         In_Out => (State, Crash_Audit.State, FPU.State, MP.Barrier,
                    Scheduling_Info.State, Subjects.State,
                    Subjects_Events.State, Subjects_Interrupts.State,
                    Subjects_MSR_Store.State, Timed_Events.State,
                    VMX.VMCS_State, X86_64.State));

   --  Set VMX-preemption timer of the currently active VMCS to trigger at the
   --  current deadline. If the deadline has already passed the timer is set to
   --  zero.
   procedure Set_VMX_Exit_Timer
   with
      Global =>
        (Input  => (State, CPU_Info.APIC_ID, CPU_Info.CPU_ID),
         In_Out => (Crash_Audit.State, X86_64.State));

   --  Handle_Vmx_Exit could be private if spark/init.adb did not need access.

   --  VMX exit handler.
   procedure Handle_Vmx_Exit (Subject_Registers : in out SK.CPU_Registers_Type)
   with
      Global     =>
         (Input  => (CPU_Info.APIC_ID, CPU_Info.CPU_ID, CPU_Info.Is_BSP,
                     Interrupt_Tables.State, MCE.State, Tau0_Interface.State,
                     VMX.Exit_Address),
          In_Out => (State, Crash_Audit.State, FPU.State, IO_Apic.State,
                     MP.Barrier, Subjects.State, Scheduling_Info.State,
                     Subjects_Events.State, Subjects_Interrupts.State,
                     Subjects_MSR_Store.State, Timed_Events.State,
                     VMX.VMCS_State, X86_64.State)),
      Pre        => MCE.Valid_State,
      Export,
      Convention => C,
      Link_Name  => "handle_vmx_exit";

private

   --D @Interface
   --D Current major frame start time in CPU cycles. It is exclusively written
   --D by BSP and only read by APs. Data consistency is established via global
   --D synchronization barrier.
   Global_Current_Major_Start_Cycles : Word64 := 0
   with
      Linker_Section => Constants.Global_Data_Section,
      Part_Of        => State;

   --D @Interface
   --D ID of currently active major frame. It is exclusively written by BSP and
   --D only read by APs. Data consistency is established via global
   --D synchronization barrier.
   Global_Current_Major_Frame_ID : Skp.Scheduling.Major_Frame_Range
     := Skp.Scheduling.Major_Frame_Range'First
   with
      Linker_Section => Constants.Global_Data_Section,
      Part_Of        => State;

   --D @Text Section => SK.Scheduler.Current_Minor_Frame_ID
   --D ID of currently active minor frame.
   Current_Minor_Frame_ID : Skp.Scheduling.Minor_Frame_Range
     := Skp.Scheduling.Minor_Frame_Range'First
   with
      Part_Of => State;

   --D @Text Section => SK.Scheduler.Scheduling_Groups
   --D IDs of active subjects per scheduling group. The array stores the ID of
   --D the current active subject for each scheduling group.
   Scheduling_Groups : Skp.Scheduling.Scheduling_Group_Array
     := Skp.Scheduling.Scheduling_Groups
   with
      Part_Of => State;

end SK.Scheduler;
