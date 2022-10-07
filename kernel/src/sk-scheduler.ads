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
--D functionality.
package SK.Scheduler
with
   Abstract_State => State,
   Initializes    => State
is

   --  Returns the subject ID of the currently active scheduling group.
   function Get_Current_Subject_ID return Skp.Global_Subject_ID_Type
   with
      Global => (Input => (State, CPU_Info.CPU_ID));

   --  Set the currently active subject ID of the current scheduling group to
   --  the given value.
   procedure Set_Current_Subject_ID (Subject_ID : Skp.Global_Subject_ID_Type)
   with
      Global  => (Input  => CPU_Info.CPU_ID,
                  In_Out => Scheduler.State);

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

   --  Update scheduling information. If the end of the current major frame is
   --  reached the major frame start time is updated by adding the period of
   --  the just expired major frame to the current start value. Additionally,
   --  the minor frame index is reset and the major frame is switched to the
   --  one set by Tau0.
   --  On regular minor frame switches the minor frame index is incremented by
   --  one.
   --  The ID of the next subject to schedule is returned to the caller.
   procedure Update_Scheduling_Info
     (Next_Subject : out Skp.Global_Subject_ID_Type)
   with
      Global =>
        (Input  => (CPU_Info.CPU_ID, CPU_Info.Is_BSP,
                    Tau0_Interface.State),
         In_Out => (State, MP.Barrier, Scheduling_Info.State));

private

   package Policy renames Skp.Scheduling;

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
   Global_Current_Major_Frame_ID : Policy.Major_Frame_Range
     := Policy.Major_Frame_Range'First
   with
      Linker_Section => Constants.Global_Data_Section,
      Part_Of        => State;

   --D @Text Section => SK.Scheduler.Current_Minor_Frame_ID
   --D ID of currently active minor frame.
   Current_Minor_Frame_ID : Policy.Minor_Frame_Range
     := Policy.Minor_Frame_Range'First
   with
      Part_Of => State;

   --D @Interface
   --D Runtime scheduling group information.
   type Scheduling_Group_Type is record
      --D @Interface
      --D ID of currently active subject of scheduling group.
      Active_Subject : Skp.Global_Subject_ID_Type;
      --D @Interface
      --D ID of scheduling group with next (later) expiring timer relative to
      --D this group's timer.
      Next_Timer     : Policy.Extended_Scheduling_Group_Range;
      --D @Interface
      --D ID of scheduling group with previous (earlier) expiring timer relative
      --D to this group's timer.
      Prev_Timer     : Policy.Extended_Scheduling_Group_Range;
      --D @Interface
      --D Timeout value of timed event of this scheduling group's active
      --D subject. Corresponds to the TSC_Trigger_Value on the timed event page
      --D of the active subject.
      Timeout        : Word64;
   end record;

   Null_Scheduling_Group : constant Scheduling_Group_Type
     := (Active_Subject => Skp.Global_Subject_ID_Type'First,
         Next_Timer     => Policy.No_Group,
         Prev_Timer     => Policy.No_Group,
         Timeout        => Word64'Last);

   type Scheduling_Group_Array is array
     (Policy.Scheduling_Group_Range) of Scheduling_Group_Type;

   --D @Text Section => SK.Scheduler.Scheduling_Groups
   --D IDs of active subjects per scheduling group. The array stores the ID of
   --D the current active subject for each scheduling group.
   Scheduling_Groups : Scheduling_Group_Array
     := (others => Null_Scheduling_Group)
   with
      Part_Of => State;

end SK.Scheduler;
