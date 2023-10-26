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
with SK.MP;
with SK.Scheduling_Info;
with SK.Subjects;
with SK.Subjects_Events;
with SK.Subjects_Interrupts;
with SK.Tau0_Interface;
with SK.Timed_Events;
with SK.Crash_Audit;

private with SK.Atomics;

--D @Interface
--D This package implements the fixed-cyclic scheduler and additional, required
--D functionality.
package SK.Scheduler
with
   Abstract_State => (State,
                      (Group_Activity_Indicator
                       with External => (Async_Writers,
                                         Async_Readers))),
   Initializes    => (State,
                      Group_Activity_Indicator => CPU_Info.Is_BSP)
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
        (Input  => (CPU_Info.APIC_ID, CPU_Info.CPU_ID, CPU_Info.Is_BSP),
         In_Out => (State, Crash_Audit.State, MP.Barrier,
                    Scheduling_Info.State, X86_64.State));

   --  Set scheduler preemption timer to trigger at the current deadline. If the
   --  deadline has already passed the timer is set to zero.
   procedure Set_Preemption_Timer
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
        (Input  => (CPU_Info.CPU_ID, CPU_Info.Is_BSP, Tau0_Interface.State,
                    Subjects_Events.State, Subjects_Interrupts.State,
                    Timed_Events.State, X86_64.State),
         In_Out => (State, Group_Activity_Indicator, MP.Barrier,
                    Scheduling_Info.State, Subjects.State));

   --  Take scheduling decision for the current scheduling partition considering
   --  whether the specified subject requested to Sleep or to Yield its
   --  remaining time of the current minor frame. The next active scheduling
   --  group in the partition will be switched to or if all are inactive, the
   --  scheduling partition is put into sleep mode until the end of the minor
   --  frame. To make sure execution of the subject resumes after the
   --  yield/sleep instruction, the RIP of the subject is incremented when
   --  increment RIP is True.
   procedure Reschedule_Partition
     (Subject_ID    :     Skp.Global_Subject_ID_Type;
      Increment_RIP :     Boolean;
      Sleep         :     Boolean;
      Next_Subject  : out Skp.Global_Subject_ID_Type)
   with
      Global => (Input  => (CPU_Info.CPU_ID, Subjects_Events.State,
                            Subjects_Interrupts.State, Timed_Events.State,
                            X86_64.State),
                 In_Out => (Group_Activity_Indicator, State, Subjects.State));

   --  Indicate that activity has occurred that might change the status of the
   --  subject given by ID. Same CPU specifies whether the subject is running
   --  on this CPU core.
   procedure Indicate_Activity
     (Subject_ID : Skp.Global_Subject_ID_Type;
      Same_CPU   : Boolean)
   with
      Global  => (Input  => State,
                  In_Out => Group_Activity_Indicator),
      Depends => (Group_Activity_Indicator =>+ (Subject_ID, Same_CPU, State));

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

   pragma Compile_Time_Error
     (Atomics.Bit_Pos'First /= Byte (Policy.Scheduling_Group_Index_Range'First),
      "Atomic bitmap index and scheduling group index start differ");
   pragma Compile_Time_Error
     (Atomics.Bit_Pos'Last /= Byte (Policy.Scheduling_Group_Index_Range'Last),
      "Atomic bitmap index and scheduling group index end differ");

   type Scheduling_Group_Activity_Indicator_Array is
     array (Policy.Scheduling_Partition_Range) of Atomics.Atomic64_Type;

   --D @Interface
   --D Scheduling group activity indicator bitmap. Tracks the active scheduling
   --D groups of each scheduling partition. The bitmap position to scheduling
   --D group mapping is specified in the scheduling partition config of the
   --D policy.
   Global_Group_Activity_Indicator : Scheduling_Group_Activity_Indicator_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Linker_Section => Constants.Global_Data_Section,
      Part_Of        => Group_Activity_Indicator;

   --D @Interface
   --D Runtime scheduling partition information.
   type Scheduling_Partition_Type is record
       --D @Interface
       --D Index of currently active scheduling group of scheduling partition.
       --D The corresponding group ID is specified by the group map of the
       --D partition.
      Active_Group_Index : Policy.Scheduling_Group_Index_Range;
      --D @Interface
      --D ID of scheduling group with earliest timer deadline.
      Earliest_Timer     : Policy.Extended_Scheduling_Group_Range;
      --D @Interface
      --D Flag indicating whether the scheduling partition is in the sleep
      --D state, i.e. all active subjects of all scheduling groups of the
      --D scheduling partition are asleep.
      Sleeping           : Boolean;
   end record;

   Null_Scheduling_Partition : constant Scheduling_Partition_Type
     := (Active_Group_Index => Policy.Scheduling_Group_Index_Range'First,
         Earliest_Timer     => Policy.No_Group,
         Sleeping           => False);

   use type Policy.Scheduling_Group_Index_Range;

   type Scheduling_Partitions_Array is array
     (Policy.Scheduling_Partition_Range) of Scheduling_Partition_Type
   with
      Dynamic_Predicate =>
         (for all I in Scheduling_Partitions_Array'Range =>
            Scheduling_Partitions_Array (I).Active_Group_Index <=
                  Policy.Scheduling_Partition_Config (I).Last_Group_Index);

   --D @Text Section => SK.Scheduler.Scheduling_Partitions
   --D Scheduling partitions management information. The array stores the
   --D currently active group, the earliest timer deadline and the sleeping
   --D state of each scheduling partition.
   Scheduling_Partitions : Scheduling_Partitions_Array
     := (others => Null_Scheduling_Partition)
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
   --D Scheduling groups management information. The array stores the currently
   --D active subject, position in the timer list as well as the timeout
   --D deadline of each scheduling group.
   Scheduling_Groups : Scheduling_Group_Array
     := (others => Null_Scheduling_Group)
   with
      Part_Of => State;

end SK.Scheduler;
