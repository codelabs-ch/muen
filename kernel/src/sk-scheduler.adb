--
--  Copyright (C) 2013-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp.Subjects;

with SK.CPU;
with SK.Kernel;

package body SK.Scheduler
with
   Refined_State =>
     (State => (Current_Minor_Frame_ID, Global_Current_Major_Frame_ID,
                Global_Current_Major_Start_Cycles, Scheduling_Partitions,
                Scheduling_Groups),
      Group_Activity_Indicator => Global_Group_Activity_Indicator)
is

   use type Policy.Extended_Scheduling_Group_Range;

   -------------------------------------------------------------------------

   --  Returns True if the subject with the given ID is active and not sleeping.
   --D @Section Id => impl_scheduling_is_active, Label => Active Subject, Parent => impl_scheduling, Priority => 10
   --D @Text Section => impl_scheduling_is_active
   --D A subject is considered active if either of the following conditions is
   --D True:
   --D @UL Id => impl_scheduling_is_active_conds, Section => impl_scheduling_is_active, Priority => 0
   function Is_Active (Subject_ID : Skp.Global_Subject_ID_Type) return Boolean
   with
      Global => (Input => (Subjects.State, Subjects_Events.State,
                           Subjects_Interrupts.State, Timed_Events.State,
                           X86_64.State)),
      Volatile_Function
   is
      --D @Item List => impl_scheduling_is_active_conds
      --D The subject has a pending event.
      Pending_Events     : constant Boolean
        := Subjects_Events.Has_Pending_Event (Subject => Subject_ID);
      --D @Item List => impl_scheduling_is_active_conds
      --D The subject has a pending interrupt.
      Pending_Interrupts : constant Boolean
        := Subjects_Interrupts.Has_Pending_Interrupt (Subject => Subject_ID);
      --D @Item List => impl_scheduling_is_active_conds
      --D The subject timed event has expired.
      Expired_Timer      : constant Boolean
        := Timed_Events.Has_Expired (Subject => Subject_ID);
   begin
      return Pending_Events or Pending_Interrupts or Expired_Timer
      --D @Item List => impl_scheduling_is_active_conds
      --D The subject is running/has the running flag set, i.e. is not in the
      --D sleep state.
        or Subjects.Is_Running (ID => Subject_ID);
   end Is_Active;

   -------------------------------------------------------------------------

   --  Returns the ID of the currently active scheduling partition which is
   --  specified by the current minor frame in the scheduling plan of the
   --  executing CPU.
   function Current_Scheduling_Partition_ID
     return Policy.Scheduling_Partition_Range
   is (Policy.Scheduling_Plans (CPU_Info.CPU_ID)
       (Global_Current_Major_Frame_ID).Minor_Frames
       (Current_Minor_Frame_ID).Partition_ID)
   with
      Global => (Input => (CPU_Info.CPU_ID, Global_Current_Major_Frame_ID,
                           Current_Minor_Frame_ID));

   -------------------------------------------------------------------------

   --  Returns the ID of the currently active scheduling group which is
   --  specified by the current scheduling partition.
   function Current_Scheduling_Group_ID return Policy.Scheduling_Group_Range
   with
      Global => (Input => (CPU_Info.CPU_ID, Global_Current_Major_Frame_ID,
                           Current_Minor_Frame_ID, Scheduling_Partitions))
   is
      Partition_ID : constant Policy.Scheduling_Partition_Range
        := Current_Scheduling_Partition_ID;
      Group_Index  : constant Policy.Scheduling_Group_Index_Range
        := Scheduling_Partitions (Partition_ID).Active_Group_Index;
   begin
      return Policy.Scheduling_Partition_Config
        (Partition_ID).Groups (Group_Index);
   end Current_Scheduling_Group_ID;

   -------------------------------------------------------------------------

   --  Returns True if the given scheduling group is in the timer list of the
   --  specified scheduling partition.
   function Is_Group_In_Timer_List
     (Partition_ID : Policy.Scheduling_Partition_Range;
      Group_ID     : Policy.Scheduling_Group_Range)
      return Boolean
   with
      Global => (Input => (Scheduling_Groups, Scheduling_Partitions))
   is
      List_Head : constant Policy.Extended_Scheduling_Group_Range
        := Scheduling_Partitions (Partition_ID).Earliest_Timer;
   begin
      return List_Head = Group_ID or else
        Scheduling_Groups (Group_ID).Prev_Timer /= Policy.No_Group;
   end Is_Group_In_Timer_List;

   -------------------------------------------------------------------------

   --D @Section Id => impl_scheduling_activate_group, Label => Scheduling Group Activation, Parent => impl_scheduling, Priority => 20
   --D @Text Section => impl_scheduling_activate_group
   --D Set the global activity indicator of the scheduling group identified by
   --D partition ID and scheduling group index. Also remove the now active
   --D scheduling group from the sorted timer list of the scheduling partition.
   procedure Activate_Group
     (Partition_ID : Policy.Scheduling_Partition_Range;
      Group_ID     : Policy.Scheduling_Group_Range)
   with
      Global => (In_Out => (Scheduling_Groups, Scheduling_Partitions,
                            Global_Group_Activity_Indicator)),
      Post =>
         Scheduling_Groups (Group_ID).Prev_Timer = Policy.No_Group and
         Scheduling_Groups (Group_ID).Next_Timer = Policy.No_Group
   is
      Group_Index : constant Policy.Scheduling_Group_Index_Range
        := Policy.Get_Scheduling_Group_Index (Group_ID => Group_ID);
      Prev_Group  : constant Policy.Extended_Scheduling_Group_Range
        := Scheduling_Groups (Group_ID).Prev_Timer;
      Next_Group  : constant Policy.Extended_Scheduling_Group_Range
        := Scheduling_Groups (Group_ID).Next_Timer;
   begin

      --  Remove group by linking previous to next group.

      if Prev_Group = Policy.No_Group then
         Scheduling_Partitions (Partition_ID).Earliest_Timer := Next_Group;
      else
         Scheduling_Groups (Prev_Group).Next_Timer := Next_Group;
      end if;

      if Next_Group /= Policy.No_Group then
         Scheduling_Groups (Next_Group).Prev_Timer := Prev_Group;
      end if;

      --  Reset prev/next since this group is no longer in the timer list.

      Scheduling_Groups (Group_ID).Prev_Timer := Policy.No_Group;
      Scheduling_Groups (Group_ID).Next_Timer := Policy.No_Group;

      Atomics.Set
        (Atomic => Global_Group_Activity_Indicator (Partition_ID),
         Bit    => Atomics.Bit_Pos (Group_Index));
   end Activate_Group;

   -------------------------------------------------------------------------

   --D @Section Id => impl_scheduling_deactivate_group, Label => Scheduling Group Deactivation, Parent => impl_scheduling, Priority => 21
   --D @Text Section => impl_scheduling_deactivate_group
   --D Clear the global activity indicator of the scheduling group identified by
   --D partition ID and scheduling group index. Also insert the timed event of
   --D the active subject of the scheduling group into the sorted timer list of
   --D the scheduling partition if it is not already in the list.
   procedure Deactivate_Group
     (Partition_ID : Policy.Scheduling_Partition_Range;
      Group_Index  : Policy.Scheduling_Group_Index_Range;
      Subject_ID   : Skp.Global_Subject_ID_Type)
   with
      Global => (Input  => Timed_Events.State,
                 In_Out => (Scheduling_Groups, Scheduling_Partitions,
                            Global_Group_Activity_Indicator)),
      Annotate => (GNATprove, Terminating)
   is
      --  Insert group with given ID and deadline into timed event list between
      --  Prev and Next.
      procedure Insert
        (Partition : Policy.Scheduling_Partition_Range;
         Group     : Policy.Scheduling_Group_Range;
         Deadline  : Word64;
         Prev      : Policy.Extended_Scheduling_Group_Range;
         Next      : Policy.Extended_Scheduling_Group_Range)
      is
      begin
         Scheduling_Groups (Group).Timeout    := Deadline;
         Scheduling_Groups (Group).Prev_Timer := Prev;
         Scheduling_Groups (Group).Next_Timer := Next;

         if Prev = Policy.No_Group then
            Scheduling_Partitions (Partition).Earliest_Timer := Group;
         else
            Scheduling_Groups (Prev).Next_Timer := Group;
         end if;

         if Next /= Policy.No_Group then
            Scheduling_Groups (Next).Prev_Timer := Group;
         end if;
      end Insert;

      ----------------------------------------------------------------------

      Group_Deadline : constant Word64
        := Timed_Events.Get_Trigger_Value (Subject => Subject_ID);
      Group_ID       : constant Policy.Scheduling_Group_Range
        := Policy.Get_Scheduling_Group_ID (Subject_ID => Subject_ID);
      Prev_Group     : Policy.Extended_Scheduling_Group_Range
        := Policy.No_Group;
      Next_Group     : Policy.Extended_Scheduling_Group_Range
        := Scheduling_Partitions (Partition_ID).Earliest_Timer;
   begin
      if not Is_Group_In_Timer_List (Partition_ID => Partition_ID,
                                     Group_ID     => Group_ID)
      then
         Insert_Loop :
         loop
            if Next_Group = Policy.No_Group
              or else Group_Deadline < Scheduling_Groups (Next_Group).Timeout
            then
               Insert (Partition => Partition_ID,
                       Group     => Group_ID,
                       Deadline  => Group_Deadline,
                       Prev      => Prev_Group,
                       Next      => Next_Group);
               exit Insert_Loop;
            end if;
            Prev_Group := Next_Group;
            Next_Group := Scheduling_Groups (Next_Group).Next_Timer;
         end loop Insert_Loop;
      end if;

      Atomics.Clear
        (Atomic => Global_Group_Activity_Indicator (Partition_ID),
         Bit    => Atomics.Bit_Pos (Group_Index));
   end Deactivate_Group;

   -------------------------------------------------------------------------

   --  Returns the index of the successor scheduling group of the specified
   --  group in the given partition.
   function Successor_Group
     (Partition_ID : Policy.Scheduling_Partition_Range;
      Group_Index  : Policy.Scheduling_Group_Index_Range)
      return Policy.Scheduling_Group_Index_Range
   with
      Global  => null,
      Depends => (Successor_Group'Result => (Group_Index, Partition_ID)),
      Post    =>
         Successor_Group'Result <= Policy.Scheduling_Partition_Config
           (Partition_ID).Last_Group_Index
   is
      Next_Group_Index : Policy.Scheduling_Group_Index_Range;
   begin
      if Group_Index < Policy.Scheduling_Partition_Config
        (Partition_ID).Last_Group_Index
      then
         Next_Group_Index := Group_Index + 1;
      else
         Next_Group_Index := Policy.Scheduling_Group_Index_Range'First;
      end if;
      return Next_Group_Index;
   end Successor_Group;

   -------------------------------------------------------------------------

   --D @Section Id => impl_scheduling_find_next_active_sg, Label => Finding the next active scheduling group, Parent => impl_scheduling, Priority => 11
   --D @Text Section => impl_scheduling_find_next_active_sg
   --D Find next active scheduling group for the scheduling partition specified
   --D by ID. \verb!No_Group! is returned if no scheduling group is active in
   --D the given partition.
   --D @OL Id => impl_scheduling_find_next_active_sg_steps, Section => impl_scheduling_find_next_active_sg, Priority => 0
   procedure Find_Next_Active_Scheduling_Group
     (Partition_ID     :     Policy.Scheduling_Partition_Range;
      Next_Group       : out Policy.Extended_Scheduling_Group_Range;
      Next_Group_Index : out Policy.Scheduling_Group_Index_Range)
   with
      Global  => (Input  => (Subjects.State,
                             Subjects_Events.State,
                             Subjects_Interrupts.State,
                             Timed_Events.State,
                             X86_64.State),
                  In_Out => (Global_Group_Activity_Indicator,
                             Scheduling_Groups,
                             Scheduling_Partitions)),
      Post =>
         (if Next_Group /= Policy.No_Group then
            Next_Group_Index <= Policy.Scheduling_Partition_Config
              (Partition_ID).Last_Group_Index),
      Annotate => (GNATprove, Terminating)
   is
      Current_SG_Index : constant Policy.Scheduling_Group_Index_Range
        := Scheduling_Partitions (Partition_ID).Active_Group_Index;
      Indicated_As_Active, Subject_Is_Active : Boolean;
   begin
      Next_Group := Policy.No_Group;
      Next_Group_Index := Current_SG_Index;
      loop
         --D @Item List => impl_scheduling_find_next_active_sg_steps
         --D Loop over all scheduling groups (maximum is 64) of the partition
         --D starting with the successor of the currently active group.
         Next_Group_Index := Successor_Group
           (Partition_ID => Partition_ID,
            Group_Index  => Next_Group_Index);

         --D @Item List => impl_scheduling_find_next_active_sg_steps
         --D Atomically examine the group activity indicator of the next group.
         Indicated_As_Active := Atomics.Bit_Test
           (Atomic => Global_Group_Activity_Indicator (Partition_ID),
            Bit    => Byte (Next_Group_Index));
         if Indicated_As_Active then
            --D @Item List => impl_scheduling_find_next_active_sg_steps
            --D If the group is indicated as active, evaluate whether the
            --D current subject of this scheduling group is actually active.
            Next_Group := Policy.Get_Scheduling_Group_ID
              (Partition_ID => Partition_ID,
               Group_Index  => Next_Group_Index);
            Subject_Is_Active := Is_Active
              (Subject_ID => Scheduling_Groups (Next_Group).Active_Subject);
            if Subject_Is_Active then
               --D @Item List => impl_scheduling_find_next_active_sg_steps
               --D If the subject is indeed active, the next active group has
               --D been found and is returned.
               return;
            else
               --D @Item List => impl_scheduling_find_next_active_sg_steps
               --D If the subject is not active, deactivate the group which also
               --D clears the global activity indicator of the group, see
               --D \ref{impl_scheduling_deactivate_group}.
               Deactivate_Group
                 (Partition_ID => Partition_ID,
                  Group_Index  => Next_Group_Index,
                  Subject_ID   => Scheduling_Groups
                    (Next_Group).Active_Subject);
               Next_Group := Skp.Scheduling.No_Group;
            end if;
         end if;

         --D @Item List => impl_scheduling_find_next_active_sg_steps
         --D Stop search if we end up back at the current scheduling group
         --D since this means there is no other active group in this partition.
         exit when Next_Group_Index = Current_SG_Index;
      end loop;
   end Find_Next_Active_Scheduling_Group;

   -------------------------------------------------------------------------

   --D @Section Id => impl_scheduling_indicate_activity, Label => Indicate Subject Activity, Parent => impl_scheduling, Priority => 30
   --D @Text Section => impl_scheduling_indicate_activity
   --D Indicate that activity for a given subject has occurred by atomically
   --D setting the global scheduling group activity indicator of the associated
   --D scheduling group of the target subject.
   procedure Indicate_Activity
     (Subject_ID : Skp.Global_Subject_ID_Type;
      Same_CPU   : Boolean)
   with
      Refined_Global  =>
         (Input  => Scheduling_Groups,
          In_Out => Global_Group_Activity_Indicator),
      Refined_Depends =>
         (Global_Group_Activity_Indicator =>+ (Subject_ID, Same_CPU,
                                               Scheduling_Groups))
   is
      Partition_ID : constant Policy.Scheduling_Partition_Range
        := Policy.Get_Scheduling_Partition_ID (Subject_ID => Subject_ID);
      Group_ID     : constant Policy.Scheduling_Group_Range
        := Policy.Get_Scheduling_Group_ID (Subject_ID => Subject_ID);
      Group_Index  : constant Policy.Scheduling_Group_Index_Range
        := Policy.Get_Scheduling_Group_Index (Group_ID);
   begin
      if Same_CPU then

         --D @Text Section => impl_scheduling_indicate_activity
         --D If the subject is running on the same CPU, only indicate activity
         --D if the target subject is the active subject of its scheduling group.
         if Subject_ID = Scheduling_Groups (Group_ID).Active_Subject then
            Atomics.Set
              (Atomic => Global_Group_Activity_Indicator (Partition_ID),
               Bit    => Byte (Group_Index));
         end if;
      else

         --D @Text Section => impl_scheduling_indicate_activity
         --D Unconditionally indicate activity for target subject running on
         --D different CPU cores.
         Atomics.Set
           (Atomic => Global_Group_Activity_Indicator (Partition_ID),
            Bit    => Byte (Group_Index));
      end if;
   end Indicate_Activity;

   -------------------------------------------------------------------------

   --D @Section Id => impl_scheduling_upd_timer_list, Label => Updating Scheduling Partition Timer List, Parent => impl_scheduling, Priority => 40
   --D @Text Section => impl_scheduling_upd_timer_list
   --D Update the timer list by scannig all inactive scheduling groups of the
   --D specified partition and activating all groups for which the timer is
   --D expired.
   procedure Update_Timer_List (Partition : Policy.Scheduling_Partition_Range)
   with
      Global  => (Input  => X86_64.State,
                  In_Out => (Global_Group_Activity_Indicator,
                             Scheduling_Groups,
                             Scheduling_Partitions))
   is
      Cur_Group : Policy.Extended_Scheduling_Group_Range
        := Scheduling_Partitions (Partition).Earliest_Timer;
      Now : constant Word64 := CPU.RDTSC;
   begin
      Find_Earliest_Non_Expire_Timer_Loop :
      while Cur_Group /= Policy.No_Group loop
         if Scheduling_Groups (Cur_Group).Timeout <= Now then
            declare
               Next_Group : constant Policy.Extended_Scheduling_Group_Range
                 := Scheduling_Groups (Cur_Group).Next_Timer;
            begin
               Activate_Group (Partition_ID => Partition,
                               Group_ID     => Cur_Group);
               Cur_Group := Next_Group;
            end;
         else
            exit Find_Earliest_Non_Expire_Timer_Loop;
         end if;
      end loop Find_Earliest_Non_Expire_Timer_Loop;

      Scheduling_Partitions (Partition).Earliest_Timer := Cur_Group;
      if Cur_Group /= Policy.No_Group then
         Scheduling_Groups (Cur_Group).Prev_Timer := Policy.No_Group;
      end if;
   end Update_Timer_List;

   -------------------------------------------------------------------------

   --D @Section Id => impl_scheduling_upd_sp, Label => Update Scheduling Partitions, Parent => impl_scheduling, Priority => 0
   --D @Text Section => impl_scheduling_upd_sp
   --D Update scheduling partition information on minor/major frame change, by
   --D performing a scheduling operation for the currently active scheduling
   --D partition.
   --D @OL Id => impl_scheduling_upd_sp_steps, Section => impl_scheduling_upd_sp, Priority => 0
   procedure Update_Scheduling_Partition
   with
      Global => (Input  => (CPU_Info.CPU_ID, Current_Minor_Frame_ID,
                            Global_Current_Major_Frame_ID,
                            Subjects_Events.State, Subjects_Interrupts.State,
                            Timed_Events.State, X86_64.State),
                 In_Out => (Global_Group_Activity_Indicator, Scheduling_Groups,
                            Scheduling_Partitions, Subjects.State))
   is
      Partition_ID   : constant Policy.Scheduling_Partition_Range
        := Current_Scheduling_Partition_ID;
      Next_Group     : Policy.Extended_Scheduling_Group_Range;
      Next_Group_Idx : Policy.Scheduling_Group_Index_Range;
   begin
      --D @Item List => impl_scheduling_upd_sp_steps
      --D Update the timer list.
      Update_Timer_List (Partition => Partition_ID);

      --D @Item List => impl_scheduling_upd_sp_steps
      --D Find the next active scheduling group of the current partition.
      Find_Next_Active_Scheduling_Group (Partition_ID     => Partition_ID,
                                         Next_Group       => Next_Group,
                                         Next_Group_Index => Next_Group_Idx);
      if Next_Group /= Policy.No_Group then
         if Scheduling_Partitions (Partition_ID).Sleeping then

            --D @Item List => impl_scheduling_upd_sp_steps
            --D If an active scheduling group is present and the partition
            --D is sleeping, transition the scheduling partition to the active
            --D state.
            Subjects.Set_Activity_State
              (ID    => Scheduling_Groups (Next_Group).Active_Subject,
               Value => Constants.GUEST_ACTIVITY_ACTIVE);
            Scheduling_Partitions (Partition_ID).Sleeping := False;
         end if;

         --D @Item List => impl_scheduling_upd_sp_steps
         --D Set the running flag of the active subject of the next active
         --D scheduling group.
         Subjects.Set_Running
           (ID    => Scheduling_Groups (Next_Group).Active_Subject,
            Value => True);

         --D @Item List => impl_scheduling_upd_sp_steps
         --D Switch to the next scheduling group by making it the active group
         --D of the scheduling partition.
         Scheduling_Partitions (Partition_ID).Active_Group_Index
           := Next_Group_Idx;
      end if;
      --D @Item List => impl_scheduling_upd_sp_steps
      --D If no active scheduling group is found, nothing (except for updating
      --D the timer list) is done.
   end Update_Scheduling_Partition;

   -------------------------------------------------------------------------

   --D @Section Id => impl_scheduling_resched_sp, Label => Rescheduling Scheduling Partitions, Parent => impl_scheduling, Priority => 0
   --D @Text Section => impl_scheduling_resched_sp
   --D Reschedule a scheduling partition due to a sleep or yield action
   --D performed by a given subject.
   --D @OL Id => impl_scheduling_resched_sp_steps, Section => impl_scheduling_resched_sp, Priority => 0
   procedure Reschedule_Partition
     (Subject_ID      :     Skp.Global_Subject_ID_Type;
      RIP_Incremented :     Boolean;
      Sleep           :     Boolean;
      Next_Subject    : out Skp.Global_Subject_ID_Type)
   with
      Refined_Global =>
        (Input  => (CPU_Info.CPU_ID, Current_Minor_Frame_ID,
                    Global_Current_Major_Frame_ID,
                    Subjects_Events.State, Subjects_Interrupts.State,
                    Timed_Events.State, X86_64.State),
         In_Out => (Global_Group_Activity_Indicator, Scheduling_Groups,
                    Scheduling_Partitions, Subjects.State))
   is
      Partition_ID      : constant Policy.Scheduling_Partition_Range
        := Current_Scheduling_Partition_ID;
      Next_Group        : Policy.Extended_Scheduling_Group_Range;
      Next_Group_Idx    : Policy.Scheduling_Group_Index_Range;
      Subject_Is_Active : Boolean;
   begin
      if Sleep then
         --D @Item List => impl_scheduling_resched_sp_steps
         --D If the subject requested to sleep, set the running flag to False.
         Subjects.Set_Running (ID    => Subject_ID,
                               Value => False);
      end if;

      Subject_Is_Active := Is_Active (Subject_ID => Subject_ID);
      if not Subject_Is_Active then
         --D @Item List => impl_scheduling_resched_sp_steps
         --D Then, if the subject is not active, deactivate the associated
         --D scheduling group.
         Deactivate_Group
           (Partition_ID => Partition_ID,
            Group_Index  => Scheduling_Partitions
              (Partition_ID).Active_Group_Index,
            Subject_ID   => Subject_ID);
      end if;

      if not RIP_Incremented then
         Subjects.Increment_RIP (ID => Subject_ID);
      end if;

      --D @Item List => impl_scheduling_resched_sp_steps
      --D Find the next active scheduling group of the partition.
      Find_Next_Active_Scheduling_Group (Partition_ID     => Partition_ID,
                                         Next_Group       => Next_Group,
                                         Next_Group_Index => Next_Group_Idx);
      if Next_Group /= Policy.No_Group then

         --D @Item List => impl_scheduling_resched_sp_steps
         --D If an active group is present, switch to the next scheduling group
         --D by making it the active group of the scheduling partition.
         Scheduling_Partitions
           (Partition_ID).Active_Group_Index := Next_Group_Idx;
      else

         --D @Item List => impl_scheduling_resched_sp_steps
         --D If there is no active group, transition the active partition to
         --D sleep state.
         Subjects.Set_Activity_State (ID    => Subject_ID,
                                      Value => Constants.GUEST_ACTIVITY_HLT);
         Scheduling_Partitions (Partition_ID).Sleeping := True;
      end if;
      Next_Subject := Get_Current_Subject_ID;
   end Reschedule_Partition;

   -------------------------------------------------------------------------

   procedure Set_Current_Subject_ID (Subject_ID : Skp.Global_Subject_ID_Type)
   with
      Refined_Global => (Input  => (Current_Minor_Frame_ID,
                                    Global_Current_Major_Frame_ID,
                                    Scheduling_Partitions, CPU_Info.CPU_ID),
                         In_Out => Scheduling_Groups),
      Refined_Post   => Scheduling_Groups
         (Current_Scheduling_Group_ID).Active_Subject = Subject_ID
   is
   begin
      --D @Interface
      --D Set active subject of current scheduling group, which is determined
      --D by using the current major and minor frame IDs as indexes into the
      --D scheduling plan and the resulting partition ID as index into the
      --D scheduling partitions array.
      Scheduling_Groups (Current_Scheduling_Group_ID).Active_Subject
        := Subject_ID;
   end Set_Current_Subject_ID;

   -------------------------------------------------------------------------

   function Get_Current_Subject_ID return Skp.Global_Subject_ID_Type
   is (Scheduling_Groups (Current_Scheduling_Group_ID).Active_Subject)
   with
      Refined_Global => (Input => (Current_Minor_Frame_ID,
                                   Global_Current_Major_Frame_ID,
                                   Scheduling_Groups, Scheduling_Partitions,
                                   CPU_Info.CPU_ID)),
      Refined_Post   =>
        Get_Current_Subject_ID'Result =
           Scheduling_Groups (Current_Scheduling_Group_ID).Active_Subject;

   -------------------------------------------------------------------------

   procedure Update_Scheduling_Info
     (Next_Subject : out Skp.Global_Subject_ID_Type)
   with
      Refined_Global =>
        (Input  => (CPU_Info.CPU_ID, CPU_Info.Is_BSP, Subjects_Events.State,
                    Subjects_Interrupts.State, Tau0_Interface.State,
                    Timed_Events.State, X86_64.State),
         In_Out => (Current_Minor_Frame_ID, Global_Current_Major_Frame_ID,
                    Global_Current_Major_Start_Cycles,
                    Global_Group_Activity_Indicator, MP.Barrier,
                    Scheduling_Groups, Scheduling_Info.State,
                    Scheduling_Partitions, Subjects.State))
   is
      use type Policy.Major_Frame_Range;
      use type Policy.Minor_Frame_Range;
      use type Policy.Barrier_Index_Range;

      --D @Interface
      --D Save current global major frame ID to local constant to allow
      --D changing the global variable on major frame change.
      Current_Major_ID     : constant Policy.Major_Frame_Range
        := Global_Current_Major_Frame_ID;
      Current_Minor_ID     : constant Policy.Minor_Frame_Range
        := Current_Minor_Frame_ID;
      Current_Major_Length : constant Policy.Minor_Frame_Range
        := Policy.Scheduling_Plans (CPU_Info.CPU_ID) (Current_Major_ID).Length;

      --D @Interface
      --D Save current major frame CPU cycles for schedule info export.
      Current_Major_Frame_Start : constant Word64
        := Global_Current_Major_Start_Cycles;

      Next_Minor_ID : Policy.Minor_Frame_Range;
   begin
      if Current_Minor_ID < Current_Major_Length then

         --D @Text Section => impl_handle_timer_expiry, Priority => 10
         --D \paragraph{}
         --D In case of a regular minor frame switch, sync on minor frame
         --D barrier if necessary and switch to next minor frame in the current
         --D major frame.
         declare
            Current_Barrier : constant Policy.Barrier_Index_Range
              := Policy.Scheduling_Plans (CPU_Info.CPU_ID)
                (Current_Major_ID).Minor_Frames (Current_Minor_ID).Barrier;
         begin
            if Current_Barrier /= Policy.No_Barrier then
               MP.Wait_On_Minor_Frame_Barrier (Index => Current_Barrier);
            end if;
         end;

         Next_Minor_ID := Current_Minor_ID + 1;
      else

         --D @Text Section => impl_handle_timer_expiry, Priority => 10
         --D If the end of the major frame has been reached, switch to the first
         --D minor frame. Sync all CPU cores and then let the BSP update the
         --D next major frame ID as designated by Tau0.
         Next_Minor_ID := Policy.Minor_Frame_Range'First;

         MP.Wait_For_All;
         if CPU_Info.Is_BSP then
            declare

               --  Next major frame ID used to access the volatile New_Major
               --  variable. Do not move the declaration outside of this scope
               --  as it is only updated on the BSP. All other CPUs must get
               --  the value from CPU_Info.

               Next_Major_ID    : Policy.Major_Frame_Range;
               Next_Major_Start : Word64;
            begin
               Tau0_Interface.Get_Major_Frame (ID => Next_Major_ID);

               --D @Text Section => impl_handle_timer_expiry, Priority => 10
               --D Calculate next major frame start by incrementing the current
               --D global start timestamp by the length (also called period) of
               --D the major frame that just ended.
               Next_Major_Start := Global_Current_Major_Start_Cycles
                 + Policy.Major_Frames (Current_Major_ID).Period;

               --D @Text Section => impl_handle_timer_expiry, Priority => 10
               --D Update the global major frame ID by setting it to the next
               --D ID.
               Global_Current_Major_Frame_ID     := Next_Major_ID;
               --D @Text Section => impl_handle_timer_expiry, Priority => 10
               --D Set global major frame start cycles to the new major frame
               --D start time.
               Global_Current_Major_Start_Cycles := Next_Major_Start;

               pragma $Major_Frame_Warnings
                 (GNAT, Off,
                  "condition can only be True if invalid values present");
               if Current_Major_ID /= Next_Major_ID then
                  pragma $Major_Frame_Warnings
                    (GNAT, On,
                     "condition can only be True if invalid values present");
                  --D @Text Section => impl_handle_timer_expiry, Priority => 10
                  --D If the major frame has changed, set the corresponding
                  --D minor frame barrier configuration as specified by the
                  --D system policy.
                  MP.Set_Minor_Frame_Barrier_Config
                    (Config => Policy.Major_Frames
                       (Next_Major_ID).Barrier_Config);
               end if;
            end;
         end if;
         MP.Wait_For_All;
      end if;

      --  Update current minor frame globally.

      Current_Minor_Frame_ID := Next_Minor_ID;

      --D @Text Section => impl_handle_timer_expiry, Priority => 10
      --D After updating the major and minor frame information, which is the
      --D first level of the hierarchical scheduling algorithm, scheduling
      --D partitions are updated, see \ref{impl_scheduling_upd_sp}.

      Update_Scheduling_Partition;

      --  Subject switch.

      Next_Subject := Get_Current_Subject_ID;

      --D @Text Section => impl_handle_timer_expiry, Priority => 10
      --D Finally, publish the updated scheduling information to the next
      --D active scheduling group.

      --  Set scheduling information of scheduling group.

      Scheduling_Info.Set_Scheduling_Info
        (ID                 => Policy.Get_Scheduling_Group_ID
           (Subject_ID => Next_Subject),
         TSC_Schedule_Start => Current_Major_Frame_Start +
           Policy.Scheduling_Plans (CPU_Info.CPU_ID)
             (Current_Major_ID).Minor_Frames (Current_Minor_ID).Deadline,
         TSC_Schedule_End   => Global_Current_Major_Start_Cycles +
           Policy.Scheduling_Plans (CPU_Info.CPU_ID)
             (Global_Current_Major_Frame_ID).Minor_Frames
             (Next_Minor_ID).Deadline);
   end Update_Scheduling_Info;

   -------------------------------------------------------------------------

   procedure Set_VMX_Exit_Timer
   is
      Now      : constant Word64 := CPU.RDTSC;
      Deadline : Word64;
      Cycles   : Word64;
   begin

      --  Absolute deadline is given by start of major frame plus the number of
      --  CPU cycles until the end of the current minor frame relative to major
      --  frame start.

      --D @Interface
      --D Calculate absolute deadline timestamp by using the current global
      --D major frame start timestamp and adding the current minor frame
      --D deadline, which is relative to major frame start.
      Deadline := Global_Current_Major_Start_Cycles +
        Policy.Scheduling_Plans (CPU_Info.CPU_ID)
        (Global_Current_Major_Frame_ID).Minor_Frames
        (Current_Minor_Frame_ID).Deadline;

      if Deadline > Now then
         Cycles := Deadline - Now;
      else
         Cycles := 0;
      end if;

      VMX.VMCS_Write (Field => Constants.GUEST_VMX_PREEMPT_TIMER,
                      Value => Cycles / 2 ** Policy.VMX_Timer_Rate);
   end Set_VMX_Exit_Timer;

   -------------------------------------------------------------------------

   --  Initialize scheduling group information which ensures that the initial
   --  subject of each scheduling group is set as the active subject.
   --  Furthermore the timer list is set up as being empty without a pending
   --  timeout.
   procedure Init_Scheduling_Groups
   with
      Global  => (Output => Scheduling_Groups),
      Depends => (Scheduling_Groups => null)
   is
   begin
      for I in Scheduling_Groups'Range loop
         Scheduling_Groups (I)
           := (Active_Subject => Policy.Scheduling_Group_Config
               (I).Initial_Subject,
               Next_Timer     => Policy.No_Group,
               Prev_Timer     => Policy.No_Group,
               Timeout        => Word64'Last);
      end loop;
   end Init_Scheduling_Groups;

   -------------------------------------------------------------------------

   --D @Section Id => impl_kernel_init_sched, Label => Scheduler Initialization, Parent => impl_kernel_init, Priority => 10
   --D @Text Section => impl_kernel_init_sched
   --D Scheduler initialization is performed by each CPU and consists of the
   --D following steps:
   --D @OL Id => impl_kernel_init_sched_steps, Section => impl_kernel_init_sched
   procedure Init
   is
      use type Skp.CPU_Range;
   begin
      --D @Item List => impl_kernel_init_sched_steps
      --D Initialize scheduling group data structures, i.e. set initially active
      --D subjects.
      Init_Scheduling_Groups;

      --D @Item List => impl_kernel_init_sched_steps
      --D Setup VMCS and state of each subject running on this logical CPU,
      --D see \ref{impl_subject_init}.
      for I in Skp.Global_Subject_ID_Type loop
         if Skp.Subjects.Get_CPU_ID (Subject_ID => I) = CPU_Info.CPU_ID then
            Kernel.Init_Subject (ID => I);
         end if;
      end loop;

      declare
         Now               : constant Word64 := CPU.RDTSC;
         Current_Subject   : constant Skp.Global_Subject_ID_Type
           := Get_Current_Subject_ID;
         Current_VMCS_Addr : constant Word64
           := Skp.Subjects.Get_VMCS_Address (Subject_ID => Current_Subject);
      begin
         --D @Item List => impl_kernel_init_sched_steps
         --D Load VMCS of initial subject.
         VMX.Load (VMCS_Address => Current_VMCS_Addr);

         --D @Item List => impl_kernel_init_sched_steps
         --D Set start and end timestamp of initial minor frame for
         --D the scheduling group of the first subject based on current
         --D TSC.
         Scheduling_Info.Set_Scheduling_Info
           (ID                 => Policy.Get_Scheduling_Group_ID
              (Subject_ID => Current_Subject),
            TSC_Schedule_Start => Now,
            TSC_Schedule_End   => Now + Policy.Scheduling_Plans
              (CPU_Info.CPU_ID)
                (Policy.Major_Frame_Range'First).Minor_Frames
                (Policy.Minor_Frame_Range'First).Deadline);

         if CPU_Info.Is_BSP then

            --D @Item List => impl_kernel_init_sched_steps
            --D Set global minor frame barriers config (BSP-only).
            MP.Set_Minor_Frame_Barrier_Config
              (Config => Policy.Major_Frames
                 (Policy.Major_Frame_Range'First).Barrier_Config);

            --D @Item List => impl_kernel_init_sched_steps
            --D Set initial major frame start time to now.
            Global_Current_Major_Start_Cycles := Now;
         end if;
      end;
   end Init;

   -------------------------------------------------------------------------

begin
   if CPU_Info.Is_BSP then

      --  The group indicator array is a single instance shared by all CPUs. So
      --  it must only be initialized by a single CPU, i.e. BSP. This is done
      --  during elaboration so gnatprove is able to show that the abstract
      --  Global_Activity_Indicator state is properly initialized.

      for I in Policy.Scheduling_Partition_Range loop
         Atomics.Init (Atomic => Global_Group_Activity_Indicator (I));

         --  Mark all defined groups of the partition as active.

         for J in Policy.Scheduling_Group_Index_Range'First ..
           Policy.Scheduling_Partition_Config (I).Last_Group_Index
         loop
            Atomics.Set (Atomic => Global_Group_Activity_Indicator (I),
                         Bit    => Atomics.Bit_Pos (J));
         end loop;
      end loop;
   end if;
end SK.Scheduler;
