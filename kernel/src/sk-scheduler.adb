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
   Refined_State => (State => (Current_Minor_Frame_ID,
                               Global_Current_Major_Frame_ID,
                               Global_Current_Major_Start_Cycles,
                               Scheduling_Partitions, Scheduling_Groups))
is

   -------------------------------------------------------------------------

   procedure Set_Current_Subject_ID (Subject_ID : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (Input  => (Current_Minor_Frame_ID,
                                     Global_Current_Major_Frame_ID,
                                     CPU_Info.CPU_ID),
                          In_Out => Scheduling_Groups),
      Refined_Post    => Scheduling_Groups
        (Policy.Scheduling_Plans (CPU_Info.CPU_ID)
         (Global_Current_Major_Frame_ID).Minor_Frames
         (Current_Minor_Frame_ID).Group_ID).Active_Subject = Subject_ID
   is
   begin
      --D @Interface
      --D Set active subject of current scheduling group, which is determined
      --D by using the current major and minor frame IDs as indexes into the
      --D scheduling plan.
      Scheduling_Groups
        (Policy.Scheduling_Plans (CPU_Info.CPU_ID)
         (Global_Current_Major_Frame_ID).Minor_Frames
         (Current_Minor_Frame_ID).Group_ID).Active_Subject := Subject_ID;
   end Set_Current_Subject_ID;

   -------------------------------------------------------------------------

   function Get_Current_Subject_ID return Skp.Global_Subject_ID_Type
   with
      Refined_Global => (Input => (Current_Minor_Frame_ID,
                                   Global_Current_Major_Frame_ID,
                                   Scheduling_Groups, CPU_Info.CPU_ID)),
      Refined_Post   =>
        Get_Current_Subject_ID'Result =
          Scheduling_Groups
            (Policy.Scheduling_Plans (CPU_Info.CPU_ID)
             (Global_Current_Major_Frame_ID).Minor_Frames
             (Current_Minor_Frame_ID).Group_ID).Active_Subject
   is
   begin
      return Scheduling_Groups
        (Policy.Scheduling_Plans (CPU_Info.CPU_ID)
         (Global_Current_Major_Frame_ID).Minor_Frames
         (Current_Minor_Frame_ID).Group_ID).Active_Subject;
   end Get_Current_Subject_ID;

   -------------------------------------------------------------------------

   procedure Update_Scheduling_Info
     (Next_Subject : out Skp.Global_Subject_ID_Type)
   with
      Refined_Global =>
        (Input  => (Scheduling_Groups, CPU_Info.CPU_ID, CPU_Info.Is_BSP,
                    Tau0_Interface.State),
         In_Out => (Current_Minor_Frame_ID, Global_Current_Major_Frame_ID,
                    Global_Current_Major_Start_Cycles, MP.Barrier,
                    Scheduling_Info.State))
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
      Current_Major_Frame_Start : constant SK.Word64
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
               Next_Major_Start : SK.Word64;
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
      Now      : constant SK.Word64 := CPU.RDTSC;
      Deadline : SK.Word64;
      Cycles   : SK.Word64;
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

   procedure Init_Scheduling_Groups
   with
      Global =>
        (Output => Scheduling_Groups)
   is
   begin
      for I in Scheduling_Groups'Range loop
         Scheduling_Groups (I)
           := (Active_Subject => Policy.Scheduling_Group_Config
               (I).Initial_Subject,
               Next_Timer     => Policy.No_Group,
               Prev_Timer     => Policy.No_Group,
               Timeout        => SK.Word64'Last);
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
         Now               : constant SK.Word64 := CPU.RDTSC;
         Current_Subject   : constant Skp.Global_Subject_ID_Type
           := Get_Current_Subject_ID;
         Current_VMCS_Addr : constant SK.Word64
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

end SK.Scheduler;
