with SK;

--D @Interface
--D This package contains scheduling plans, minor frame synchronization barrier
--D configurations as well as subject to scheduling group ID mappings as
--D specified by the system policy.
package Skp.Scheduling
is

   VMX_Timer_Rate : constant := __vmx_timer_rate__;

   type Scheduling_Partition_Range is range __scheduling_partition_range__;
   type Scheduling_Group_Range is range __scheduling_group_range__;

   type Barrier_Index_Range is range 0 .. __max_barrier_count__;

   subtype Barrier_Range is
     Barrier_Index_Range range 1 .. Barrier_Index_Range'Last;

   No_Barrier : constant Barrier_Index_Range := Barrier_Index_Range'First;

   type Minor_Frame_Type is record
      Partition_ID : Scheduling_Partition_Range;
      Group_ID     : Scheduling_Group_Range;
      Barrier      : Barrier_Index_Range;
      Deadline     : SK.Word64;
   end record;

   Null_Minor_Frame : constant Minor_Frame_Type := Minor_Frame_Type'
     (Partition_ID => Scheduling_Partition_Range'First,
      Group_ID     => Scheduling_Group_Range'First,
      Barrier      => No_Barrier,
      Deadline     => 0);

   type Minor_Frame_Range is range __minor_range__;

   type Minor_Frame_Array is array (Minor_Frame_Range) of Minor_Frame_Type;

   type Major_Frame_Type is record
      Length       : Minor_Frame_Range;
      Minor_Frames : Minor_Frame_Array;
   end record;

   type Major_Frame_Range is range __major_range__;

   type Major_Frame_Array is array (Major_Frame_Range) of Major_Frame_Type;

   Null_Major_Frames : constant Major_Frame_Array := Major_Frame_Array'
     (others => Major_Frame_Type'
        (Length       => Minor_Frame_Range'First,
         Minor_Frames => Minor_Frame_Array'
           (others => Null_Minor_Frame)));

   type Scheduling_Plan_Type is array (CPU_Range) of Major_Frame_Array;

   Scheduling_Plans : constant Scheduling_Plan_Type := Scheduling_Plan_Type'(
__scheduling_plans__);

   subtype Barrier_Size_Type is
     Natural range 1 .. Natural (CPU_Range'Last) + 1;

   type Barrier_Config_Array is array (Barrier_Range) of Barrier_Size_Type;

   type Major_Frame_Info_Type is record
      Period         : SK.Word64;
      Barrier_Config : Barrier_Config_Array;
   end record;

   type Major_Frame_Info_Array is array (Major_Frame_Range)
     of Major_Frame_Info_Type;

   Major_Frames : constant Major_Frame_Info_Array := Major_Frame_Info_Array'(
__major_frames_info__);

   type Scheduling_Group_Array is array (Scheduling_Group_Range)
     of Global_Subject_ID_Type;

   Scheduling_Groups : constant Scheduling_Group_Array
     := Scheduling_Group_Array'(
__scheduling_groups__);

   type Subject_To_Sched_Partition_Array is array (Global_Subject_ID_Type)
     of Scheduling_Partition_Range;

   Subject_To_Scheduling_Partition : constant Subject_To_Sched_Partition_Array
     := Subject_To_Sched_Partition_Array'(
__subj_to_scheduling_partition__);

   type Subject_To_Scheduling_Group_Array is array (Global_Subject_ID_Type)
     of Scheduling_Group_Range;

   Subject_To_Scheduling_Group : constant Subject_To_Scheduling_Group_Array
     := Subject_To_Scheduling_Group_Array'(
__subj_to_scheduling_group__);

   --  Returns the scheduling group ID of the subject specified by ID.
   function Get_Scheduling_Group_ID
     (Subject_ID : Global_Subject_ID_Type)
     return Scheduling_Group_Range
   is
     (Subject_To_Scheduling_Group (Subject_ID));

   --  Returns the scheduling partition ID of the subject specified by ID.
   function Get_Scheduling_Partition_ID
     (Subject_ID : Global_Subject_ID_Type)
     return Scheduling_Partition_Range
   is
     (Subject_To_Scheduling_Partition (Subject_ID));

end Skp.Scheduling;
