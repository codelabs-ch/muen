with SK;

--D @Interface
--D This package contains scheduling plans, minor frame synchronization barrier
--D configurations as well as subject to scheduling group ID mappings as
--D specified by the system policy.
package Skp.Scheduling
is

   VMX_Timer_Rate : constant := 5;

   Max_Groups_Per_Partition : constant := 64;

   type Scheduling_Partition_Range is range 1 .. 3;

   type Extended_Scheduling_Group_Range is
     range 0 .. 4;
   subtype Scheduling_Group_Range is Extended_Scheduling_Group_Range
     range 1 .. Extended_Scheduling_Group_Range'Last;

   No_Group : constant Extended_Scheduling_Group_Range
     := Extended_Scheduling_Group_Range'First;

   type Barrier_Index_Range is range 0 .. 3;

   subtype Barrier_Range is
     Barrier_Index_Range range 1 .. Barrier_Index_Range'Last;

   No_Barrier : constant Barrier_Index_Range := Barrier_Index_Range'First;

   type Minor_Frame_Type is record
      Partition_ID : Scheduling_Partition_Range;
      Barrier      : Barrier_Index_Range;
      Deadline     : SK.Word64;
   end record;

   Null_Minor_Frame : constant Minor_Frame_Type := Minor_Frame_Type'
     (Partition_ID => Scheduling_Partition_Range'First,
      Barrier      => No_Barrier,
      Deadline     => 0);

   type Minor_Frame_Range is range 1 .. 9;

   type Minor_Frame_Array is array (Minor_Frame_Range) of Minor_Frame_Type;

   type Major_Frame_Type is record
      Length       : Minor_Frame_Range;
      Minor_Frames : Minor_Frame_Array;
   end record;

   type Major_Frame_Range is range 0 .. 2;

   type Major_Frame_Array is array (Major_Frame_Range) of Major_Frame_Type;

   Null_Major_Frames : constant Major_Frame_Array := Major_Frame_Array'
     (others => Major_Frame_Type'
        (Length       => Minor_Frame_Range'First,
         Minor_Frames => Minor_Frame_Array'
           (others => Null_Minor_Frame)));

   type Scheduling_Plan_Type is array (CPU_Range) of Major_Frame_Array;

   Scheduling_Plans : constant Scheduling_Plan_Type := Scheduling_Plan_Type'(
     0 => Major_Frame_Array'(
       0 => Major_Frame_Type'
         (Length       => 1,
          Minor_Frames => Minor_Frame_Array'(
             1 => Minor_Frame_Type'(Partition_ID => 1,
                                    Barrier      => No_Barrier,
                                    Deadline     => 4930000000),
             others => Null_Minor_Frame)),
       1 => Major_Frame_Type'
         (Length       => 4,
          Minor_Frames => Minor_Frame_Array'(
             1 => Minor_Frame_Type'(Partition_ID => 1,
                                    Barrier      => No_Barrier,
                                    Deadline     => 1450000000),
             2 => Minor_Frame_Type'(Partition_ID => 3,
                                    Barrier      => 1,
                                    Deadline     => 2900000000),
             3 => Minor_Frame_Type'(Partition_ID => 1,
                                    Barrier      => No_Barrier,
                                    Deadline     => 4350000000),
             4 => Minor_Frame_Type'(Partition_ID => 3,
                                    Barrier      => No_Barrier,
                                    Deadline     => 4930000000),
             others => Null_Minor_Frame)),
       2 => Major_Frame_Type'
         (Length       => 6,
          Minor_Frames => Minor_Frame_Array'(
             1 => Minor_Frame_Type'(Partition_ID => 1,
                                    Barrier      => 1,
                                    Deadline     => 870000000),
             2 => Minor_Frame_Type'(Partition_ID => 3,
                                    Barrier      => 2,
                                    Deadline     => 1450000000),
             3 => Minor_Frame_Type'(Partition_ID => 1,
                                    Barrier      => No_Barrier,
                                    Deadline     => 1595000000),
             4 => Minor_Frame_Type'(Partition_ID => 3,
                                    Barrier      => No_Barrier,
                                    Deadline     => 1812500000),
             5 => Minor_Frame_Type'(Partition_ID => 1,
                                    Barrier      => 3,
                                    Deadline     => 2030000000),
             6 => Minor_Frame_Type'(Partition_ID => 3,
                                    Barrier      => No_Barrier,
                                    Deadline     => 2610000000),
             others => Null_Minor_Frame))),
     1 => Major_Frame_Array'(
       0 => Major_Frame_Type'
         (Length       => 1,
          Minor_Frames => Minor_Frame_Array'(
             1 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => No_Barrier,
                                    Deadline     => 4930000000),
             others => Null_Minor_Frame)),
       1 => Major_Frame_Type'
         (Length       => 2,
          Minor_Frames => Minor_Frame_Array'(
             1 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => 1,
                                    Deadline     => 2900000000),
             2 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => No_Barrier,
                                    Deadline     => 4930000000),
             others => Null_Minor_Frame)),
       2 => Major_Frame_Type'
         (Length       => 9,
          Minor_Frames => Minor_Frame_Array'(
             1 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => No_Barrier,
                                    Deadline     => 290000000),
             2 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => No_Barrier,
                                    Deadline     => 580000000),
             3 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => 1,
                                    Deadline     => 870000000),
             4 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => No_Barrier,
                                    Deadline     => 1160000000),
             5 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => 2,
                                    Deadline     => 1450000000),
             6 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => No_Barrier,
                                    Deadline     => 1740000000),
             7 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => 3,
                                    Deadline     => 2030000000),
             8 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => No_Barrier,
                                    Deadline     => 2320000000),
             9 => Minor_Frame_Type'(Partition_ID => 2,
                                    Barrier      => No_Barrier,
                                    Deadline     => 2610000000)))));

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
       0 => Major_Frame_Info_Type'
         (Period         => 4930000000,
          Barrier_Config => Barrier_Config_Array'(
            others => Barrier_Size_Type'First)),
       1 => Major_Frame_Info_Type'
         (Period         => 4930000000,
          Barrier_Config => Barrier_Config_Array'(
            1 => 2,
            others => Barrier_Size_Type'First)),
       2 => Major_Frame_Info_Type'
         (Period         => 2610000000,
          Barrier_Config => Barrier_Config_Array'(
            1 => 2,
            2 => 2,
            3 => 2)));

   type Scheduling_Group_Index_Range is
      range 0 .. Max_Groups_Per_Partition - 1;
   type Scheduling_Group_Map is array (Scheduling_Group_Index_Range)
     of Extended_Scheduling_Group_Range;

   type Scheduling_Partition_Config_Type is record
      Last_Group_Index : Scheduling_Group_Index_Range;
      Groups           : Scheduling_Group_Map;
   end record
     with Dynamic_Predicate =>
       (for all I in Scheduling_Group_Index_Range =>
          (if I <= Last_Group_Index then
             Groups (I) /= No_Group
          else Groups (I) = No_Group));

   type Scheduling_Partition_Config_Array is array (Scheduling_Partition_Range)
     of Scheduling_Partition_Config_Type;

   Scheduling_Partition_Config : constant Scheduling_Partition_Config_Array
     := Scheduling_Partition_Config_Array'(
          1 => (Last_Group_Index => 0,
                Groups           => Scheduling_Group_Map'(
                   0 => 1,
                   others => No_Group)),
          2 => (Last_Group_Index => 0,
                Groups           => Scheduling_Group_Map'(
                   0 => 2,
                   others => No_Group)),
          3 => (Last_Group_Index => 1,
                Groups           => Scheduling_Group_Map'(
                   0 => 3,
                   1 => 4,
                   others => No_Group)));

   type Scheduling_Group_Config_Type is record
      Initial_Subject : Global_Subject_ID_Type;
      Group_Index     : Scheduling_Group_Index_Range;
   end record;

   type Scheduling_Group_Config_Array is array (Scheduling_Group_Range)
     of Scheduling_Group_Config_Type;

   Scheduling_Group_Config : constant Scheduling_Group_Config_Array
     := Scheduling_Group_Config_Array'(
          1 => (Initial_Subject => 0,
                Group_Index     => 0),
          2 => (Initial_Subject => 1,
                Group_Index     => 0),
          3 => (Initial_Subject => 2,
                Group_Index     => 0),
          4 => (Initial_Subject => 3,
                Group_Index     => 1));

   type Subject_To_Sched_Partition_Array is array (Global_Subject_ID_Type)
     of Scheduling_Partition_Range;

   Subject_To_Scheduling_Partition : constant Subject_To_Sched_Partition_Array
     := Subject_To_Sched_Partition_Array'(
          0 => 1,
          1 => 2,
          2 => 3,
          3 => 3);

   type Subject_To_Scheduling_Group_Array is array (Global_Subject_ID_Type)
     of Scheduling_Group_Range;

   Subject_To_Scheduling_Group : constant Subject_To_Scheduling_Group_Array
     := Subject_To_Scheduling_Group_Array'(
          0 => 1,
          1 => 2,
          2 => 3,
          3 => 4);

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

   --  Returns the scheduling group ID of the group specified by index in the
   --  context of the given scheduling partition.
   function Get_Scheduling_Group_ID
     (Partition_ID : Scheduling_Partition_Range;
      Group_Index  : Scheduling_Group_Index_Range)
     return Scheduling_Group_Range
   is
     (Scheduling_Partition_Config (Partition_ID).Groups (Group_Index))
   with
      Pre => Group_Index <= Scheduling_Partition_Config
           (Partition_ID).Last_Group_Index;

   --  Returns the scheduling group index of the group specified by ID.
   function Get_Scheduling_Group_Index
     (Group_ID : Scheduling_Group_Range)
      return Scheduling_Group_Index_Range
   is
     (Scheduling_Group_Config (Group_ID).Group_Index);

end Skp.Scheduling;
