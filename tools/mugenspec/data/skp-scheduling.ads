with SK;

package Skp.Scheduling
is

   VMX_Timer_Rate : constant := 5;

   type Scheduling_Group_Range is range 1 .. 4;

   type Barrier_Index_Range is range 0 .. 3;

   subtype Barrier_Range is
     Barrier_Index_Range range 1 .. Barrier_Index_Range'Last;

   No_Barrier : constant Barrier_Index_Range := Barrier_Index_Range'First;

   type Minor_Frame_Type is record
      Group_ID : Scheduling_Group_Range;
      Barrier  : Barrier_Index_Range;
      Deadline : SK.Word64;
   end record;

   Null_Minor_Frame : constant Minor_Frame_Type := Minor_Frame_Type'
     (Group_ID => Scheduling_Group_Range'First,
      Barrier  => No_Barrier,
      Deadline => 0);

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
             1 => Minor_Frame_Type'(Group_ID => 1,
                                    Barrier  => No_Barrier,
                                    Deadline => 4930000000),
             others => Null_Minor_Frame)),
       1 => Major_Frame_Type'
         (Length       => 4,
          Minor_Frames => Minor_Frame_Array'(
             1 => Minor_Frame_Type'(Group_ID => 1,
                                    Barrier  => No_Barrier,
                                    Deadline => 1450000000),
             2 => Minor_Frame_Type'(Group_ID => 3,
                                    Barrier  => 1,
                                    Deadline => 2900000000),
             3 => Minor_Frame_Type'(Group_ID => 1,
                                    Barrier  => No_Barrier,
                                    Deadline => 4350000000),
             4 => Minor_Frame_Type'(Group_ID => 4,
                                    Barrier  => No_Barrier,
                                    Deadline => 4930000000),
             others => Null_Minor_Frame)),
       2 => Major_Frame_Type'
         (Length       => 6,
          Minor_Frames => Minor_Frame_Array'(
             1 => Minor_Frame_Type'(Group_ID => 1,
                                    Barrier  => 1,
                                    Deadline => 870000000),
             2 => Minor_Frame_Type'(Group_ID => 3,
                                    Barrier  => 2,
                                    Deadline => 1450000000),
             3 => Minor_Frame_Type'(Group_ID => 1,
                                    Barrier  => No_Barrier,
                                    Deadline => 1595000000),
             4 => Minor_Frame_Type'(Group_ID => 3,
                                    Barrier  => No_Barrier,
                                    Deadline => 1812500000),
             5 => Minor_Frame_Type'(Group_ID => 1,
                                    Barrier  => 3,
                                    Deadline => 2030000000),
             6 => Minor_Frame_Type'(Group_ID => 3,
                                    Barrier  => No_Barrier,
                                    Deadline => 2610000000),
             others => Null_Minor_Frame))),
     1 => Major_Frame_Array'(
       0 => Major_Frame_Type'
         (Length       => 1,
          Minor_Frames => Minor_Frame_Array'(
             1 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => No_Barrier,
                                    Deadline => 4930000000),
             others => Null_Minor_Frame)),
       1 => Major_Frame_Type'
         (Length       => 2,
          Minor_Frames => Minor_Frame_Array'(
             1 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => 1,
                                    Deadline => 2900000000),
             2 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => No_Barrier,
                                    Deadline => 4930000000),
             others => Null_Minor_Frame)),
       2 => Major_Frame_Type'
         (Length       => 9,
          Minor_Frames => Minor_Frame_Array'(
             1 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => No_Barrier,
                                    Deadline => 290000000),
             2 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => No_Barrier,
                                    Deadline => 580000000),
             3 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => 1,
                                    Deadline => 870000000),
             4 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => No_Barrier,
                                    Deadline => 1160000000),
             5 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => 2,
                                    Deadline => 1450000000),
             6 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => No_Barrier,
                                    Deadline => 1740000000),
             7 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => 3,
                                    Deadline => 2030000000),
             8 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => No_Barrier,
                                    Deadline => 2320000000),
             9 => Minor_Frame_Type'(Group_ID => 2,
                                    Barrier  => No_Barrier,
                                    Deadline => 2610000000)))));

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

   type Scheduling_Group_Array is array (Scheduling_Group_Range)
     of Global_Subject_ID_Type;

   Scheduling_Groups : constant Scheduling_Group_Array
     := Scheduling_Group_Array'(
          1 => 0,
          2 => 1,
          3 => 2,
          4 => 3);

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

end Skp.Scheduling;
