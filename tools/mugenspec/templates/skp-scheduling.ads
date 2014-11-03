with SK;

package Skp.Scheduling
is

   type Barrier_Index_Range is range 0 .. __max_barrier_count__;

   subtype Barrier_Range is
     Barrier_Index_Range range 1 .. Barrier_Index_Range'Last;

   No_Barrier : constant Barrier_Index_Range := Barrier_Index_Range'First;

   type Minor_Frame_Type is record
      Subject_Id : Skp.Subject_Id_Type;
      Ticks      : SK.Word32;
      Barrier    : Barrier_Index_Range;
   end record;

   Null_Minor_Frame : constant Minor_Frame_Type := Minor_Frame_Type'
     (Subject_Id => 0,
      Ticks      => 0,
      Barrier    => No_Barrier);

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

   type Scheduling_Plan_Type is array (Skp.CPU_Range) of Major_Frame_Array;

   Scheduling_Plans : constant Scheduling_Plan_Type := Scheduling_Plan_Type'(
__scheduling_plans__);

end Skp.Scheduling;
