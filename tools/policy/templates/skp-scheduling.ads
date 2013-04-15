with SK;

--# inherit SK, Skp;
package Skp.Scheduling is

   type Minor_Frame_Type is record
      Subject_Id : Skp.Subject_Id_Type;
      Ticks      : SK.Word32;
   end record;

   Null_Minor_Frame : constant Minor_Frame_Type := Minor_Frame_Type'
     (Subject_Id => 0,
      Ticks      => 0);

   type Minor_Frame_Range is range __minor_range__;

   type Minor_Frame_Array is array (Minor_Frame_Range) of Minor_Frame_Type;

   type CPU_Range is range __cpu_range__;

   type Major_Frame_Type is record
      Length       : Minor_Frame_Range;
      Minor_Frames : Minor_Frame_Array;
   end record;

   type Major_Frame_Range is range __major_range__;

   type Scheduling_Plan_Type is array (Major_Frame_Range) of Major_Frame_Type;

   Scheduling_Plans : constant Scheduling_Plan_Type := Scheduling_Plan_Type'(
__scheduling_plans__);

end Skp.Scheduling;
