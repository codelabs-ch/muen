package Skp
is

   CPU_Count : constant := __cpu_count__;

   type CPU_Range is range 0 .. CPU_Count - 1;

   subtype Global_Subject_ID_Type is Natural range __subj_range__;

   subtype Subject_Id_Type is Natural range Global_Subject_ID_Type'Range;

   Invalid_Subject : constant := Global_Subject_ID_Type'Last + 1;

   subtype Dst_Subject_Type is Natural range 0 .. Invalid_Subject;

   type Vector_Range is range 0 .. 255;

   Invalid_Vector : constant := 256;

   type Dst_Vector_Range is range 0 .. Invalid_Vector;

   Vmxon_Address : constant := __vmxon_addr__;

end Skp;
