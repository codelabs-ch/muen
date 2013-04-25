package Skp is

   type CPU_Range is range __cpu_range__;

   subtype Subject_Id_Type is Natural range __subj_range__;

   Invalid_Subject : constant := Subject_Id_Type'Last + 1;

   subtype Dst_Subject_Type is Natural range 0 .. Invalid_Subject;

   Vmxon_Address : constant := 16#__vmxon_addr__#;

end Skp;
