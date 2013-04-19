package Skp is

   type CPU_Range is range __cpu_range__;

   subtype Subject_Id_Type is Natural range __subj_range__;

   Vmxon_Address : constant := 16#__vmxon_addr__#;

end Skp;
