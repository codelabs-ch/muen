package Skp.Kernel
is

   Stack_Address             : constant := 16#0011_3000#;
   CPU_Store_Address         : constant := 16#0011_6000#;
   Tau0_Iface_Address        : constant := 16#001f_f000#;
   Subj_States_Address       : constant := 16#001e_0000#;
   Subj_Timed_Events_Address : constant := 16#0040_0000#;
   Subj_Interrupts_Address   : constant := 16#0060_0000#;
   Subj_MSR_Store_Address    : constant := 16#0080_0000#;
   Subj_VMCS_Address         : constant := 16#0090_0000#;
   Subj_FPU_State_Address    : constant := 16#00b0_0000#;
   IO_Apic_Address           : constant := 16#001f_c000#;
   Subj_Sinfo_Address        : constant := 16#0050_0000#;
   Subj_Sinfo_Size           : constant := 16#7000#;
   Sched_Group_Info_Address  : constant := 16#0090_0000#;

end Skp.Kernel;
