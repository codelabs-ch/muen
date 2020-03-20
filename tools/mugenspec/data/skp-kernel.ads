package Skp.Kernel
is

   TSC_Mhz : constant := 2900;

   Stack_Address             : constant := 16#0011_3000#;
   Intr_Stack_Address        : constant := 16#0011_4000#;
   Tau0_Iface_Address        : constant := 16#001f_f000#;
   Subj_States_Address       : constant := 16#001e_0000#;
   Subj_Timed_Events_Address : constant := 16#0040_0000#;
   Subj_Interrupts_Address   : constant := 16#0060_0000#;
   Subj_MSR_Store_Address    : constant := 16#0080_0000#;
   Subj_VMCS_Address         : constant := 16#0090_0000#;
   Subj_FPU_State_Address    : constant := 16#00b0_0000#;
   Sched_Group_Info_Address  : constant := 16#0090_0000#;
   Crash_Audit_Address       : constant := 16#000e_0001_9000#;
   Crash_Audit_Size          : constant := 16#1000#;

end Skp.Kernel;
