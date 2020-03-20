package Skp.Kernel
is

   TSC_Mhz : constant := __tsc_mhz__;

   Stack_Address             : constant := __stack_addr__;
   Intr_Stack_Address        : constant := __intr_stack_addr__;
   Tau0_Iface_Address        : constant := __tau0_iface_addr__;
   Subj_States_Address       : constant := __subj_states_addr__;
   Subj_Timed_Events_Address : constant := __subj_timed_events_addr__;
   Subj_Interrupts_Address   : constant := __subj_interrupts_addr__;
   Subj_MSR_Store_Address    : constant := __subj_msr_store_addr__;
   Subj_VMCS_Address         : constant := __subj_vmcs_addr__;
   Subj_FPU_State_Address    : constant := __subj_fpu_state_addr__;
   Sched_Group_Info_Address  : constant := __sched_group_info_addr__;
   Crash_Audit_Address       : constant := __crash_audit_addr__;
   Crash_Audit_Size          : constant := __crash_audit_size__;

end Skp.Kernel;
