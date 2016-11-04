package Skp.Kernel
is

   Stack_Address             : constant := __stack_addr__;
   CPU_Store_Address         : constant := __cpu_store_addr__;
   Tau0_Iface_Address        : constant := __tau0_iface_addr__;
   Subj_States_Address       : constant := __subj_states_addr__;
   Subj_Timed_Events_Address : constant := __subj_timed_events_addr__;
   Subj_Interrupts_Address   : constant := __subj_interrupts_addr__;
   Subj_MSR_Store_Address    : constant := __subj_msr_store_addr__;
   Subj_VMCS_Address         : constant := __subj_vmcs_addr__;
   IO_Apic_Address           : constant := __ioapic_addr__;
   Subj_Sinfo_Address        : constant := __subj_sinfo_addr__;
   Subj_Sinfo_Size           : constant := __subj_sinfo_size__;
   Sched_Group_Info_Address  : constant := __sched_group_info_addr__;

end Skp.Kernel;
