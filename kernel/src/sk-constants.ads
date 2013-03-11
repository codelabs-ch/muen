package SK.Constants
is

   -----------
   -- Flags --
   -----------

   RFLAGS_CF_FLAG           : constant := 0;
   RFLAGS_ZF_FLAG           : constant := 6;
   RFLAGS_VM_FLAG           : constant := 17;

   CR0_PE_FLAG              : constant := 0;
   CR0_PG_FLAG              : constant := 0;

   CR4_VMXE_FLAG            : constant := 13;

   CPUID_FEATURE_VMX_FLAG   : constant := 5;

   IA32_EFER_LMA_FLAG       : constant := 10;
   IA32_FCTRL_SMX_FLAG      : constant := 2;

   ----------
   -- MSRs --
   ----------

   IA32_FEATURE_CONTROL     : constant := 16#3a#;
   IA32_EFER                : constant := 16#c000_0080#;

   IA32_VMX_BASIC           : constant := 16#480#;
   IA32_VMX_PINBASED_CTLS   : constant := 16#481#;
   IA32_VMX_PROCBASED_CTLS  : constant := 16#482#;
   IA32_VMX_EXIT_CTLS       : constant := 16#483#;
   IA32_VMX_ENTRY_CTLS      : constant := 16#484#;
   IA32_VMX_CR0_FIXED0      : constant := 16#486#;
   IA32_VMX_CR0_FIXED1      : constant := 16#487#;
   IA32_VMX_CR4_FIXED0      : constant := 16#488#;
   IA32_VMX_CR4_FIXED1      : constant := 16#489#;

   ----------
   -- VMCS --
   ----------

   --  Control fields

   PIN_BASED_EXEC_CONTROL   : constant := 16#4000#;
   CPU_BASED_EXEC_CONTROL   : constant := 16#4002#;
   EXCEPTION_BITMAP         : constant := 16#4004#;
   VM_EXIT_CONTROLS         : constant := 16#400c#;
   VM_ENTRY_CONTROLS        : constant := 16#4012#;
   VMX_INST_ERROR           : constant := 16#4400#;
   VMX_EXIT_REASON          : constant := 16#4402#;
   VMX_EXIT_INTR_INFO       : constant := 16#4404#;
   VMX_EXIT_QUALIFICATION   : constant := 16#6400#;

   --  Host state fields

   HOST_SEL_ES              : constant := 16#0c00#;
   HOST_SEL_CS              : constant := 16#0c02#;
   HOST_SEL_SS              : constant := 16#0c04#;
   HOST_SEL_DS              : constant := 16#0c06#;
   HOST_SEL_TR              : constant := 16#0c0c#;
   HOST_CR0                 : constant := 16#6c00#;
   HOST_CR3                 : constant := 16#6c02#;
   HOST_CR4                 : constant := 16#6c04#;
   HOST_BASE_GDTR           : constant := 16#6c0c#;
   HOST_BASE_IDTR           : constant := 16#6c0e#;
   HOST_RSP                 : constant := 16#6c14#;
   HOST_RIP                 : constant := 16#6c16#;

   --  Guest state fields

   GUEST_SEL_ES             : constant := 16#0800#;
   GUEST_SEL_CS             : constant := 16#0802#;
   GUEST_SEL_SS             : constant := 16#0804#;
   GUEST_SEL_DS             : constant := 16#0806#;
   GUEST_SEL_TR             : constant := 16#080e#;
   VMCS_LINK_POINTER        : constant := 16#2800#;
   GUEST_LIMIT_ES           : constant := 16#4800#;
   GUEST_LIMIT_CS           : constant := 16#4802#;
   GUEST_LIMIT_SS           : constant := 16#4804#;
   GUEST_LIMIT_DS           : constant := 16#4806#;
   GUEST_LIMIT_TR           : constant := 16#480e#;
   GUEST_LIMIT_GDTR         : constant := 16#4810#;
   GUEST_LIMIT_IDTR         : constant := 16#4812#;
   GUEST_ACCESS_RIGHTS_ES   : constant := 16#4814#;
   GUEST_ACCESS_RIGHTS_CS   : constant := 16#4816#;
   GUEST_ACCESS_RIGHTS_SS   : constant := 16#4818#;
   GUEST_ACCESS_RIGHTS_DS   : constant := 16#481a#;
   GUEST_ACCESS_RIGHTS_FS   : constant := 16#481c#;
   GUEST_ACCESS_RIGHTS_GS   : constant := 16#481e#;
   GUEST_ACCESS_RIGHTS_LDTR : constant := 16#4820#;
   GUEST_ACCESS_RIGHTS_TR   : constant := 16#4822#;
   GUEST_VMX_PREEMPT_TIMER  : constant := 16#482e#;
   GUEST_CR0                : constant := 16#6800#;
   GUEST_CR3                : constant := 16#6802#;
   GUEST_CR4                : constant := 16#6804#;
   GUEST_BASE_GDTR          : constant := 16#6816#;
   GUEST_BASE_IDTR          : constant := 16#6818#;
   GUEST_RSP                : constant := 16#681c#;
   GUEST_RIP                : constant := 16#681e#;
   GUEST_RFLAGS             : constant := 16#6820#;

   --  VMX control flags

   VM_CONTROL_PREEMPT_TIMER : constant := 16#040#;
   VM_CONTROL_EXIT_HLT      : constant := 16#080#;
   VM_CONTROL_IA32E_MODE    : constant := 16#200#;

   --  VMX basic exit reasons

   VMEXIT_EXCEPTION_NMI     : constant := 0;
   VMEXIT_TIMER_EXPIRY      : constant := 52;

end SK.Constants;
