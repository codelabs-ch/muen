package SK.Constants
is

   ----------
   -- VMCS --
   ----------

   --  Control fields

   PIN_BASED_EXEC_CONTROL   : constant := 16#4000#;
   CPU_BASED_EXEC_CONTROL   : constant := 16#4002#;
   VM_EXIT_CONTROLS         : constant := 16#400c#;
   VM_ENTRY_CONTROLS        : constant := 16#4012#;
   VMX_INST_ERROR           : constant := 16#4400#;
   VMX_EXIT_REASON          : constant := 16#4402#;

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

   GUEST_SEL_CS             : constant := 16#0802#;
   GUEST_SEL_SS             : constant := 16#0804#;
   GUEST_SEL_DS             : constant := 16#0806#;
   GUEST_SEL_TR             : constant := 16#080e#;
   VMCS_LINK_POINTER        : constant := 16#2800#;
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
   GUEST_CR0                : constant := 16#6800#;
   GUEST_CR3                : constant := 16#6802#;
   GUEST_CR4                : constant := 16#6804#;
   GUEST_BASE_GDTR          : constant := 16#6816#;
   GUEST_BASE_IDTR          : constant := 16#6818#;
   GUEST_RSP                : constant := 16#681c#;
   GUEST_RIP                : constant := 16#681e#;
   GUEST_RFLAGS             : constant := 16#6820#;

   --  VMX control flags

   VM_CONTROL_EXIT_HLT      : constant := 16#080#;
   VM_CONTROL_IA32E_MODE    : constant := 16#200#;

end SK.Constants;
