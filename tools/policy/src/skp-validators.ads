package Skp.Validators
is

   --  Validate given memory region.
   procedure Validate_Mem_Region (R : Memory_Region_Type);

   --  Validate memory layout.
   procedure Validate_Mem_Layout (L : Memory_Layout_Type);

   --  Validate MSR.
   procedure Validate_MSR (M : MSR_Type);

   --  Validate MSRs.
   procedure Validate_MSRs (M : MSRs_Type);

   --  Validate hardware specification.
   procedure Validate_Hardware (H : Hardware_Type);

   --  Validate kernel specification.
   procedure Validate_Kernel (K : Kernel_Type);

   --  Validate subjects.
   procedure Validate_Subjects (P : Policy_Type);

   --  Validate scheduling policy.
   procedure Validate_Scheduling (P : Policy_Type);

   --  Validate given policy.
   procedure Validate_Policy (P : Policy_Type);

   Validation_Error : exception;

end Skp.Validators;
