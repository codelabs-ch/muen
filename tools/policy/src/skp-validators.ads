package Skp.Validators
is

   --  Validate given memory region.
   procedure Validate_Mem_Region (R : Memory_Region_Type);

   --  Validate memory layout.
   procedure Validate_Mem_Layout (L : Memory_Layout_Type);

   --  Validate device specification.
   procedure Validate_Device (D : Device_Type);

   --  Validate kernel specification.
   procedure Validate_Kernel (K : Kernel_Type);

   --  Validate subjects.
   procedure Validate_Subjects (P : Policy_Type);

   --  Validate given policy.
   procedure Validate_Policy (P : Policy_Type);

   Validation_Error : exception;

end Skp.Validators;
