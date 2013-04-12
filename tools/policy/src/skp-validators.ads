package Skp.Validators
is

   --  Validate given memory region.
   procedure Validate_Mem_Region (R : Memory_Region_Type);

   --  Validate memory layout.
   procedure Validate (Memory_Layout : Memory_Layout_Type);

   --  Validate device specification.
   procedure Validate (Device : Device_Type);

   --  Validate kernel specification.
   procedure Validate (Kernel : Kernel_Type);

   --  Validate given policy.
   procedure Validate (Policy : Policy_Type);

   Validation_Error : exception;

end Skp.Validators;
