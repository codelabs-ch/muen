package Skp.Validators
is

   --  Validate given memory region.
   procedure Validate (Region : Memory_Region_Type);

   --  Validate memory layout.
   procedure Validate (Memory_Layout : Memory_Layout_Type);

   --  Validate given policy.
   procedure Validate (Policy : Policy_Type);

   Validation_Error : exception;

end Skp.Validators;
