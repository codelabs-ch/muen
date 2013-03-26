package Skp.Validators
is

   --  Validate given memory region, raises Validation_Error if it is not
   --- valid.
   procedure Validate (Region : Memory_Region_Type);

   Validation_Error : exception;

end Skp.Validators;
