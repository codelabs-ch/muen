package Skp.Test
is

   --  Return first memory region of memory layout.
   function First (Layout : Memory_Layout_Type) return Memory_Region_Type;

   --  Return first subject description in system policy.
   function First (Policy : Policy_Type) return Subject_Type;

end Skp.Test;
