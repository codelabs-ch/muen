--# inherit
--#    SK;
package SK.Policy
is

   type Subject_Id_Type is range 0 .. 2;

   type Subject_Spec_Type is record
      PML4_Address : SK.Word64;
   end record;

   type Subject_Spec_Array is array (Subject_Id_Type) of Subject_Spec_Type;

   Subject_Specs : constant Subject_Spec_Array := Subject_Spec_Array'
     (0 => Subject_Spec_Type'(PML4_Address => 16#1f0000#),
      1 => Subject_Spec_Type'(PML4_Address => 16#1f4000#),
      2 => Subject_Spec_Type'(PML4_Address => 16#1f8000#));

end SK.Policy;
