with SK;

--# inherit SK, Skp;
package Skp.Subjects is

   type Subject_Spec_Type is record
      PML4_Address      : SK.Word64;
      VMCS_Address      : SK.Word64;
      IO_Bitmap_Address : SK.Word64;
      Stack_Address     : SK.Word64;
      Entry_Point       : SK.Word64;
   end record;

   type Subject_Spec_Array is array (Skp.Subject_Id_Type) of Subject_Spec_Type;

   Subject_Specs : constant Subject_Spec_Array := Subject_Spec_Array'(
__subjects__);

end Skp.Subjects;
