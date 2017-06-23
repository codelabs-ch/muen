--  Disable line length check
pragma Style_Checks ("-m");

package body Skp.Subjects
is

   type Subject_Spec_Type is record
      CPU_Id             : CPU_Range;
      PML4_Address       : SK.Word64;
      EPT_Pointer        : SK.Word64;
      VMCS_Address       : SK.Word64;
      IO_Bitmap_Address  : SK.Word64;
      MSR_Bitmap_Address : SK.Word64;
      MSR_Store_Address  : SK.Word64;
      Stack_Address      : SK.Word64;
      Entry_Point        : SK.Word64;
      CR0_Value          : SK.Word64;
      CR0_Mask           : SK.Word64;
      CR4_Value          : SK.Word64;
      CR4_Mask           : SK.Word64;
      CS_Access          : SK.Word32;
      Exception_Bitmap   : SK.Word32;
      MSR_Count          : SK.Word32;
      VMX_Controls       : VMX_Controls_Type;
   end record;

   type Subject_Spec_Array is array (Subject_Id_Type) of Subject_Spec_Type;

   Subject_Specs : constant Subject_Spec_Array := Subject_Spec_Array'(
__subjects__);

   -------------------------------------------------------------------------

   function Get_CPU_Id (Subject_Id : Subject_Id_Type) return CPU_Range
   is (Subject_Specs (Subject_Id).CPU_Id);

   -------------------------------------------------------------------------

   function Get_CR0 (Subject_Id : Subject_Id_Type) return SK.Word64
   is (Subject_Specs (Subject_Id).CR0_Value);

   -------------------------------------------------------------------------

   function Get_CR0_Mask (Subject_Id : Subject_Id_Type) return SK.Word64
   is (Subject_Specs (Subject_Id).CR0_Mask);

   -------------------------------------------------------------------------

   function Get_CR4 (Subject_Id : Subject_Id_Type) return SK.Word64
   is (Subject_Specs (Subject_Id).CR4_Value);

   -------------------------------------------------------------------------

   function Get_CR4_Mask (Subject_Id : Subject_Id_Type) return SK.Word64
   is (Subject_Specs (Subject_Id).CR4_Mask);

   -------------------------------------------------------------------------

   function Get_CS_Access (Subject_Id : Subject_Id_Type) return SK.Word32
   is (Subject_Specs (Subject_Id).CS_Access);

   -------------------------------------------------------------------------

   function Get_Entry_Point (Subject_Id : Subject_Id_Type) return SK.Word64
   is (Subject_Specs (Subject_Id).Entry_Point);

   -------------------------------------------------------------------------

   function Get_EPT_Pointer (Subject_Id : Subject_Id_Type) return SK.Word64
   is (Subject_Specs (Subject_Id).EPT_Pointer);

   -------------------------------------------------------------------------

   function Get_Exception_Bitmap
     (Subject_Id : Subject_Id_Type)
      return SK.Word32
   is (Subject_Specs (Subject_Id).Exception_Bitmap);

   -------------------------------------------------------------------------

   function Get_IO_Bitmap_Address
     (Subject_Id : Subject_Id_Type)
      return SK.Word64
   is (Subject_Specs (Subject_Id).IO_Bitmap_Address);

   -------------------------------------------------------------------------

   function Get_MSR_Bitmap_Address
     (Subject_Id : Subject_Id_Type)
      return SK.Word64
   is (Subject_Specs (Subject_Id).MSR_Bitmap_Address);

   -------------------------------------------------------------------------

   function Get_MSR_Count (Subject_Id : Subject_Id_Type) return SK.Word32
   is (Subject_Specs (Subject_Id).MSR_Count);

   -------------------------------------------------------------------------

   function Get_MSR_Store_Address
     (Subject_Id : Subject_Id_Type)
      return SK.Word64
   is (Subject_Specs (Subject_Id).MSR_Store_Address);

   -------------------------------------------------------------------------

   function Get_PML4_Address (Subject_Id : Subject_Id_Type) return SK.Word64
   is (Subject_Specs (Subject_Id).PML4_Address);

   -------------------------------------------------------------------------

   function Get_Stack_Address (Subject_Id : Subject_Id_Type) return SK.Word64
   is (Subject_Specs (Subject_Id).Stack_Address);

   -------------------------------------------------------------------------

   function Get_VMCS_Address (Subject_Id : Subject_Id_Type) return SK.Word64
   is (Subject_Specs (Subject_Id).VMCS_Address);

   -------------------------------------------------------------------------

   function Get_VMX_Controls
     (Subject_Id : Subject_Id_Type)
      return VMX_Controls_Type
   is (Subject_Specs (Subject_Id).VMX_Controls);

end Skp.Subjects;
