package body Skp.Subjects
is

   type Trap_Table_Type is array (Trap_Range) of Trap_Entry_Type;

   Null_Trap_Table : constant Trap_Table_Type := Trap_Table_Type'
     (others => Null_Trap);

   type Signal_Table_Type is array (Signal_Range) of Signal_Entry_Type;

   Null_Signal_Table : constant Signal_Table_Type := Signal_Table_Type'
     (others => Null_Signal);

   type Subject_Spec_Type is record
      CPU_Id             : Skp.CPU_Range;
      PML4_Address       : SK.Word64;
      EPT_Pointer        : SK.Word64;
      VMCS_Address       : SK.Word64;
      IO_Bitmap_Address  : SK.Word64;
      MSR_Bitmap_Address : SK.Word64;
      Stack_Address      : SK.Word64;
      Entry_Point        : SK.Word64;
      CR0_Value          : SK.Word64;
      CR0_Mask           : SK.Word64;
      CR4_Value          : SK.Word64;
      CS_Access          : SK.Word32;
      VMX_Controls       : VMX_Controls_Type;
      Trap_Table         : Trap_Table_Type;
      Signal_Table       : Signal_Table_Type;
   end record;

   type Subject_Spec_Array is array (Skp.Subject_Id_Type) of Subject_Spec_Type;

   Subject_Specs : constant Subject_Spec_Array := Subject_Spec_Array'(
__subjects__);

   -------------------------------------------------------------------------

   function Get_CPU_Id (Subject_Id : Skp.Subject_Id_Type) return Skp.CPU_Range
   is
   begin
      --# accept Warning, 444, "CPU_Id is in CPU_Range (obviously)";
      --# assume Subject_Specs (Subject_Id).CPU_Id in Skp.CPU_Range;
      --# end accept;
      return Subject_Specs (Subject_Id).CPU_Id;
   end Get_CPU_Id;

   -------------------------------------------------------------------------

   function Get_CR0 (Subject_Id : Skp.Subject_Id_Type) return SK.Word64
   is
   begin
      --# accept Warning, 444, "CR0_Value is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).CR0_Value in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).CR0_Value;
   end Get_CR0;

   -------------------------------------------------------------------------

   function Get_CR0_Mask (Subject_Id : Skp.Subject_Id_Type) return SK.Word64
   is
   begin
      --# accept Warning, 444, "CR0_Mask is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).CR0_Mask in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).CR0_Mask;
   end Get_CR0_Mask;

   -------------------------------------------------------------------------

   function Get_CR4 (Subject_Id : Skp.Subject_Id_Type) return SK.Word64
   is
   begin
      --# accept Warning, 444, "CR4_Value is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).CR4_Value in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).CR4_Value;
   end Get_CR4;

   -------------------------------------------------------------------------

   function Get_CS_Access (Subject_Id : Skp.Subject_Id_Type) return SK.Word32
   is
   begin
      --# accept Warning, 444, "CS_Access is Word32 (obviously)";
      --# assume Subject_Specs (Subject_Id).CS_Access in SK.Word32;
      --# end accept;
      return Subject_Specs (Subject_Id).CS_Access;
   end Get_CS_Access;

   -------------------------------------------------------------------------

   function Get_Entry_Point
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      --# accept Warning, 444, "Entry_Point is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).Entry_Point in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).Entry_Point;
   end Get_Entry_Point;

   -------------------------------------------------------------------------

   function Get_EPT_Pointer
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      --# accept Warning, 444, "EPT_Pointer is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).EPT_Pointer in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).EPT_Pointer;
   end Get_EPT_Pointer;

   -------------------------------------------------------------------------

   function Get_IO_Bitmap_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      --# accept Warning, 444, "IO_Bitmap_Address is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).IO_Bitmap_Address in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).IO_Bitmap_Address;
   end Get_IO_Bitmap_Address;

   -------------------------------------------------------------------------

   function Get_MSR_Bitmap_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      --# accept Warning, 444, "MSR_Bitmap_Address is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).MSR_Bitmap_Address in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).MSR_Bitmap_Address;
   end Get_MSR_Bitmap_Address;

   -------------------------------------------------------------------------

   function Get_PML4_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      --# accept Warning, 444, "PML4_Address is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).PML4_Address in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).PML4_Address;
   end Get_PML4_Address;

   -------------------------------------------------------------------------

   function Get_Signal
     (Subject_Id : Skp.Subject_Id_Type;
      Signal_Nr  : Signal_Range)
      return Signal_Entry_Type
   is
   begin
      return Subject_Specs (Subject_Id).Signal_Table (Signal_Nr);
   end Get_Signal;

   -------------------------------------------------------------------------

   function Get_Stack_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      --# accept Warning, 444, "Stack_Address is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).Stack_Address in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).Stack_Address;
   end Get_Stack_Address;

   -------------------------------------------------------------------------

   function Get_Trap
     (Subject_Id : Skp.Subject_Id_Type;
      Trap_Nr    : Trap_Range)
      return Trap_Entry_Type
   is
   begin
      return Subject_Specs (Subject_Id).Trap_Table (Trap_Nr);
   end Get_Trap;

   -------------------------------------------------------------------------

   function Get_VMCS_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      --# accept Warning, 444, "VMCS_Address is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).VMCS_Address in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).VMCS_Address;
   end Get_VMCS_Address;

   -------------------------------------------------------------------------

   function Get_VMX_Controls
     (Subject_Id : Skp.Subject_Id_Type)
      return VMX_Controls_Type
   is
   begin
      return Subject_Specs (Subject_Id).VMX_Controls;
   end Get_VMX_Controls;

end Skp.Subjects;
