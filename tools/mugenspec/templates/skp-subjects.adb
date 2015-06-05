--  Disable line length check
pragma Style_Checks ("-m");

package body Skp.Subjects
is

   type Trap_Table_Type is array (Trap_Range) of Trap_Entry_Type;

   Null_Trap_Table : constant Trap_Table_Type := Trap_Table_Type'
     (others => Null_Trap);

   type Event_Table_Type is array (Event_Range) of Event_Entry_Type;

   Null_Event_Table : constant Event_Table_Type := Event_Table_Type'
     (others => Null_Event);

   type Subject_Spec_Type is record
      CPU_Id             : Skp.CPU_Range;
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
      Trap_Table         : Trap_Table_Type;
      Event_Table        : Event_Table_Type;
   end record;

   type Subject_Spec_Array is array (Skp.Subject_Id_Type) of Subject_Spec_Type;

   Subject_Specs : constant Subject_Spec_Array := Subject_Spec_Array'(
__subjects__);

   -------------------------------------------------------------------------

   function Get_CPU_Id (Subject_Id : Skp.Subject_Id_Type) return Skp.CPU_Range
   is
   begin
      return Subject_Specs (Subject_Id).CPU_Id;
   end Get_CPU_Id;

   -------------------------------------------------------------------------

   function Get_CR0 (Subject_Id : Skp.Subject_Id_Type) return SK.Word64
   is
   begin
      return Subject_Specs (Subject_Id).CR0_Value;
   end Get_CR0;

   -------------------------------------------------------------------------

   function Get_CR0_Mask (Subject_Id : Skp.Subject_Id_Type) return SK.Word64
   is
   begin
      return Subject_Specs (Subject_Id).CR0_Mask;
   end Get_CR0_Mask;

   -------------------------------------------------------------------------

   function Get_CR4 (Subject_Id : Skp.Subject_Id_Type) return SK.Word64
   is
   begin
      return Subject_Specs (Subject_Id).CR4_Value;
   end Get_CR4;

   -------------------------------------------------------------------------

   function Get_CR4_Mask (Subject_Id : Skp.Subject_Id_Type) return SK.Word64
   is
   begin
      return Subject_Specs (Subject_Id).CR4_Mask;
   end Get_CR4_Mask;

   -------------------------------------------------------------------------

   function Get_CS_Access (Subject_Id : Skp.Subject_Id_Type) return SK.Word32
   is
   begin
      return Subject_Specs (Subject_Id).CS_Access;
   end Get_CS_Access;

   -------------------------------------------------------------------------

   function Get_Entry_Point
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      return Subject_Specs (Subject_Id).Entry_Point;
   end Get_Entry_Point;

   -------------------------------------------------------------------------

   function Get_EPT_Pointer
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      return Subject_Specs (Subject_Id).EPT_Pointer;
   end Get_EPT_Pointer;

   -------------------------------------------------------------------------

   function Get_Event
     (Subject_Id : Skp.Subject_Id_Type;
      Event_Nr   : Event_Range)
      return Event_Entry_Type
   is
   begin
      --  XXX Help the prover for now [N226-016]
      pragma Assume
       (for all Subject in Skp.Subject_Id_Type =>
          (for all Event in Event_Range =>
            (Subject_Specs (Subject).Event_Table (Event).Dst_Subject
             /= Subject)));
      return Subject_Specs (Subject_Id).Event_Table (Event_Nr);
   end Get_Event;

   -------------------------------------------------------------------------

   function Get_Exception_Bitmap
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word32
   is
   begin
      return Subject_Specs (Subject_Id).Exception_Bitmap;
   end Get_Exception_Bitmap;

   -------------------------------------------------------------------------

   function Get_IO_Bitmap_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      return Subject_Specs (Subject_Id).IO_Bitmap_Address;
   end Get_IO_Bitmap_Address;

   -------------------------------------------------------------------------

   function Get_MSR_Bitmap_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      return Subject_Specs (Subject_Id).MSR_Bitmap_Address;
   end Get_MSR_Bitmap_Address;

   -------------------------------------------------------------------------

   function Get_MSR_Count (Subject_Id : Skp.Subject_Id_Type) return SK.Word32
   is
   begin
      return Subject_Specs (Subject_Id).MSR_Count;
   end Get_MSR_Count;

   -------------------------------------------------------------------------

   function Get_MSR_Store_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      return Subject_Specs (Subject_Id).MSR_Store_Address;
   end Get_MSR_Store_Address;

   -------------------------------------------------------------------------

   function Get_PML4_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      return Subject_Specs (Subject_Id).PML4_Address;
   end Get_PML4_Address;

   -------------------------------------------------------------------------

   function Get_Stack_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      return Subject_Specs (Subject_Id).Stack_Address;
   end Get_Stack_Address;

   -------------------------------------------------------------------------

   function Get_Trap
     (Subject_Id : Skp.Subject_Id_Type;
      Trap_Nr    : Trap_Range)
      return Trap_Entry_Type
   is
   begin
      --  XXX Help the prover for now [N226-016]
      pragma Assume
       (for all Subject in Skp.Subject_Id_Type =>
          (for all Trap in Trap_Range =>
            (Subject_Specs (Subject).Trap_Table (Trap).Dst_Subject
             /= Subject)));
      return Subject_Specs (Subject_Id).Trap_Table (Trap_Nr);
   end Get_Trap;

   -------------------------------------------------------------------------

   function Get_VMCS_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
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
