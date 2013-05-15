with SK;

--# inherit SK, Skp;
package Skp.Subjects is

   type Trap_Entry_Type is record
      Dst_Subject : Skp.Dst_Subject_Type;
      Dst_Vector  : Skp.Dst_Vector_Range;
   end record;

   Null_Trap : constant Trap_Entry_Type := Trap_Entry_Type'
     (Dst_Subject => Skp.Invalid_Subject,
      Dst_Vector  => Skp.Invalid_Vector);

   type Trap_Range is range 0 .. 59;

   type Trap_Table_Type is array (Trap_Range) of Trap_Entry_Type;

   Null_Trap_Table : constant Trap_Table_Type := Trap_Table_Type'
     (others => Null_Trap);

   type Signal_Kind is
     (Asynchronous,
      Synchronous,
      Handover);

   type Signal_Entry_Type is record
      Kind        : Signal_Kind;
      Dst_Subject : Skp.Dst_Subject_Type;
      Dst_Vector  : Skp.Dst_Vector_Range;
   end record;

   Null_Signal : constant Signal_Entry_Type := Signal_Entry_Type'
     (Kind        => Asynchronous,
      Dst_Subject => Skp.Invalid_Subject,
      Dst_Vector  => Skp.Invalid_Vector);

   type Signal_Range is range 0 .. 31;

   type Signal_Table_Type is array (Signal_Range) of Signal_Entry_Type;

   Null_Signal_Table : constant Signal_Table_Type := Signal_Table_Type'
     (others => Null_Signal);

   type Subject_Spec_Type is record
      CPU_Id             : Skp.CPU_Range;
      PML4_Address       : SK.Word64;
      VMCS_Address       : SK.Word64;
      IO_Bitmap_Address  : SK.Word64;
      MSR_Bitmap_Address : SK.Word64;
      Stack_Address      : SK.Word64;
      Entry_Point        : SK.Word64;
      Trap_Table         : Trap_Table_Type;
      Signal_Table       : Signal_Table_Type;
   end record;

   type Subject_Spec_Array is array (Skp.Subject_Id_Type) of Subject_Spec_Type;

   Subject_Specs : constant Subject_Spec_Array := Subject_Spec_Array'(
__subjects__);

   function Get_CPU_Id (Subject_Id : Skp.Subject_Id_Type) return Skp.CPU_Range;
   --# return
   --#    Subject_Specs (Subject_Id).CPU_Id;

   function Get_PML4_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;
   --# return
   --#    Subject_Specs (Subject_Id).PML4_Address;

   function Get_VMCS_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;
   --# return
   --#    Subject_Specs (Subject_Id).VMCS_Address;

   function Get_IO_Bitmap_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;
   --# return
   --#    Subject_Specs (Subject_Id).IO_Bitmap_Address;

   function Get_MSR_Bitmap_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;
   --# return
   --#    Subject_Specs (Subject_Id).MSR_Bitmap_Address;

   function Get_Trap
     (Subject_Id : Skp.Subject_Id_Type;
      Trap_Nr    : Trap_Range)
      return Trap_Entry_Type;
   --# return
   --#    Subject_Specs (Subject_Id).Trap_Table (Trap_Nr);

end Skp.Subjects;
