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
      PML4_Address       : SK.Word64;
      VMCS_Address       : SK.Word64;
      IO_Bitmap_Address  : SK.Word64;
      MSR_Bitmap_Address : SK.Word64;
      VAPIC_Address      : SK.Word64;
      Stack_Address      : SK.Word64;
      Entry_Point        : SK.Word64;
      Trap_Table         : Trap_Table_Type;
      Signal_Table       : Signal_Table_Type;
   end record;

   type Subject_Spec_Array is array (Skp.Subject_Id_Type) of Subject_Spec_Type;

   Subject_Specs : constant Subject_Spec_Array := Subject_Spec_Array'(
__subjects__);

end Skp.Subjects;
