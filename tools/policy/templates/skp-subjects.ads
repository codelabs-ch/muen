with SK;

--# inherit SK, Skp;
package Skp.Subjects
is

   type Profile_Kind is (Native, Vm);

   type Trap_Entry_Type is record
      Dst_Subject : Skp.Dst_Subject_Type;
      Dst_Vector  : Skp.Dst_Vector_Range;
   end record;

   Null_Trap : constant Trap_Entry_Type := Trap_Entry_Type'
     (Dst_Subject => Skp.Invalid_Subject,
      Dst_Vector  => Skp.Invalid_Vector);

   type Trap_Range is range 0 .. 59;

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

   type VMX_Controls_Type is record
      Exec_Pin    : SK.Word32;
      Exec_Proc   : SK.Word32;
      Exec_Proc2  : SK.Word32;
      Exit_Ctrls  : SK.Word32;
      Entry_Ctrls : SK.Word32;
   end record;

   function Get_CPU_Id (Subject_Id : Skp.Subject_Id_Type) return Skp.CPU_Range;

   function Get_PML4_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_EPT_Pointer
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_VMCS_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_IO_Bitmap_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_MSR_Bitmap_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_Stack_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_Entry_Point
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_Trap
     (Subject_Id : Skp.Subject_Id_Type;
      Trap_Nr    : Trap_Range)
      return Trap_Entry_Type;

   function Get_Signal
     (Subject_Id : Skp.Subject_Id_Type;
      Signal_Nr  : Signal_Range)
      return Signal_Entry_Type;

   function Get_VMX_Controls
     (Subject_Id : Skp.Subject_Id_Type)
      return VMX_Controls_Type;

   function Get_CR0 (Subject_Id : Skp.Subject_Id_Type) return SK.Word64;

   function Get_CR0_Mask (Subject_Id : Skp.Subject_Id_Type) return SK.Word64;

   function Get_CR4 (Subject_Id : Skp.Subject_Id_Type) return SK.Word64;

   function Get_CR4_Mask (Subject_Id : Skp.Subject_Id_Type) return SK.Word64;

   function Get_CS_Access (Subject_Id : Skp.Subject_Id_Type) return SK.Word32;

   function Get_Exception_Bitmap
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word32;

   function Get_Profile
     (Subject_Id : Skp.Subject_Id_Type)
      return Profile_Kind;

end Skp.Subjects;
