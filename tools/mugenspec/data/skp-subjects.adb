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
      0 => Subject_Spec_Type'(
       CPU_Id             => 0,
       PML4_Address       => 16#001f_0000#,
       EPT_Pointer        => 0,
       VMCS_Address       => 16#3000#,
       IO_Bitmap_Address  => 16#001a_0000#,
       MSR_Bitmap_Address => 16#0090_0000#,
       MSR_Store_Address  => 16#0090_1000#,
       Stack_Address      => 16#0012_0000#,
       Entry_Point        => 16#0abc#,
       CR0_Value          => 16#8001_0035#,
       CR0_Mask           => 16#e005_003f#,
       CR4_Value          => 16#2020#,
       CR4_Mask           => 16#0017_67ff#,
       CS_Access          => 16#a09b#,
       Exception_Bitmap   => 16#ffff_ffff#,
       MSR_Count          => 1,
       VMX_Controls       => VMX_Controls_Type'(
          Exec_Pin    => 73,
          Exec_Proc   => 2996411904,
          Exec_Proc2  => 64,
          Exit_Ctrls  => 4227584,
          Entry_Ctrls => 512),
       Trap_Table         => Trap_Table_Type'(
          00 => Trap_Entry_Type'(Dst_Subject => 2, Dst_Vector => Skp.Invalid_Vector),
          48 => Trap_Entry_Type'(Dst_Subject => 2, Dst_Vector => 12),
          others => Null_Trap),
       Event_Table        => Event_Table_Type'(
          17 => Event_Entry_Type'(
            Dst_Subject => 1,
            Dst_Vector  => 32,
            Handover    => False,
            Send_IPI    => True),
          18 => Event_Entry_Type'(
            Dst_Subject => 2,
            Dst_Vector  => Skp.Invalid_Vector,
            Handover    => True,
            Send_IPI    => False),
          others => Null_Event)),
      1 => Subject_Spec_Type'(
       CPU_Id             => 1,
       PML4_Address       => 0,
       EPT_Pointer        => 16#001f_401e#,
       VMCS_Address       => 16#4000#,
       IO_Bitmap_Address  => 16#001b_0000#,
       MSR_Bitmap_Address => 16#0091_0000#,
       MSR_Store_Address  => 16#0091_1000#,
       Stack_Address      => 16#0012_0000#,
       Entry_Point        => 16#0abc#,
       CR0_Value          => 16#0035#,
       CR0_Mask           => 16#0020#,
       CR4_Value          => 16#2020#,
       CR4_Mask           => 16#2000#,
       CS_Access          => 16#c09b#,
       Exception_Bitmap   => 16#fff0_8002#,
       MSR_Count          => 5,
       VMX_Controls       => VMX_Controls_Type'(
          Exec_Pin    => 73,
          Exec_Proc   => 2996313600,
          Exec_Proc2  => 194,
          Exit_Ctrls  => 7373312,
          Entry_Ctrls => 32768),
       Trap_Table         => Null_Trap_Table,
       Event_Table        => Null_Event_Table),
      2 => Subject_Spec_Type'(
       CPU_Id             => 0,
       PML4_Address       => 16#001f_8000#,
       EPT_Pointer        => 0,
       VMCS_Address       => 16#5000#,
       IO_Bitmap_Address  => 16#001c_0000#,
       MSR_Bitmap_Address => 16#0092_0000#,
       MSR_Store_Address  => 16#0000#,
       Stack_Address      => 16#0012_0000#,
       Entry_Point        => 16#0abc#,
       CR0_Value          => 16#8001_0035#,
       CR0_Mask           => 16#e005_003f#,
       CR4_Value          => 16#2020#,
       CR4_Mask           => 16#0017_67ff#,
       CS_Access          => 16#a09b#,
       Exception_Bitmap   => 16#ffff_ffff#,
       MSR_Count          => 0,
       VMX_Controls       => VMX_Controls_Type'(
          Exec_Pin    => 73,
          Exec_Proc   => 2996411904,
          Exec_Proc2  => 64,
          Exit_Ctrls  => 4227584,
          Entry_Ctrls => 512),
       Trap_Table         => Trap_Table_Type'(
          00 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          02 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          03 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          04 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          05 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          06 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          08 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          09 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          10 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          11 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          12 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          13 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          14 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          15 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          16 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          17 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          18 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          19 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          20 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          21 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          22 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          23 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          24 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          25 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          26 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          27 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          28 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          29 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          30 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          31 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          32 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          33 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          34 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          36 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          37 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          39 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          40 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          41 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          43 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          44 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          45 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          46 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          47 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          48 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          49 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          50 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          51 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          53 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          54 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          55 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          56 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          57 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          58 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          59 => Trap_Entry_Type'(Dst_Subject => 0, Dst_Vector => 32),
          others => Null_Trap),
       Event_Table        => Null_Event_Table));

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
