--  Disable line length check
pragma Style_Checks ("-m");

package body Skp.Subjects
is

   type Subject_Spec_Type is record
      CPU_ID             : CPU_Range;
      PML4_Address       : SK.Word64;
      EPT_Pointer        : SK.Word64;
      VMCS_Address       : SK.Word64;
      IO_Bitmap_Address  : SK.Word64;
      MSR_Bitmap_Address : SK.Word64;
      MSR_Store_Address  : SK.Word64;
      Stack_Address      : SK.Word64;
      Entry_Point        : SK.Word64;
      GPRs               : SK.CPU_Registers_Type;
      Segment_Regs       : SK.Segment_Registers_Type;
      CR0_Value          : SK.Word64;
      CR0_Mask           : SK.Word64;
      CR4_Value          : SK.Word64;
      CR4_Mask           : SK.Word64;
      Exception_Bitmap   : SK.Word32;
      MSR_Count          : SK.Word32;
      VMX_Controls       : VMX_Controls_Type;
   end record;

   type Subject_Spec_Array is array (Global_Subject_ID_Type) of
     Subject_Spec_Type;

   Subject_Specs : constant Subject_Spec_Array := Subject_Spec_Array'(
      0 => Subject_Spec_Type'(
       CPU_ID             => 0,
       PML4_Address       => 16#001f_0000#,
       EPT_Pointer        => 0,
       VMCS_Address       => 16#3000#,
       IO_Bitmap_Address  => 16#001a_0000#,
       MSR_Bitmap_Address => 16#0090_0000#,
       MSR_Store_Address  => 16#0090_1000#,
       Stack_Address      => 16#0012_0000#,
       Entry_Point        => 16#0abc#,
       GPRs               => SK.CPU_Registers_Type'(
          CR2 => 16#0000#,
          RAX => 16#0000#,
          RBX => 16#0000#,
          RCX => 16#0000#,
          RDX => 16#0000#,
          RDI => 16#0000#,
          RSI => 16#0000#,
          RBP => 16#0000#,
          R08 => 16#0000#,
          R09 => 16#0000#,
          R10 => 16#0000#,
          R11 => 16#0000#,
          R12 => 16#0000#,
          R13 => 16#0000#,
          R14 => 16#0000#,
          R15 => 16#0000#),
       Segment_Regs       => SK.Segment_Registers_Type'(
          CS =>
            (Selector      => 16#0008#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#a09b#),
          DS =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#c093#),
          ES =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff_ffff#,
             Access_Rights => 16#c093#),
          FS =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#),
          GS =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#),
          SS =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff_ffff#,
             Access_Rights => 16#c093#),
          TR =>
            (Selector      => 16#0018#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#008b#),
          LDTR =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#)),
       CR0_Value          => 16#8001_0035#,
       CR0_Mask           => 16#ffff_ffff_ffff_ffff#,
       CR4_Value          => 16#2020#,
       CR4_Mask           => 16#ffff_ffff_ffff_ffff#,
       Exception_Bitmap   => 16#ffff_ffff#,
       MSR_Count          => 1,
       VMX_Controls       => VMX_Controls_Type'(
          Exec_Pin    => 73,
          Exec_Proc   => 2996411904,
          Exec_Proc2  => 64,
          Exit_Ctrls  => 4227584,
          Entry_Ctrls => 512)),
      1 => Subject_Spec_Type'(
       CPU_ID             => 1,
       PML4_Address       => 0,
       EPT_Pointer        => 16#001f_401e#,
       VMCS_Address       => 16#4000#,
       IO_Bitmap_Address  => 16#001b_0000#,
       MSR_Bitmap_Address => 16#0091_0000#,
       MSR_Store_Address  => 16#0091_1000#,
       Stack_Address      => 16#0012_0000#,
       Entry_Point        => 16#0abc#,
       GPRs               => SK.CPU_Registers_Type'(
          CR2 => 16#0000#,
          RAX => 16#0001#,
          RBX => 16#00ff#,
          RCX => 16#0002#,
          RDX => 16#00fe#,
          RDI => 16#0003#,
          RSI => 16#00f0#,
          RBP => 16#9000#,
          R08 => 16#0f0f#,
          R09 => 16#ff00#,
          R10 => 16#cafe#,
          R11 => 16#fefe#,
          R12 => 16#efef#,
          R13 => 16#c9c9#,
          R14 => 16#a0b0#,
          R15 => 16#0000#),
       Segment_Regs       => SK.Segment_Registers_Type'(
          CS =>
            (Selector      => 16#0008#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#c09b#),
          DS =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#c093#),
          ES =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff_ffff#,
             Access_Rights => 16#c093#),
          FS =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#),
          GS =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#),
          SS =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff_ffff#,
             Access_Rights => 16#c093#),
          TR =>
            (Selector      => 16#0018#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#008b#),
          LDTR =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#)),
       CR0_Value          => 16#0035#,
       CR0_Mask           => 16#ffff_ffff_1ffa_ffe0#,
       CR4_Value          => 16#2020#,
       CR4_Mask           => 16#ffff_ffff_ff88_b000#,
       Exception_Bitmap   => 16#fff0_8006#,
       MSR_Count          => 5,
       VMX_Controls       => VMX_Controls_Type'(
          Exec_Pin    => 73,
          Exec_Proc   => 2996313600,
          Exec_Proc2  => 194,
          Exit_Ctrls  => 7373312,
          Entry_Ctrls => 32768)),
      2 => Subject_Spec_Type'(
       CPU_ID             => 0,
       PML4_Address       => 16#001f_8000#,
       EPT_Pointer        => 0,
       VMCS_Address       => 16#5000#,
       IO_Bitmap_Address  => 16#001c_0000#,
       MSR_Bitmap_Address => 16#0092_0000#,
       MSR_Store_Address  => 16#0000#,
       Stack_Address      => 16#0012_0000#,
       Entry_Point        => 16#0abc#,
       GPRs               => SK.CPU_Registers_Type'(
          CR2 => 16#0000#,
          RAX => 16#0000#,
          RBX => 16#0000#,
          RCX => 16#0000#,
          RDX => 16#0000#,
          RDI => 16#0000#,
          RSI => 16#0000#,
          RBP => 16#0000#,
          R08 => 16#0000#,
          R09 => 16#0000#,
          R10 => 16#0000#,
          R11 => 16#0000#,
          R12 => 16#0000#,
          R13 => 16#0000#,
          R14 => 16#0000#,
          R15 => 16#0000#),
       Segment_Regs       => SK.Segment_Registers_Type'(
          CS =>
            (Selector      => 16#0008#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#a09b#),
          DS =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#c093#),
          ES =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff_ffff#,
             Access_Rights => 16#c093#),
          FS =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#),
          GS =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#),
          SS =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff_ffff#,
             Access_Rights => 16#c093#),
          TR =>
            (Selector      => 16#0018#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#008b#),
          LDTR =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#)),
       CR0_Value          => 16#8001_0035#,
       CR0_Mask           => 16#ffff_ffff_ffff_ffff#,
       CR4_Value          => 16#2020#,
       CR4_Mask           => 16#ffff_ffff_ffff_ffff#,
       Exception_Bitmap   => 16#ffff_ffff#,
       MSR_Count          => 0,
       VMX_Controls       => VMX_Controls_Type'(
          Exec_Pin    => 73,
          Exec_Proc   => 2996411904,
          Exec_Proc2  => 64,
          Exit_Ctrls  => 4227584,
          Entry_Ctrls => 512)),
      3 => Subject_Spec_Type'(
       CPU_ID             => 0,
       PML4_Address       => 16#001f_8000#,
       EPT_Pointer        => 0,
       VMCS_Address       => 16#6000#,
       IO_Bitmap_Address  => 16#001c_0000#,
       MSR_Bitmap_Address => 16#0092_0000#,
       MSR_Store_Address  => 16#0000#,
       Stack_Address      => 16#0012_0000#,
       Entry_Point        => 16#0abc#,
       GPRs               => SK.CPU_Registers_Type'(
          CR2 => 16#0000#,
          RAX => 16#0001#,
          RBX => 16#0002#,
          RCX => 16#0003#,
          RDX => 16#0004#,
          RDI => 16#0005#,
          RSI => 16#0006#,
          RBP => 16#0007#,
          R08 => 16#0008#,
          R09 => 16#0009#,
          R10 => 16#000a#,
          R11 => 16#000b#,
          R12 => 16#000c#,
          R13 => 16#000d#,
          R14 => 16#000e#,
          R15 => 16#000f#),
       Segment_Regs       => SK.Segment_Registers_Type'(
          CS =>
            (Selector      => 16#0008#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#a09b#),
          DS =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#c093#),
          ES =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff_ffff#,
             Access_Rights => 16#c093#),
          FS =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#),
          GS =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#),
          SS =>
            (Selector      => 16#0010#,
             Base          => 16#0000#,
             Limit         => 16#ffff_ffff#,
             Access_Rights => 16#c093#),
          TR =>
            (Selector      => 16#0018#,
             Base          => 16#0000#,
             Limit         => 16#ffff#,
             Access_Rights => 16#008b#),
          LDTR =>
            (Selector      => 16#0000#,
             Base          => 16#0000#,
             Limit         => 16#0000#,
             Access_Rights => 16#0001_0000#)),
       CR0_Value          => 16#8001_0035#,
       CR0_Mask           => 16#ffff_ffff_ffff_ffff#,
       CR4_Value          => 16#2020#,
       CR4_Mask           => 16#ffff_ffff_ffff_ffff#,
       Exception_Bitmap   => 16#ffff_ffff#,
       MSR_Count          => 0,
       VMX_Controls       => VMX_Controls_Type'(
          Exec_Pin    => 73,
          Exec_Proc   => 2996411904,
          Exec_Proc2  => 64,
          Exit_Ctrls  => 4227584,
          Entry_Ctrls => 512)));

   -------------------------------------------------------------------------

   function Get_CPU_ID (Subject_ID : Global_Subject_ID_Type) return CPU_Range
   is (Subject_Specs (Subject_ID).CPU_ID);

   -------------------------------------------------------------------------

   function Get_CR0 (Subject_ID : Global_Subject_ID_Type) return SK.Word64
   is (Subject_Specs (Subject_ID).CR0_Value);

   -------------------------------------------------------------------------

   function Get_CR0_Mask (Subject_ID : Global_Subject_ID_Type) return SK.Word64
   is (Subject_Specs (Subject_ID).CR0_Mask);

   -------------------------------------------------------------------------

   function Get_CR4 (Subject_ID : Global_Subject_ID_Type) return SK.Word64
   is (Subject_Specs (Subject_ID).CR4_Value);

   -------------------------------------------------------------------------

   function Get_CR4_Mask (Subject_ID : Global_Subject_ID_Type) return SK.Word64
   is (Subject_Specs (Subject_ID).CR4_Mask);

   -------------------------------------------------------------------------

   function Get_Entry_Point
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Word64
   is (Subject_Specs (Subject_ID).Entry_Point);

   -------------------------------------------------------------------------

   function Get_EPT_Pointer
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Word64
   is (Subject_Specs (Subject_ID).EPT_Pointer);

   -------------------------------------------------------------------------

   function Get_Exception_Bitmap
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Word32
   is (Subject_Specs (Subject_ID).Exception_Bitmap);

   -------------------------------------------------------------------------

   function Get_GPRs
     (Subject_ID : Global_Subject_ID_Type)
      return SK.CPU_Registers_Type
   is (Subject_Specs (Subject_ID).GPRs);

   -------------------------------------------------------------------------

   function Get_IO_Bitmap_Address
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Word64
   is (Subject_Specs (Subject_ID).IO_Bitmap_Address);

   -------------------------------------------------------------------------

   function Get_MSR_Bitmap_Address
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Word64
   is (Subject_Specs (Subject_ID).MSR_Bitmap_Address);

   -------------------------------------------------------------------------

   function Get_MSR_Count
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Word32
   is (Subject_Specs (Subject_ID).MSR_Count);

   -------------------------------------------------------------------------

   function Get_MSR_Store_Address
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Word64
   is (Subject_Specs (Subject_ID).MSR_Store_Address);

   -------------------------------------------------------------------------

   function Get_PML4_Address
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Word64
   is (Subject_Specs (Subject_ID).PML4_Address);

   -------------------------------------------------------------------------

   function Get_Segment_Registers
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Segment_Registers_Type
   is (Subject_Specs (Subject_ID).Segment_Regs);

   -------------------------------------------------------------------------

   function Get_Stack_Address
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Word64
   is (Subject_Specs (Subject_ID).Stack_Address);

   -------------------------------------------------------------------------

   function Get_VMCS_Address
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Word64
   is (Subject_Specs (Subject_ID).VMCS_Address);

   -------------------------------------------------------------------------

   function Get_VMX_Controls
     (Subject_ID : Global_Subject_ID_Type)
      return VMX_Controls_Type
   is (Subject_Specs (Subject_ID).VMX_Controls);

end Skp.Subjects;
