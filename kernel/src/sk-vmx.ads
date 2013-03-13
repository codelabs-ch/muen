with SK.Subjects;

use type SK.Subjects.Index_Type;

--# inherit
--#    X86_64,
--#    SK.CPU,
--#    SK.Interrupts,
--#    SK.GDT,
--#    SK.Descriptors,
--#    SK.Constants,
--#    SK.Subjects;
package SK.VMX
--# own
--#    VMXON_Address,
--#    VMX_Exit_Address,
--#    Kernel_Stack_Address,
--#    Current_Subject;
--# initializes
--#    VMXON_Address,
--#    VMX_Exit_Address,
--#    Kernel_Stack_Address,
--#    Current_Subject;
is

   procedure Enable;
   --# global
   --#    in     VMXON_Address;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, VMXON_Address;

   procedure Launch (Subject_Id : Subjects.Index_Type);
   --# global
   --#    in     VMX_Exit_Address;
   --#    in     Kernel_Stack_Address;
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    Subjects.Descriptors from *, Subject_Id &
   --#    X86_64.State from
   --#       *,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       VMX_Exit_Address,
   --#       Kernel_Stack_Address,
   --#       Subjects.Descriptors,
   --#       Subject_Id;

   procedure Resume (Subject_Id : Subjects.Index_Type);
   --# global
   --#    in     Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Subject_Id, Subjects.Descriptors;

   --  Read value from specified field of the current, active VMCS. If the
   --  operation fails, CPU.Panic is called.
   procedure VMCS_Read
     (Field :     SK.Word16;
      Value : out SK.Word64);
   --# global
   --#    X86_64.State;
   --# derives
   --#    Value, X86_64.State from X86_64.State, Field;

private

   --  VMX exit handler.
   procedure Handle_Vmx_Exit
     (RDI : SK.Word64; RSI : SK.Word64; RDX : SK.Word64; RCX : SK.Word64;
      R08 : SK.Word64; R09 : SK.Word64; RAX : SK.Word64; RBX : SK.Word64;
      RBP : SK.Word64; R10 : SK.Word64; R11 : SK.Word64; R12 : SK.Word64;
      R13 : SK.Word64; R14 : SK.Word64; R15 : SK.Word64);
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     VMX_Exit_Address;
   --#    in     Kernel_Stack_Address;
   --#    in out Current_Subject;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    Current_Subject from * &
   --#    X86_64.State    from
   --#       *,
   --#       RAX,
   --#       RBX,
   --#       RCX,
   --#       RDX,
   --#       RDI,
   --#       RSI,
   --#       RBP,
   --#       R08,
   --#       R09,
   --#       R10,
   --#       R11,
   --#       R12,
   --#       R13,
   --#       R14,
   --#       R15,
   --#       VMX_Exit_Address,
   --#       Kernel_Stack_Address,
   --#       Current_Subject,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       Subjects.Descriptors &
   --#    Subjects.Descriptors from
   --#       *,
   --#       RAX,
   --#       RBX,
   --#       RCX,
   --#       RDX,
   --#       RDI,
   --#       RSI,
   --#       RBP,
   --#       R08,
   --#       R09,
   --#       R10,
   --#       R11,
   --#       R12,
   --#       R13,
   --#       R14,
   --#       R15,
   --#       Current_Subject;
   pragma Export (C, Handle_Vmx_Exit, "handle_vmx_exit");

end SK.VMX;
