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
--#    Kernel_Stack_Address;
--# initializes
--#    VMXON_Address,
--#    VMX_Exit_Address,
--#    Kernel_Stack_Address;
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

end SK.VMX;
