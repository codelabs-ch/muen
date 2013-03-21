with SK.Policy;

use type SK.Policy.Subject_Id_Type;

--# inherit
--#    X86_64,
--#    SK.CPU,
--#    SK.Interrupts,
--#    SK.GDT,
--#    SK.Descriptors,
--#    SK.Constants,
--#    SK.Policy,
--#    SK.Subjects;
package SK.VMX
--# own
--#    State;
--# initializes
--#    State;
is

   procedure Enable;
   --# global
   --#    in     State;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, State;

   procedure Launch (Subject_Id : Policy.Subject_Id_Type);
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     State;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    Subjects.Descriptors from *, Subject_Id &
   --#    X86_64.State from
   --#       *,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       Subjects.Descriptors,
   --#       Subject_Id,
   --#       State;

   procedure Resume (Subject_Id : Policy.Subject_Id_Type);
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
