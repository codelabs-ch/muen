with Skp;

--# inherit
--#    Skp.Kernel,
--#    Skp.Subjects,
--#    X86_64,
--#    SK.CPU,
--#    SK.CPU_Global,
--#    SK.Interrupts,
--#    SK.GDT,
--#    SK.Descriptors,
--#    SK.Constants,
--#    SK.Subjects,
--#    SK.Apic;
package SK.VMX
--# own
--#    State;
--# initializes
--#    State;
is

   --  VMX preemption timer value.
   subtype Time_Type is SK.Word32;

   --  Enter VMX root operation.
   procedure Enable;
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;

   --  Launch given subject.
   procedure Launch (Subject_Id : Skp.Subject_Id_Type);
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

   --  Resume given subject.
   procedure Resume (Subject_Id : Skp.Subject_Id_Type);
   --# global
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    Subjects.Descriptors from *, Subject_Id, X86_64.State &
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

   --  Write given value to the specified field of the current, active VMCS. If
   --  the operation fails, CPU.Panic is called.
   procedure VMCS_Write
     (Field : SK.Word16;
      Value : SK.Word64);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Field, Value;

end SK.VMX;
