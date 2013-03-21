with SK.CPU;
with SK.Policy;

use type SK.Policy.Subject_Id_Type;

--# inherit
--#    X86_64,
--#    SK.Constants,
--#    SK.CPU,
--#    SK.GDT,
--#    SK.Interrupts,
--#    SK.Policy,
--#    SK.Subjects,
--#    SK.VMX;
package SK.Scheduler
--# own
--#       State,
--#    in Tau0_Kernel_Iface_Address;
--# initializes
--#    State;
is

   --  Schedule subject according to the current scheduling plan.
   procedure Schedule;
   --# global
   --#    in     VMX.State;
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     State;
   --#    in out X86_64.State;
   --#    in out Subjects.Descriptors;
   --# derives
   --#    Subjects.Descriptors from *, State &
   --#    X86_64.State         from
   --#       *,
   --#       VMX.State,
   --#       GDT.GDT_Pointer,
   --#       Interrupts.IDT_Pointer,
   --#       Subjects.Descriptors,
   --#       State;

private

   --  VMX exit handler.
   procedure Handle_Vmx_Exit (Subject_Registers : CPU.Registers_Type);
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     VMX.State;
   --#    in out State;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    State                from *                           &
   --#    Subjects.Descriptors from *, Subject_Registers, State &
   --#    X86_64.State         from
   --#       *,
   --#       Subject_Registers,
   --#       VMX.State,
   --#       State,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       Subjects.Descriptors;
   pragma Export (C, Handle_Vmx_Exit, "handle_vmx_exit");

end SK.Scheduler;
