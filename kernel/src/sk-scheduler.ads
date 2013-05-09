with Skp.Scheduling;
with Skp.Subjects;

use type Skp.Dst_Vector_Range;
use type Skp.Scheduling.Minor_Frame_Range;
use type Skp.Subjects.Trap_Entry_Type;
use type Skp.Subjects.Signal_Kind;
use type Skp.Subjects.Signal_Entry_Type;

--# inherit
--#    Skp.Subjects,
--#    Skp.Scheduling,
--#    Skp.Interrupts,
--#    X86_64,
--#    SK.Constants,
--#    SK.CPU,
--#    SK.CPU_Global,
--#    SK.GDT,
--#    SK.Interrupts,
--#    SK.Subjects,
--#    SK.VMX,
--#    SK.Apic,
--#    SK.MP;
package SK.Scheduler
--# own
--#       State,
--#    in Tau0_Kernel_Iface_Address;
--# initializes
--#    State;
is

   --  Init scheduler.
   procedure Init;
   --# global
   --#    in     State;
   --#    in out X86_64.State;
   --#    in out CPU_Global.Storage;
   --# derives
   --#    X86_64.State       from * &
   --#    CPU_Global.Storage from *, State, X86_64.State;

   --  Schedule subject according to the current scheduling plan.
   procedure Schedule;
   --# global
   --#    in     VMX.State;
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     State;
   --#    in     CPU_Global.Storage;
   --#    in out X86_64.State;
   --#    in out Subjects.Descriptors;
   --# derives
   --#    Subjects.Descriptors from
   --#       *,
   --#       State,
   --#       CPU_Global.Storage,
   --#       X86_64.State &
   --#    X86_64.State from
   --#       *,
   --#       VMX.State,
   --#       GDT.GDT_Pointer,
   --#       Interrupts.IDT_Pointer,
   --#       Subjects.Descriptors,
   --#       CPU_Global.Storage,
   --#       State;

private

   --  VMX exit handler.
   procedure Handle_Vmx_Exit (Subject_Registers : SK.CPU_Registers_Type);
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     VMX.State;
   --#    in out CPU_Global.Storage;
   --#    in out State;
   --#    in out MP.Barrier;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    State, MP.Barrier, CPU_Global.Storage from
   --#       *,
   --#       State,
   --#       Subject_Registers,
   --#       Subjects.Descriptors,
   --#       CPU_Global.Storage,
   --#       X86_64.State &
   --#    Subjects.Descriptors from
   --#       *,
   --#       Subject_Registers,
   --#       State,
   --#       CPU_Global.Storage,
   --#       X86_64.State &
   --#    X86_64.State from
   --#       *,
   --#       Subject_Registers,
   --#       VMX.State,
   --#       State,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       Subjects.Descriptors,
   --#       CPU_Global.Storage;
   pragma Export (C, Handle_Vmx_Exit, "handle_vmx_exit");

end SK.Scheduler;
