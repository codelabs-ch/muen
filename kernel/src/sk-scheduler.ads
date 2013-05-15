with Skp.Scheduling;
with Skp.Subjects;

use type Skp.CPU_Range;
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
   --#    in     Interrupts.IDT_Pointer;
   --#    in     GDT.GDT_Pointer;
   --#    in     VMX.State;
   --#    in out X86_64.State;
   --#    in out CPU_Global.Storage;
   --# derives
   --#    CPU_Global.Storage from *, State, X86_64.State &
   --#    X86_64.State from
   --#       *,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       VMX.State;

   --  Schedule subject according to the current scheduling plan.
   procedure Schedule;
   --# global
   --#    in     CPU_Global.Storage;
   --#    in out X86_64.State;
   --#    in out Subjects.Descriptors;
   --# derives
   --#    Subjects.Descriptors, X86_64.State from
   --#       Subjects.Descriptors,
   --#       CPU_Global.Storage,
   --#       X86_64.State;

private

   --  VMX exit handler.
   procedure Handle_Vmx_Exit (Subject_Registers : SK.CPU_Registers_Type);
   --# global
   --#    in out CPU_Global.Storage;
   --#    in out State;
   --#    in out MP.Barrier;
   --#    in out Subjects.Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    State, MP.Barrier, CPU_Global.Storage, Subjects.Descriptors,
   --#    X86_64.State from
   --#       *,
   --#       State,
   --#       Subject_Registers,
   --#       Subjects.Descriptors,
   --#       CPU_Global.Storage,
   --#       X86_64.State;
   pragma Export (C, Handle_Vmx_Exit, "handle_vmx_exit");

end SK.Scheduler;
