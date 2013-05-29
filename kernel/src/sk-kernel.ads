--# inherit
--#    X86_64,
--#    SK.Apic,
--#    SK.Interrupts,
--#    SK.GDT,
--#    SK.Scheduler,
--#    SK.System_State,
--#    SK.Subjects,
--#    SK.VMX,
--#    SK.MP,
--#    SK.IO_Apic,
--#    SK.CPU_Global,
--#    SK.Locks;
package SK.Kernel
is

   --  Kernel entry point.
   procedure Main;
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Scheduler.State;
   --#    in     VMX.State;
   --#    in out Interrupts.State;
   --#    in out Subjects.State;
   --#    in out MP.Barrier;
   --#    in out IO_Apic.State;
   --#    in out CPU_Global.State;
   --#    in out Locks.State;
   --#    in out X86_64.State;
   --# derives
   --#    Interrupts.State from *, X86_64.State &
   --#    MP.Barrier, IO_Apic.State from
   --#       *,
   --#       X86_64.State,
   --#       Interrupts.State &
   --#    CPU_Global.State from
   --#       *,
   --#       X86_64.State,
   --#       Scheduler.State,
   --#       Interrupts.State &
   --#    Subjects.State, Locks.State, X86_64.State from
   --#       *,
   --#       GDT.GDT_Pointer,
   --#       Subjects.State,
   --#       Scheduler.State,
   --#       VMX.State,
   --#       Interrupts.State,
   --#       X86_64.State;
   pragma Export (C, Main, "kmain");

end SK.Kernel;
