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
--#    SK.IO;
package SK.Kernel
is

   --  Kernel entry point.
   procedure Main;
   --# global
   --#    in out X86_64.State;
   --#    in out Subjects.Descriptors;
   --#    in out MP.Barrier;
   --#    in     GDT.GDT_Pointer;
   --#    in     Scheduler.State;
   --#    in     VMX.State;
   --#    in     Interrupts.IDT;
   --#    in     Interrupts.IDT_Pointer;
   --# derives
   --#    MP.Barrier from
   --#       *,
   --#       X86_64.State,
   --#       Interrupts.IDT,
   --#       Interrupts.IDT_Pointer &
   --#    Subjects.Descriptors from
   --#       *,
   --#       X86_64.State,
   --#       Scheduler.State,
   --#       Interrupts.IDT,
   --#       Interrupts.IDT_Pointer &
   --#    X86_64.State from
   --#       *,
   --#       GDT.GDT_Pointer,
   --#       Subjects.Descriptors,
   --#       Scheduler.State,
   --#       VMX.State,
   --#       Interrupts.IDT,
   --#       Interrupts.IDT_Pointer;
   pragma Export (C, Main, "kmain");

end SK.Kernel;
