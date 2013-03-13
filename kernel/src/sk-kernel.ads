--# inherit
--#    X86_64,
--#    SK.Interrupts,
--#    SK.GDT,
--#    SK.Scheduler,
--#    SK.System_State,
--#    SK.Subjects,
--#    SK.VMX;
package SK.Kernel
is

   --  Kernel entry point.
   procedure Main;
   --# global
   --#    in out X86_64.State;
   --#    in out Subjects.Descriptors;
   --#    in     VMX.VMXON_Address;
   --#    in     VMX.VMX_Exit_Address;
   --#    in     VMX.Kernel_Stack_Address;
   --#    in     Interrupts.ISR_List;
   --#    in     GDT.GDT_Pointer;
   --#    in     Scheduler.State;
   --#       out Interrupts.IDT;
   --#       out Interrupts.IDT_Pointer;
   --# derives
   --#    Interrupts.IDT, Interrupts.IDT_Pointer from Interrupts.ISR_List &
   --#    Subjects.Descriptors from
   --#       *,
   --#       X86_64.State,
   --#       Scheduler.State &
   --#    X86_64.State from
   --#       *,
   --#       VMX.VMXON_Address,
   --#       VMX.VMX_Exit_Address,
   --#       VMX.Kernel_Stack_Address,
   --#       Interrupts.ISR_List,
   --#       GDT.GDT_Pointer,
   --#       Subjects.Descriptors,
   --#       Scheduler.State;
   pragma Export (C, Main, "kmain");

end SK.Kernel;
