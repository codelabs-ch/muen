--# inherit
--#    X86_64,
--#    SK.Interrupts,
--#    SK.GDT,
--#    SK.System_State,
--#    SK.VMX;
package SK.Kernel
is

   --  Kernel entry point.
   procedure Main;
   --# global
   --#    in out X86_64.State;
   --#    in     VMX.VMXON_Address;
   --#    in     VMX.VMCS_Address;
   --#    in     Interrupts.ISR_List;
   --#    in     GDT.GDT_Pointer;
   --#       out Interrupts.IDT;
   --#       out Interrupts.IDT_Pointer;
   --# derives
   --#    Interrupts.IDT, Interrupts.IDT_Pointer from Interrupts.ISR_List &
   --#    X86_64.State from
   --#       *,
   --#       VMX.VMXON_Address,
   --#       VMX.VMCS_Address,
   --#       Interrupts.ISR_List,
   --#       GDT.GDT_Pointer;
   pragma Export (C, Main, "kmain");

end SK.Kernel;
