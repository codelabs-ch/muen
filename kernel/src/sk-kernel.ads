--# inherit
--#    SK,
--#    SK.Interrupts;
package SK.Kernel
is

   --  Kernel entry point.
   procedure Main;
   --# global
   --#    in     Interrupts.ISR_List;
   --#       out Interrupts.IDT;
   --#       out Interrupts.IDT_Pointer;
   --# derives
   --#    Interrupts.IDT, Interrupts.IDT_Pointer from Interrupts.ISR_List;
   pragma Export (C, Main, "kmain");

end SK.Kernel;
