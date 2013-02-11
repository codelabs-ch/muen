--# inherit
--#    SK,
--#    SK.Interrupts;
package SK.Kernel
is

   --  Kernel entry point.
   procedure Main;
   --# global
   --#       out Interrupts.IDT;
   --#       out Interrupts.IDT_Pointer;
   --# derives
   --#    Interrupts.IDT, Interrupts.IDT_Pointer from ;
   pragma Export (C, Main, "kmain");

end SK.Kernel;
