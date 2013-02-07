package SK.Kernel
is

   --  Kernel entry point.
   procedure Main;
   --# derives ;
   pragma Export (C, Main, "kmain");

end SK.Kernel;
