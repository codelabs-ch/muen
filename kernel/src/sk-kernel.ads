package SK.Kernel
is

   procedure Main;
   --  Kernel entry point.
   pragma Export (C, Main, "kmain");

end SK.Kernel;
