with SK;

package Dump is

   --  Interrupt handler.
   procedure Handle_Interrupt (Vector : SK.Byte);
   pragma Export (C, Handle_Interrupt, "dispatch_interrupt");

   --  Install and load the GDT as well as the IDT and enable interrupts.
   procedure Initialize;

end Dump;
