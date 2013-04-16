package SubjC
is

   --  Interrupt handler.
   procedure Handle_Interrupt;
   pragma Export (C, Handle_Interrupt, "dispatch_interrupt");

   --  Install ISR, load IDT and initialize VGA console.
   procedure Initialize;

end SubjC;
