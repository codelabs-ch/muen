with SK;

package Handler
is

   --  Interrupt handler.
   procedure Handle_Interrupt (Vector : SK.Byte);
   pragma Export (C, Handle_Interrupt, "dispatch_interrupt");

   --  Initialize interrupt handler.
   procedure Initialize;

end Handler;
