with Skp;

with SK;

package Handler
is

   --  Interrupt handler.
   procedure Handle_Interrupt (Vector : SK.Byte);
   pragma Export (C, Handle_Interrupt, "dispatch_interrupt");

   Requesting_Subject : Skp.Subject_Id_Type;
   pragma Atomic (Requesting_Subject);

end Handler;
