with SK;

with Skp;

package Handler
is

   --  Interrupt handler.
   procedure Handle_Interrupt (Vector : SK.Byte);
   pragma Export (C, Handle_Interrupt, "dispatch_interrupt");

   Current_Subject : Skp.Subject_Id_Type;
   pragma Atomic (Current_Subject);

end Handler;
