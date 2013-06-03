with Skp;

with SK;

use type SK.Byte;

--# inherit
--#    SK,
--#    Skp;
package Handler
--# own
--#    Requesting_Subject : Skp.Subject_Id_Type;
--# initializes
--#    Requesting_Subject;
is

   --  Interrupt handler.
   procedure Handle_Interrupt (Vector : SK.Byte);
   --# global
   --#    out Requesting_Subject;
   --# derives
   --#    Requesting_Subject from Vector;
   pragma Export (C, Handle_Interrupt, "dispatch_interrupt");

   Requesting_Subject : Skp.Subject_Id_Type := Skp.Subject_Id_Type'First;
   pragma Atomic (Requesting_Subject);

end Handler;
