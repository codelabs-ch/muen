with System;

package body Crypt.Receiver
is

   Request : Message_Type;
   for Request'Address use System'To_Address (16#10000#);
   pragma Volatile (Request);

   -------------------------------------------------------------------------

   procedure Receive (Req : out Message_Type)
   is
   begin
      Req := Request;
   end Receive;

end Crypt.Receiver;
