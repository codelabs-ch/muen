with System;

package body Crypt.Receiver
--# own State is in Request;
is

   Request : Crypt.Message_Type;
   for Request'Address use System'To_Address (16#10000#);
   pragma Volatile (Request);
   --# assert Request'Always_Valid;

   -------------------------------------------------------------------------

   procedure Receive (Req : out Crypt.Message_Type)
   --# global
   --#    in Request;
   --# derives
   --#    Req from Request;
   is
   begin
      Req := Request;
   end Receive;

end Crypt.Receiver;
