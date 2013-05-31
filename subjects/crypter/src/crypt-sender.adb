with System;

package body Crypt.Sender
is

   Response : Message_Type;
   for Response'Address use System'To_Address (16#20000#);
   pragma Volatile (Response);

   -------------------------------------------------------------------------

   procedure Send (Res : Message_Type)
   is
   begin
      Response := Null_Message;
      Response := Res;
   end Send;

end Crypt.Sender;
