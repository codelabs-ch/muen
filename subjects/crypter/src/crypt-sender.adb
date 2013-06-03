with System;

package body Crypt.Sender
--# own
--#    State is out Response;
is

   Response : Crypt.Message_Type;
   for Response'Address use System'To_Address (16#20000#);
   pragma Volatile (Response);

   -------------------------------------------------------------------------

   procedure Send (Res : Crypt.Message_Type)
   --# global
   --#    out Response;
   --# derives
   --#    Response from Res;
   is
   begin
      Response := Res;
   end Send;

end Crypt.Sender;
