--# inherit
--#    Crypt;
package Crypt.Sender
--# own
--#    out State;
is

   --  Copies the given response message into the crypter response page.
   procedure Send (Res : Crypt.Message_Type);
   --# global
   --#    out State;
   --# derives
   --#    State from Res;

end Crypt.Sender;
