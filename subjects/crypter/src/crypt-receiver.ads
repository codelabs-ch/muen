--# inherit
--#    Crypt;
package Crypt.Receiver
--# own
--#    in State;
is

   --  Fills the current crypter request into the given req parameter.
   procedure Receive (Req : out Crypt.Message_Type);
   --# global
   --#    in State;
   --# derives
   --#    Req from State;

end Crypt.Receiver;
