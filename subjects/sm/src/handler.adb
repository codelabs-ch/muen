package body Handler
is

   -------------------------------------------------------------------------

   procedure Handle_Interrupt (Vector : SK.Byte)
   is
   begin
      Current_Subject := Integer (Vector) - 32;
   end Handle_Interrupt;

end Handler;
