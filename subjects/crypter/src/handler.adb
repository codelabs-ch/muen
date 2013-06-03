with Crypt.Text_IO;

package body Handler
is

   -------------------------------------------------------------------------

   procedure Handle_Interrupt (Vector : SK.Byte)
   is
   begin
      if Vector >= SK.Byte (Skp.Subject_Id_Type'First) + 32
        and then Vector <= SK.Byte (Skp.Subject_Id_Type'Last) + 32
      then
         Requesting_Subject := Integer (Vector) - 32;
      else
         Requesting_Subject := Skp.Subject_Id_Type'First;
      end if;

      pragma Debug (Vector < 32,
                    Crypt.Text_IO.Put_String ("Ignoring spurious interrupt "));
      pragma Debug (Vector < 32, Crypt.Text_IO.Put_Byte (Item => Vector));
      pragma Debug (Vector < 32, Crypt.Text_IO.New_Line);
   end Handle_Interrupt;

end Handler;
