with Subject.Text_IO;

package body Crypt.Debug
is

   -------------------------------------------------------------------------

   procedure Put_Message (Item : Crypt.Message_Type)
   is
   begin
      for I in Crypt.Data_Range range 1 .. Item.Size loop
         Subject.Text_IO.Put_Byte (Item => Item.Data (I));
      end loop;
   end Put_Message;

end Crypt.Debug;
