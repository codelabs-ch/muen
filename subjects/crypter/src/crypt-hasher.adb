with Ada.Unchecked_Conversion;

package body Crypt.Hasher
is

   subtype SHA256_Message_Range is LSC.SHA256.Message_Index range
     0 .. LSC.SHA256.Message_Index (Crypt.Data_Range'Last) / 64 - 1;
   subtype SHA256_Message_Type is LSC.SHA256.Message_Type
     (SHA256_Message_Range);

   --  Convert crypter message data to SHA256 message.
   function To_SHA256_Msg is new Ada.Unchecked_Conversion
     (Source => Crypt.Data_Array,
      Target => SHA256_Message_Type);

   -------------------------------------------------------------------------

   procedure SHA256_Hash
     (Input  :     Crypt.Message_Type;
      Output : out Crypt.Message_Type)
   is
      LSC_Hash : LSC.SHA256.SHA256_Hash_Type;
   begin
      --# accept Warning, 13, To_SHA256_Msg, "Message to SHA256 conversion";
      LSC_Hash := LSC.SHA256.Hash
        (Message => To_SHA256_Msg (S => Input.Data),
         Length  => LSC.Types.Word64 (Input.Size * 8));
      --# end accept;

      Output      := Crypt.Null_Message;
      Output.Size := 32;
      for I in LSC.SHA256.SHA256_Hash_Index loop

         --# accept Warning, 444, "LSC_Hash is an array of LSC.Types.Word32.";
         --# assume LSC_Hash (I) in LSC.Types.Word32;
         --# end accept;

         Output.Data (Crypt.Data_Range (I * 4 + 1)) := SK.Byte'Mod
           (LSC_Hash (I));
         Output.Data (Crypt.Data_Range (I * 4 + 2)) := SK.Byte'Mod
           (LSC.Types.SHR32 (Value  => LSC_Hash (I),
                             Amount => 8));
         Output.Data (Crypt.Data_Range (I * 4 + 3)) := SK.Byte'Mod
           (LSC.Types.SHR32 (Value  => LSC_Hash (I),
                             Amount => 16));
         Output.Data (Crypt.Data_Range (I * 4 + 4)) := SK.Byte'Mod
           (LSC.Types.SHR32 (Value  => LSC_Hash (I),
                             Amount => 24));
      end loop;
   end SHA256_Hash;

end Crypt.Hasher;
