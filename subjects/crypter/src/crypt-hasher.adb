with Ada.Unchecked_Conversion;

with LSC.SHA256;
with LSC.Types;

package body Crypt.Hasher
is

   use type LSC.SHA256.Message_Index;

   subtype SHA256_Message_Range is LSC.SHA256.Message_Index range
     0 .. LSC.SHA256.Message_Index (Data_Range'Last) / 64 - 1;
   subtype SHA256_Message_Type is LSC.SHA256.Message_Type
     (SHA256_Message_Range);

   --  Convert crypter message data to SHA256 message.
   function To_SHA256_Msg is new Ada.Unchecked_Conversion
     (Source => Data_Array,
      Target => SHA256_Message_Type);

   --  Convert SHA256 hash type to data array.
   procedure To_Data
     (Input  :     LSC.SHA256.SHA256_Hash_Type;
      Output : out Data_Array);

   -------------------------------------------------------------------------

   procedure SHA256_Hash
     (Input  :     Message_Type;
      Output : out Message_Type)
   is
   begin
      Output.Size := 32;

      To_Data (Input  => LSC.SHA256.Hash
               (Message => To_SHA256_Msg (S => Input.Data),
                Length  => LSC.Types.Word64 (Input.Size * 8)),
               Output => Output.Data);
   end SHA256_Hash;

   -------------------------------------------------------------------------

   procedure To_Data
     (Input  :     LSC.SHA256.SHA256_Hash_Type;
      Output : out Data_Array)
   is
      use type LSC.SHA256.SHA256_Hash_Index;
      use type LSC.Types.Word32;
   begin
      Output := Null_Data;

      for I in LSC.SHA256.SHA256_Hash_Index loop
         Output (Data_Range (I) * 4 + 1) := SK.Byte'Mod (Input (I));
         Output (Data_Range (I) * 4 + 2) := SK.Byte'Mod
           (LSC.Types.SHR32 (Value  => Input (I),
                             Amount => 8));
         Output (Data_Range (I) * 4 + 3) := SK.Byte'Mod
           (LSC.Types.SHR32 (Value  => Input (I),
                             Amount => 16));
         Output (Data_Range (I) * 4 + 4) := SK.Byte'Mod
           (LSC.Types.SHR32 (Value  => Input (I),
                             Amount => 24));
      end loop;
   end To_Data;

end Crypt.Hasher;
