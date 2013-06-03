with LSC.SHA256;
with LSC.Types;

use type LSC.SHA256.Message_Index;
use type LSC.SHA256.SHA256_Hash_Index;
use type LSC.Types.Word32;

--# inherit
--#    Ada,
--#    SK,
--#    LSC.SHA256,
--#    LSC.Types,
--#    Crypt;
package Crypt.Hasher
is

   --  Calculate SHA256 hash of given input message and store in output.
   procedure SHA256_Hash
     (Input  :     Crypt.Message_Type;
      Output : out Crypt.Message_Type);

end Crypt.Hasher;
