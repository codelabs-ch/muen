package Crypt.Hasher
is

   --  Calculate SHA256 hash of given input message and store in output.
   procedure SHA256_Hash
     (Input  :     Message_Type;
      Output : out Message_Type);

end Crypt.Hasher;
