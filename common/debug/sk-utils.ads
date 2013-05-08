package SK.Utils
is

   --  Convert given quadword to hex string and store it in specified buffer.
   procedure To_Hex
     (Item   :        Word64;
      Buffer : in out String);

   subtype Word64_Hex_Str is String (1 .. 16);

   --  Convert given quadword to hex string.
   function To_Hex (Item : Word64) return Word64_Hex_Str;

   subtype Word16_Hex_Str is String (1 .. 4);

   --  Convert given word to hex string.
   function To_Hex (Item : Word16) return Word16_Hex_Str;

   subtype Word32_Hex_Str is String (1 .. 8);

   --  Convert given doubleword to hex string.
   function To_Hex (Item : Word32) return Word32_Hex_Str;

end SK.Utils;
