package SK.Utils
is

   --  Convert given quadword to hex string and store it in specified buffer.
   procedure To_Hex
     (Item   :        Word64;
      Buffer : in out String);

   subtype Word64_Hex_Str is String (1 .. 16);

   --  Convert given quadword to hex string.
   function To_Hex (Item : Word64) return Word64_Hex_Str;

end SK.Utils;
