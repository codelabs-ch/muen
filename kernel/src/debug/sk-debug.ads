package SK.Debug
is

   --  Dump register state to console.
   procedure Dump_Info;

   --  Convert given quadword to hex string and store it in specified buffer.
   procedure To_Hex
     (Item   :        Word64;
      Buffer : in out String);

   --  Return character representation of given quadword.
   function To_Character (Value : Word64) return Character;

end SK.Debug;
