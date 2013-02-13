package body SK
is

   -------------------------------------------------------------------------

   function Bit_Set
     (Value : Word64;
      Pos   : Word64_Pos)
      return Word64
   is
   begin
      return (Value or 2 ** Natural (Pos));
   end Bit_Set;

   -------------------------------------------------------------------------

   function Bit_Test
     (Value : Word64;
      Pos   : Word64_Pos)
      return Boolean
   is
      Mask : Word64;
   begin
      Mask := 2 ** Natural (Pos);
      return ((Value and Mask) /= 0);
   end Bit_Test;

end SK;
