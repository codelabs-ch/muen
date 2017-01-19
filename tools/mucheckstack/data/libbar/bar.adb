with Log;

package body Bar
is


   -------------------------------------------------------------------------

   function Sub (X, Y : Natural) return Natural
   is
   begin
      return X - Y;
   end Sub;

   -------------------------------------------------------------------------

   procedure Swap_Sub
     (X, Y   :     Natural;
      Result : out Natural)
   is
      Tmp1, Tmp2 : Natural;

      procedure Nested_Sub (Res : out Natural)
      is
      begin
         Res := Sub (Tmp2, Tmp1);
      end Nested_Sub;
   begin
      Tmp1 := X;
      Tmp2 := Y;
      Nested_Sub (Result);
   end Swap_Sub;


   -------------------------------------------------------------------------

   procedure Intricate_Calculation
     (X, Y   :     Natural;
      Result : out Natural)
   is
   begin
      Swap_Sub (X, Y , Result);
      Log.Bool (Result = 45);
   end Intricate_Calculation;

end Bar;
