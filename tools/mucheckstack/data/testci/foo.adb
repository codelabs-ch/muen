with Bar;
with Log;

package body Foo
is

   Data : Natural;

   -------------------------------------------------------------------------

   procedure Largest_Stack_Usage
   is
      Result : Natural;
   begin
      Bar.Intricate_Calculation (X      => 23,
                                 Y      => 52,
                                 Result => Result);
   end Largest_Stack_Usage;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Data := 0;
   end Init;

   -------------------------------------------------------------------------

   procedure Run
     (Parameter :     Natural;
      Result    : out Boolean)
   is
      Is_Eq : Boolean;
      X     : Natural := 4;
      Y     : Natural := 57;
   begin
      X      := X + Parameter;
      Is_Eq  := Data = X;
      Data   := X + Y;
      Result := not Is_Eq;
   end Run;

end Foo;
