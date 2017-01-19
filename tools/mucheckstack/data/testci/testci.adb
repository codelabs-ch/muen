with Foo;
with Log;

procedure Testci
is
   Result : Boolean;
begin
   Foo.Init;
   Log.Str ("Initialized");
   Foo.Run (Parameter => 42,
            Result    => Result);
   Log.Bool (Result);
end Testci;
