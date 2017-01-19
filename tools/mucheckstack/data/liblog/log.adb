package body Log
is

   -------------------------------------------------------------------------

   procedure Bool (Val : Boolean)
   is
   begin
      if Val then
         Str (Msg => "True");
      else
         Str (Msg => "False");
      end if;
   end Bool;

   -------------------------------------------------------------------------

   procedure Char (C : Character)
   is
   begin
      null;
   end Char;

   -------------------------------------------------------------------------

   procedure Str (Msg : String)
   is
   begin
      for C of Msg loop
         Char (C => C);
      end loop;
   end Str;

end Log;
