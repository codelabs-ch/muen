with SK.Console;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Main
   is
   begin
      pragma Debug
        (SK.Console.Put_String (Item => "Booting Separation Kernel..."));
      null;
   end Main;

end SK.Kernel;
