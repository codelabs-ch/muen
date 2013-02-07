with SK.Console;
with SK.Version;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Main
   is
   begin
      pragma Debug
        (SK.Console.Put_String
           (Item => "Booting Separation Kernel ("
            & SK.Version.Version_String & ") ..."));
      null;
   end Main;

end SK.Kernel;
