with SK.Console;
with SK.Version;
with SK.Debug;

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
      pragma Debug (SK.Console.New_Line);
      pragma Debug (SK.Debug.Dump_Info);
      null;
   end Main;

end SK.Kernel;
