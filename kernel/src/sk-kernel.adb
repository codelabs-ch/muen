with System.Machine_Code;

with SK.Console;
with SK.Version;
with SK.Debug;
with SK.Interrupts;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Main
   is
      --# hide Main;
   begin
      pragma Debug
        (SK.Console.Put_String
           (Item => "Booting Separation Kernel ("
            & SK.Version.Version_String & ") ..."));
      pragma Debug (SK.Console.New_Line);

      --  Setup IDT.

      SK.Interrupts.Init;
      SK.Interrupts.Load;

      System.Machine_Code.Asm
        (Template => "ud2",
         Volatile => True);
   end Main;

end SK.Kernel;
