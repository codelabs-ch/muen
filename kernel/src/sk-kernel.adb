with System.Machine_Code;

with SK.Console;
with SK.Version;
with SK.Debug;
with SK.Interrupts;
with SK.System_State;
with SK.VMX;
with SK.CPU;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Main
   is
      --# hide Main;
   begin
      pragma Debug
        (SK.Console.Put_Line
           (Item => "Booting Separation Kernel ("
            & SK.Version.Version_String & ") ..."));

      --  Setup IDT.

      Interrupts.Init;
      Interrupts.Load;

      if System_State.Is_Valid then

         --  Enable VMX operation.

         VMX.Enable;
      else
         pragma Debug
           (SK.Console.Put_Line
              (Item => "System initialisation error"));
         null;
      end if;

      pragma Debug
        (SK.Console.Put_Line
           (Item => "Terminating"));
      pragma Debug (System.Machine_Code.Asm
        (Template => "ud2",
         Volatile => True));
   end Main;

end SK.Kernel;
