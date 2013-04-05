with SK.KC;
with SK.Version;
with SK.Interrupts;
with SK.System_State;
with SK.VMX;
with SK.Scheduler;
with SK.Apic;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Main
   is
      Success : Boolean;
   begin
      pragma Debug (KC.Init);
      pragma Debug
        (KC.Put_Line (Item => "Booting Separation Kernel ("
                      & SK.Version.Version_String & ") ..."));

      --  Setup IDT.

      Interrupts.Init;
      Interrupts.Load;

      Success := System_State.Is_Valid;
      if Success then
         if Apic.Is_BSP then
            pragma Debug (KC.Put_Line
                          (Item => "BSP online, waking AP processors"));
            Apic.Enable;
            Apic.Start_AP_Processors;
         end if;

         VMX.Enable;
         Scheduler.Schedule;
      else
         pragma Debug (KC.Put_Line (Item => "System initialisation error"));
         null;
      end if;

      pragma Debug (KC.Put_Line (Item => "Terminating"));
   end Main;

end SK.Kernel;
