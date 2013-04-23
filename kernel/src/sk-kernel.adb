with SK.KC;
with SK.Version;
with SK.Interrupts;
with SK.System_State;
with SK.VMX;
with SK.Scheduler;
with SK.Apic;
with SK.MP;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Main
   is
      Success, Is_Bsp : Boolean;
   begin
      Interrupts.Load;

      Success := System_State.Is_Valid;
      if Success then

         Apic.Enable;

         Is_Bsp := Apic.Is_BSP;
         if Is_Bsp then

            --  BSP

            pragma Debug (KC.Init);
            pragma Debug (KC.Put_Line
                          (Item => "Booting Separation Kernel ("
                           & SK.Version.Version_String & ") ..."));

            Interrupts.Disable_Legacy_PIC;
            Interrupts.Setup_IRQ_Routing;

            pragma Debug (KC.Put_Line (Item => "Starting AP processors"));
            Apic.Start_AP_Processors;
         end if;

         --  Synchronize all logical CPUs

         MP.Wait_For_All;

         if Is_Bsp then
            MP.Reset_Barrier;
         end if;

         --  BSP & APs

         VMX.Enable;
         Scheduler.Schedule;
      else
         pragma Debug (KC.Put_Line (Item => "System initialisation error"));
         null;
      end if;
      pragma Debug (KC.Put_Line (Item => "Terminating"));
   end Main;

end SK.Kernel;
