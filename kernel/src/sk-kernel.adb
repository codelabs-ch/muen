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
      Is_Bsp := Apic.Is_BSP;

      pragma Debug (Is_Bsp, KC.Init);
      pragma Debug (Is_Bsp, KC.Put_Line
                    (Item => "Booting Separation Kernel ("
                     & SK.Version.Version_String & ") ..."));

      Success := System_State.Is_Valid;
      if Success then

         Apic.Enable;

         if Is_Bsp then

            --  BSP

            Interrupts.Disable_Legacy_PIC;
            Interrupts.Setup_IRQ_Routing;
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
      end if;

      pragma Debug (not Success, KC.Put_Line
                    (Item => "System initialisation error"));
      pragma Debug (KC.Put_Line (Item => "Terminating"));
   end Main;

end SK.Kernel;
