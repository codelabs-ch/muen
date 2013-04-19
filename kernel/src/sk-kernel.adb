with SK.KC;
with SK.Version;
with SK.Interrupts;
with SK.System_State;
with SK.VMX;
with SK.Scheduler;
with SK.Apic;
with SK.MP;
with SK.IO_Apic;

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

            IO_Apic.Route_IRQ (IRQ            => 1,
                               Vector         => 32,
                               Trigger_Mode   => IO_Apic.Edge,
                               Destination_Id => Apic.Get_ID);

            pragma Debug (KC.Put_Line (Item => "Starting AP processors"));
            Apic.Start_AP_Processors;
         end if;

         --  Synchronize all logical CPUs

         MP.Wait_For_All;

         if Apic.Is_BSP then
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
