with SK.KC;
with SK.Version;
with SK.Interrupts;
with SK.System_State;
with SK.VMX;
with SK.Scheduler;
with SK.Apic;
with SK.MP;
with SK.CPU_Global;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Main
   is
      Success, Is_Bsp : Boolean;
   begin
      Is_Bsp := Apic.Is_BSP;

      if Is_Bsp then
         Interrupts.Init;
      end if;
      Interrupts.Load;

      pragma Debug (Is_Bsp, KC.Init);
      pragma Debug (Is_Bsp, KC.Put_Line
                    (Item => "Booting Separation Kernel "
                     & SK.Version.Version_String & " ("
                     & Standard'Compiler_Version & ")"));
      Success := System_State.Is_Valid;
      if Success then

         Apic.Enable;
         CPU_Global.Init;

         if Is_Bsp then

            --  BSP

            Interrupts.Disable_Legacy_PIC;
            Interrupts.Setup_IRQ_Routing;
            Apic.Start_AP_Processors;
         end if;

         VMX.Enable;
         Scheduler.Init;

         --  Synchronize all logical CPUs.

         MP.Wait_For_All;
         VMX.Run (Subject_Id => CPU_Global.Get_Current_Minor_Frame.Subject_Id);
      end if;

      pragma Debug (not Success, KC.Put_Line
                    (Item => "System initialisation error"));
      pragma Debug (KC.Put_Line (Item => "Terminating"));
   end Main;

end SK.Kernel;
